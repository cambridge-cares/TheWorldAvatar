package uk.ac.cam.cares.jps.agent.trajectoryqueryagent;

import com.cmclinnovations.stack.clients.ontop.OntopClient;
import org.apache.jena.graph.Node;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import java.sql.Connection;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.time.OffsetDateTime;
import org.apache.jena.graph.NodeFactory;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.sql.SQLException;
import java.sql.Statement;


@WebServlet(urlPatterns = "/createlayer")
public class TrajectoryQueryAgent extends JPSAgent {
    private KGQueryClient kgQueryClient;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;
    private final String USER_ID = "userID";
    private static final Logger LOGGER = LogManager.getLogger(TrajectoryQueryAgent.class);
    private String userID; 


    public void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        kgQueryClient = new KGQueryClient(storeClient);
        remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
    }

    /**
     * 1) Receive userID
     * 2) SPARQL query for pointIRI based on userID 
     * 3) Create geoserver layer
     * 4) Return pointIRI to application as response
     * @param requestParams
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        if (!validateInput(requestParams)) {
            throw new JPSRuntimeException("Unable to validate request sent to the agent.");
        }

        //Retrieve params
        this.userID = requestParams.getString(USER_ID);

        //SPARQL query for pointIRI based on userID 
        //Node smartphoneIRI = NodeFactory.createURI(kgQueryClient.getIRIfromJSONarray(kgQueryClient.getSmartPhoneIRI(deviceID)));
        //String pointIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getPointIRIArray(smartphoneIRI));

        //Create Geoserver layer
        createGeoserver();

        //Return pointIRI to app as response
        JSONObject response = new JSONObject();
        response.put("message", "PointIRI is XXXX.");
        return response;
    }


    private void createGeoserver(){

         String getGeometryTableFunction = "CREATE OR REPLACE FUNCTION public.get_geometry_table(\"table\" TEXT, \"columnName\" TEXT)\n" +
                "RETURNS TABLE (\"time\" timestamptz, \"value\" geometry) AS\n" +
                "$$\n" +
                "BEGIN\n" +
                "RETURN QUERY EXECUTE 'SELECT \"time\", \"' || \"columnName\" || '\" AS value FROM \"' || \"table\" || '\"';\n" +
                "END;\n" +
                "$$ LANGUAGE plpgsql;";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, getGeometryTableFunction);
            System.out.println("Created get_geometry_table function.");
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        //Create geoserver layer
        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        String workspaceName= "twa";
        String schema = "public";
        String dbName="postgres";
        geoServerClient.createWorkspace(workspaceName);
        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        virtualTable.setSql("SELECT timeseries.time AS time,\n" +
                "                    timeseries.value AS geom\n" +
                "                FROM \"dbTable\",\n" +
                "                    public.get_geometry_table(\"tableName\", \"columnName\") AS timeseries,\n" +
                "                    information_schema.columns c\n" +
                "                WHERE \"dbTable\".\"dataIRI\" = '%pointiri%'\n" +
                "                    AND c.table_schema = 'public'\n" +
                "                    AND c.table_name = \"tableName\"\n" +
                "                    AND c.column_name = \"columnName\"\n" +
                "                    AND c.udt_name = 'geometry'\n");
        virtualTable.setEscapeSql(true);
        virtualTable.setName("trajectoryVirtualTable");
        virtualTable.addVirtualTableGeometry("value", "Geometry", "4326"); // geom needs to match the sql query
        virtualTable.addVirtualTableParameter("pointiri","",".*");
        geoServerVectorSettings.setVirtualTable(virtualTable);
        geoServerClient.createPostGISDataStore(workspaceName,"trajectory" , dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName,"trajectory" ,geoServerVectorSettings);

        //Sample requests to retrieve Geojson
        //http://localhost:3838/geoserver/twa/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=twa%3Atrajectory&outputFormat=application%2Fjson&viewparams=pointiri:https://www.theworldavatar.com/kg/sensorloggerapp/point_3bfa75a3-5b2c-45d3-b05a-63879a2e7b94
    }

    /**
     * Create connection to remoteStoreClient and execute SQL statement
     * @param connection PostgreSQL connection object
     * @param sql SQl statement to execute
     */
    private void executeSql(Connection connection, String sql) throws SQLException {
        try (Statement statement = connection.createStatement()) {
            statement.execute(sql);
        }
    }

    /**
     * Check if the JSONObject in the processRequestParameters inputs are correct or missing.
     * @param requestParams
     * @return
     * @throws BadRequestException
     */
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (!requestParams.has(USER_ID)) {
            LOGGER.error("userID is missing.");
            return false;
        }
        return true;
    }
}