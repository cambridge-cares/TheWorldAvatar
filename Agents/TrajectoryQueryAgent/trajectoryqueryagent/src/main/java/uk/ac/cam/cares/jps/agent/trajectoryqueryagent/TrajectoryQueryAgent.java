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

        //Retrieve params (Although currently receiver deviceID)
        this.userID = requestParams.getString(USER_ID);

        //SPARQL query for pointIRI based on userID - Note currently userID is deviceID, needs to be changed
        Node smartphoneIRI = NodeFactory.createURI(kgQueryClient.getIRIfromJSONarray(kgQueryClient.getSmartPhoneIRI(userID)));
        String pointIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getPointIRIArray(smartphoneIRI));
        String altitudeIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getAltitudeIRIArray(smartphoneIRI));
        String speedIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getSpeedIRIArray(smartphoneIRI));
        String bearingIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getBearingIRIArray(smartphoneIRI));


        //Create Geoserver layer
        createGeoserver(pointIRI,altitudeIRI,speedIRI,bearingIRI );

        //Return pointIRI to app as response
        JSONObject response = new JSONObject();
        response.put("message", "PointIRI is XXXX.");
        return response;
    }


    private void createGeoserver(String pointIRI, String altitudeIRI, String speedIRI, String bearingIRI){
        //Function retrieves column_name from dbTable given IRI
         String getColumnNameFunction = "CREATE OR REPLACE FUNCTION getColumnName(iri VARCHAR)\n" +
                 "RETURNS VARCHAR AS\n" +
                 "$$\n" +
                 "DECLARE\n" +
                 "    column_name VARCHAR;\n" +
                 "BEGIN\n" +
                 "    SELECT \"columnName\" INTO column_name FROM \"dbTable\" WHERE \"dataIRI\" = iri;\n" +
                 "    RETURN column_name;\n" +
                 "END;\n" +
                 "$$\n" +
                 "LANGUAGE plpgsql;";

        //Function retrieves table_name from dbTable given IRI
         String getTableNameFunction= "CREATE OR REPLACE FUNCTION getTableName(iri VARCHAR)\n" +
                 "RETURNS VARCHAR AS\n" +
                 "$$\n" +
                 "DECLARE\n" +
                 "    table_name VARCHAR;\n" +
                 "BEGIN\n" +
                 "    SELECT \"tableName\" INTO table_name FROM \"dbTable\" WHERE \"dataIRI\" = iri;\n" +
                 "    RETURN table_name;\n" +
                 "END;\n" +
                 "$$\n" +
                 "LANGUAGE plpgsql;";

        //Function retrieves locationTable given pointiri, speediri, altitudeiri, bearingiri
         String getLocationTableFunction="CREATE OR REPLACE FUNCTION getLocationTable(pointiri VARCHAR, speediri VARCHAR, altitudeiri VARCHAR, bearingiri VARCHAR)\n" +
                 "RETURNS TABLE (\"time\" timestamptz, \"geom\" geometry, \"speed\" double precision, \"altitude\" double precision, \"bearing\" double precision)\n" +
                 "AS $$\n" +
                 "DECLARE\n" +
                 "    tableName TEXT;\n" +
                 "BEGIN\n" +
                 "    tableName := getTableName(pointiri);\n" +
                 "    RETURN QUERY EXECUTE \n" +
                 "        format('SELECT time, %I AS geom, %I AS speed, %I AS altitude, %I AS bearing FROM %I', \n" +
                 "               getColumnName(pointiri), getColumnName(speediri), getColumnName(altitudeiri), getColumnName(bearingiri), tableName);\n" +
                 "END\n" +
                 "$$ LANGUAGE plpgsql;";

        try (Connection connection = remoteRDBStoreClient.getConnection()) {
            executeSql(connection, getColumnNameFunction);
            executeSql(connection, getTableNameFunction);
            executeSql(connection, getLocationTableFunction);
            System.out.println("Created get_geometry_table function.");
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        //Create geoserver point layer
        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        String workspaceName= "twa";
        String schema = "public";
        String dbName="postgres";
        geoServerClient.createWorkspace(workspaceName);
        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        virtualTable.setSql("SELECT timeseries.time AS time,\n" +
                "timeseries.speed AS speed,\n" +
                "timeseries.altitude AS altitude,\n" +
                "timeseries.geom AS geom,\n" +
                "timeseries.bearing AS bearing\n" +
                "FROM public.getLocationTable('%pointiri%','%speediri%','%altitudeiri%','%bearingiri%') as timeseries");
        virtualTable.setEscapeSql(true);
        virtualTable.setName("trajectoryPointVirtualTable");
        virtualTable.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        virtualTable.addVirtualTableParameter("pointiri",pointIRI,".*");
        virtualTable.addVirtualTableParameter("speediri",speedIRI,".*");
        virtualTable.addVirtualTableParameter("altitudeiri",altitudeIRI,".*");
        virtualTable.addVirtualTableParameter("bearingiri",bearingIRI,".*");
        geoServerVectorSettings.setVirtualTable(virtualTable);
        geoServerClient.createPostGISDataStore(workspaceName,"trajectoryPoint" , dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName,"trajectoryPoint" ,geoServerVectorSettings);
        
        //Create line layer
        UpdatedGSVirtualTableEncoder virtualTableLine = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorLineSettings = new GeoServerVectorSettings();
        virtualTableLine.setSql("WITH numbered_points AS (\n" +
                "    SELECT\n" +
                "        time,\n" +
                "        speed,\n" +
                "        altitude,\n" +
                "        geom,\n" +
                "        bearing,\n" +
                "        LEAD(geom) OVER (ORDER BY time) AS next_geom\n" +
                "    FROM\n" +
                "        public.getLocationTable('%pointiri%','%speediri%','%altitudeiri%','%bearingiri%') as timeseries\n" +
                ")\n" +
                "SELECT\n" +
                "    ST_MakeLine(geom, next_geom) AS geom,\n" +
                "    AVG(speed) AS speed,\n" +
                "    AVG(altitude) AS altitude,\n" +
                "    AVG(bearing) AS bearing\n" +
                "FROM\n" +
                "    numbered_points\n" +
                "GROUP BY\n" +
                "    geom, next_geom");
        virtualTableLine.setEscapeSql(true);
        virtualTableLine.setName("trajectoryLineVirtualTable");
        virtualTableLine.addVirtualTableGeometry("geom", "Geometry", "4326"); // geom needs to match the sql query
        virtualTableLine.addVirtualTableParameter("pointiri",pointIRI,".*");
        virtualTableLine.addVirtualTableParameter("speediri",speedIRI,".*");
        virtualTableLine.addVirtualTableParameter("altitudeiri",altitudeIRI,".*");
        virtualTableLine.addVirtualTableParameter("bearingiri",bearingIRI,".*");
        geoServerVectorLineSettings.setVirtualTable(virtualTableLine);
        geoServerClient.createPostGISDataStore(workspaceName,"trajectoryLine" , dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName,"trajectoryLine" ,geoServerVectorLineSettings);


        //Sample point request - specify pointIRI, speedIRI, altitudeIRI, bearingIRI
        //http://localhost:3838/geoserver/twa/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=twa%3AtrajectoryPoint&outputFormat=application%2Fjson&viewparams=pointiri:https://www.theworldavatar.com/kg/sensorloggerapp/point_3bfa75a3-5b2c-45d3-b05a-63879a2e7b94;speediri:https://www.theworldavatar.com/kg/sensorloggerapp/measure_speed_b9f3ee65-7269-4aef-a738-fd7bf9485143;altitudeiri=https://www.theworldavatar.com/kg/sensorloggerapp/measure_altitude_86b4c979-4d94-42ff-8844-a64ee1cb1229;bearingiri:https://www.theworldavatar.com/kg/sensorloggerapp/measure_bearing_4733199d-46de-429e-ac1d-c94fca537e7a;

        //Sample line request - specify pointIRI, speedIRI, altitudeIRI, bearingIRI
        //http://localhost:3838/geoserver/twa/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=twa%3AtrajectoryLine&outputFormat=application%2Fjson&viewparams=pointiri:https://www.theworldavatar.com/kg/sensorloggerapp/point_3bfa75a3-5b2c-45d3-b05a-63879a2e7b94;speediri:https://www.theworldavatar.com/kg/sensorloggerapp/measure_speed_b9f3ee65-7269-4aef-a738-fd7bf9485143;altitudeiri=https://www.theworldavatar.com/kg/sensorloggerapp/measure_altitude_86b4c979-4d94-42ff-8844-a64ee1cb1229;bearingiri:https://www.theworldavatar.com/kg/sensorloggerapp/measure_bearing_4733199d-46de-429e-ac1d-c94fca537e7a;
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