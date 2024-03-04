package uk.ac.cam.cares.jps.agent.trajectoryqueryagent;

import com.cmclinnovations.stack.clients.ontop.OntopClient;
import org.apache.jena.graph.Node;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
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


@WebServlet(urlPatterns = "/createlayer")

public class TrajectoryQueryAgent extends JPSAgent {
    private KGQueryClient kgQueryClient;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient timelineRdbClient;

    private static final Path obdaFile = Path.of("/inputs/user.obda");

    public void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        kgQueryClient = new KGQueryClient(storeClient);

        initTimelineDatabase(endpointConfig);
        timelineRdbClient = new RemoteRDBStoreClient(endpointConfig.getDburl("timeline"), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        initPhoneTable();

        try {
            OntopClient ontopClient = OntopClient.getInstance();
            ontopClient.updateOBDA(obdaFile);
        } catch (Exception e) {
            System.out.println("Could not retrieve user.obda file.");
        }
    }

    private void initTimelineDatabase(EndpointConfig endpointConfig) {
        RemoteRDBStoreClient postgresRdbClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        JSONArray result = postgresRdbClient.executeQuery("SELECT datname FROM pg_database WHERE datname = 'timeline';");
        if (!result.isEmpty()) {
            return;
        }

        postgresRdbClient.executeUpdate("CREATE DATABASE timeline;");
    }

    private void initPhoneTable() {
        timelineRdbClient.executeUpdate("CREATE TABLE IF NOT EXISTS 'timeline'.'smartPhone' (" +
                "phone_id CHAR(36) PRIMARY KEY," +
                "user_id CHAR(36)");
    }
    

    //Receive userID
    //Query for pointIRI based on userID
    //Create geoserver layer
    //Return pointIRI to application 
    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse response) throws IOException {

        //Process HTTP Request
        response.setStatus(HttpServletResponse.SC_OK);
        PrintWriter out = response.getWriter();
        out.println("HTTP POST request processed.");

        //Retrieve params
        String deviceID = req.getParameter("userID");

        //Find pointIRI
        //Node smartphoneIRI = NodeFactory.createURI(kgQueryClient.getIRIfromJSONarray(kgQueryClient.getSmartPhoneIRI(deviceID)));
        //String pointIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getPointIRIArray(smartphoneIRI));

        //Create Geoserver layer
        createGeoserver();

        //Return pointIRI to application

    }

    private void createGeoserver(){
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

}