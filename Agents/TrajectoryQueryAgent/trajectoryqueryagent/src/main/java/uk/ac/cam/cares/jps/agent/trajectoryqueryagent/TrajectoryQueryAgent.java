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


@WebServlet(urlPatterns = "/update")

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
    
    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse response)
            throws IOException {

        response.setStatus(HttpServletResponse.SC_OK);
        PrintWriter out = response.getWriter();
        out.println("HTTP POST request processed.");

        // EWKT literal for the scope to create
        String deviceID = req.getParameter("deviceID");
        OffsetDateTime date = OffsetDateTime.parse(req.getParameter("date"));

        Node smartphoneIRI = NodeFactory.createURI(kgQueryClient.getIRIfromJSONarray(kgQueryClient.getSmartPhoneIRI(deviceID)));


        String pointIRI = kgQueryClient.getIRIfromJSONarray(kgQueryClient.getPointIRIArray(smartphoneIRI));

        createGeoserver(pointIRI);

        // Next thing is to pass to pointIRI, date
        // SQL view to return (GPS trajectory, date)

    }

    private void createGeoserver(String pointIRI){

        //Create geoserver layer
        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        String workspaceName= "twa";
        String schema = "public";
        String dbName="postgres";
        geoServerClient.createWorkspace(workspaceName);

        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        virtualTable.setSql("SELECT get_geometry_table(c.\"tableName\", c.\"columnName\")\n" +
                            "FROM (\n" +
                            "    SELECT \"tableName\", \"columnName\" \n" +
                            "    FROM \"dbTable\" \n" +
                            "    WHERE \"dataIRI\" = '"+pointIRI+"'\n" +
                            ") AS c");
        virtualTable.setEscapeSql(true);
        virtualTable.setName("trajectoryVirtualTable");
        virtualTable.addVirtualTableGeometry("geometry", "Geometry", "4326"); // geom needs to match the sql query
        geoServerVectorSettings.setVirtualTable(virtualTable);
        geoServerClient.createPostGISDataStore(workspaceName,"trajectory" , dbName, schema);
        geoServerClient.createPostGISLayer(workspaceName, dbName,"trajectory" ,geoServerVectorSettings);



    }

}