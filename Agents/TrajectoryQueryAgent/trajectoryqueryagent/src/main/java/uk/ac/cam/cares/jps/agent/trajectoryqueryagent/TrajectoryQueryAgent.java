package uk.ac.cam.cares.jps.agent.trajectoryqueryagent;

import org.apache.jena.graph.Node;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.io.PrintWriter;
import java.time.OffsetDateTime;

import org.apache.jena.graph.NodeFactory;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;


@WebServlet(urlPatterns = "/update")

public class TrajectoryQueryAgent extends JPSAgent {
    private KGQueryClient kgQueryClient;
    private RemoteStoreClient storeClient;

    public void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        kgQueryClient = new KGQueryClient(storeClient);
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