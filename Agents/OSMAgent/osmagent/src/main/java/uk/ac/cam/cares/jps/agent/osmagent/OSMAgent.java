package uk.ac.cam.cares.jps.agent.osmagent;

import uk.ac.cam.cares.jps.agent.osmagent.usage.UsageMatcher;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.Connection;
import java.util.List;

@WebServlet(urlPatterns = "/update")

public class OSMAgent extends JPSAgent {
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient rdbStoreClient;

    private UsageMatcher usageMatcher;

    private List<String> tableNames = List.of("points", "polygons");

    public void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(),
                endpointConfig.getDbpassword());
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        usageMatcher = new UsageMatcher();
    }

    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        try (Connection conn = rdbStoreClient.getConnection()) {

            usageMatcher.checkAndAddColumns(conn, tableNames);
            usageMatcher.updateOntoBuilt(conn, tableNames);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);

        }

        // Log the request data
        System.out.println("Received POST request");

        // Set the content type of the response
        response.setContentType("text/plain");
    }

}