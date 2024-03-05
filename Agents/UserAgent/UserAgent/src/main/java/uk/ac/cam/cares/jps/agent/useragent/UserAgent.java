package uk.ac.cam.cares.jps.agent.useragent;

import com.cmclinnovations.stack.clients.ontop.OntopClient;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import java.nio.file.Path;


@WebServlet(urlPatterns = "/registerPhone")

public class UserAgent extends JPSAgent {
    private KGQueryClient kgQueryClient;
    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient timelineRdbClient;
    private final Logger LOGGER = LogManager.getLogger(UserAgent.class);

    private static final Path obdaFile = Path.of("/inputs/user.obda");

    public void init() {
        EndpointConfig endpointConfig = new EndpointConfig();
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        kgQueryClient = new KGQueryClient(storeClient, endpointConfig.getOntopurl());

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
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        return new JSONObject();
    }

}