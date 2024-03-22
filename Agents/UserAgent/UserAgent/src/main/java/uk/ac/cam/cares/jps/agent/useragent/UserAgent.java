package uk.ac.cam.cares.jps.agent.useragent;

import com.cmclinnovations.stack.clients.ontop.OntopClient;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.codehaus.jettison.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import java.nio.file.Path;


@WebServlet(urlPatterns = "/registerPhone")

public class UserAgent extends JPSAgent {
    private TimelineRDBStoreHelper timelineRdbStoreHelper;
    private final Logger LOGGER = LogManager.getLogger(UserAgent.class);

    private static final Path obdaFile = Path.of("/inputs/user.obda");

    public void init() {
        LOGGER.debug("This is a debug message.");
        LOGGER.info("This is an info message.");
        LOGGER.warn("This is a warn message.");
        LOGGER.error("This is an error message.");
        LOGGER.fatal("This is a fatal message.");

        EndpointConfig endpointConfig = new EndpointConfig();

        LOGGER.info("initializing rdb");
        RemoteRDBStoreClient postgresRdbClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        timelineRdbStoreHelper = new TimelineRDBStoreHelper(postgresRdbClient);

        try {
            OntopClient ontopClient = OntopClient.getInstance();
            LOGGER.info("updating obda");
            ontopClient.updateOBDA(obdaFile);
        } catch (Exception e) {
            System.out.println(e.getMessage());
            System.out.println("Could not retrieve user.obda file.");
        }
    }


    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        if (request.getRequestURI().contains("registerPhone")) {
            validateInputRegisterPhone(requestParams);
            timelineRdbStoreHelper.registerPhone(requestParams.getString("phoneId"), requestParams.getString("userId"));

            JSONObject result = new JSONObject();
            result.put("Comment", "Phone is registered successfully.");
            return result;
        }
        return processRequestParameters(requestParams);
    }

    private void validateInputRegisterPhone(JSONObject requestParams) throws BadRequestException {
        LOGGER.debug("Request received: " + requestParams.toString());
        if (!requestParams.has("userId")) {
            throw new BadRequestException("No user id provided");
        }
        if (!requestParams.has("phoneId")) {
            throw new BadRequestException("No phone id provided");
        }
    }
}