package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.regex.Pattern;

/**
 * This class acts as the entry point of the agent that accepts parameter requests to specific routes.
 * It processes requests, reads configurations of other clients, sets up the agent and calls functions in the agent to achieve its task.
 *
 * @author sandradeng20
 */
@WebServlet(urlPatterns = {"/status", "/retrieve/equipment", "/retrieve/zones"})
public class BMSQueryAgentLauncher extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(BMSQueryAgentLauncher.class);

    static final String KEY_ROOMIRI = "roomIRI";

    public static final String PARAMETERS_VALIDATION_ERROR_MSG = "Unable to validate request sent to the agent.";
    public static final String EMPTY_PARAMETER_ERROR_MSG = "Empty Request.";
    public static final String AGENT_Construction_ERROR_MSG = "The BMSQueryAgent could not be constructed.";

    /**
     * Servlet init.
     *
     * @throws ServletException
     */
    @Override
    public void init() throws ServletException {
        super.init();
        LOGGER.debug("This is a debug message.");
        LOGGER.info("This is an info message.");
        LOGGER.warn("This is a warn message.");
        LOGGER.error("This is an error message.");
        LOGGER.fatal("This is a fatal message.");
    }

    /**
     * Handle GET request and route to different functions based on the path.
     * @param requestParams Parameters sent with HTTP request
     * @param request HTTPServletRequest instance
     * @return result of the request
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {}", datetime);

        String url = request.getRequestURI();
        if (url.contains("?")) url = url.split(Pattern.quote("?"))[0];

        if (url.contains("status")) {
            return getStatus();
        }

        if (url.contains("retrieve")) {
            BMSQueryAgent agent = initializeAgent();

            if (url.contains("zones")) {
                // handle the case "retrieve/zones", return the list of zones (buildings, facilities, rooms)
                JSONObject queryResult = agent.queryAllZones();

                LOGGER.info("query for zones is completed");
                return queryResult;
            } else if (url.contains("equipment")) {
                // handle the case "retrieve/equipment", return the list of equipments in a given room

                if (!validateInput(request)) {
                    LOGGER.error(PARAMETERS_VALIDATION_ERROR_MSG);
                    throw new JPSRuntimeException(PARAMETERS_VALIDATION_ERROR_MSG);
                }

                String roomIRI = request.getParameter(KEY_ROOMIRI);

                JSONObject queryResult = agent.queryEquipmentInstances(roomIRI);

                LOGGER.info("query for equipment in " + roomIRI + " is completed");
                return queryResult;
            }
        }

        throw new JPSRuntimeException("Route: " + url + " does not exist");
    }

    /**
     * Validate request params.
     *
     * @param request Http request
     * @return Validity of the request params
     */
    public boolean validateInput(HttpServletRequest request) {
        LOGGER.info("Getting requestParams: " + request.getQueryString());

        if (request.getParameterMap().isEmpty()) {
            LOGGER.error(EMPTY_PARAMETER_ERROR_MSG);
            return false;
        }

        if (request.getParameter(KEY_ROOMIRI).isEmpty()) {
            LOGGER.error(KEY_ROOMIRI + "is missing.");
            return false;
        }
        LOGGER.info("Data Received: " + request.getParameter(KEY_ROOMIRI));

        return true;
    }

    /**
     * Initialize agent and its remotestore client with EndpointConfig which get other agents' config from docker stack
     * @return
     */
    private BMSQueryAgent initializeAgent() {
        EndpointConfig endpointConfig = new EndpointConfig();

        BMSQueryAgent agent = createBMSQueryAgent();

        RemoteStoreClient rsClient = new RemoteStoreClient();
        rsClient.setUser(endpointConfig.getKguser());
        rsClient.setPassword(endpointConfig.getKgpassword());
        agent.setRSClient(rsClient, endpointConfig.getKgurls());

        LOGGER.info("Input agent object initialized.");
        return agent;
    }

    /**
     * Create BMSQueryAgent
     * @return BMSQueryAgent instance
     */
    private BMSQueryAgent createBMSQueryAgent() {
        BMSQueryAgent agent;
        try {
            agent = new BMSQueryAgent();
        } catch (Exception e) {
            LOGGER.error(AGENT_Construction_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_Construction_ERROR_MSG, e);
        }
        return agent;
    }

    /**
     * Handle GET /status route and return the status of the agent.
     * @return Status of the agent
     */
    private JSONObject getStatus() {
        LOGGER.info("Detected request to get agent status...");
        JSONObject result = new JSONObject();
        result.put("description", "BMSQueryAgent is ready.");
        return result;
    }

}
