package uk.ac.cam.cares.jps.agent.pips;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class with a main method that is the entry point of the compiled war and puts all components together to retrieve
 * data from the API and write it into the database.
 * @author GMMajal
 */
@WebServlet(urlPatterns = {"/status"})
public class PIPSAgent extends JPSAgent {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(PIPSAgent.class);

    /**
     * Logging / error messages
     */
    private static final String UNDEFINED_ROUTE_ERROR_MSG = "Invalid route! Requested route does not exist for : ";

    /**
     * Keys
     */
    private static final String JSON_ERROR_KEY = "Error";
    private static final String JSON_RESULT_KEY = "Result";

    private boolean valid = false;
    
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
        valid = true;
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        LOGGER.info("Passing request to CARES Weather Station Input Agent..");
        String route = requestParams.get("requestUrl").toString();
        route = route.substring(route.lastIndexOf("/") + 1);
        switch (route) {
            case "status":
                jsonMessage = statusRoute();
                break;
            default:
                LOGGER.fatal("{}{}", UNDEFINED_ROUTE_ERROR_MSG, route);
                jsonMessage.put(JSON_ERROR_KEY, UNDEFINED_ROUTE_ERROR_MSG + route);
        }
        return jsonMessage;
    }

    /**
     * Handle GET /status route and return the status of the agent.
     *
     * @return Status of the agent
     */
    private JSONObject statusRoute() {
        JSONObject response = new JSONObject();
        LOGGER.info("Detected request to get agent status...");
        if (valid) {
            response.put(JSON_RESULT_KEY, "Agent is ready to receive requests.");
        } else {
            response.put(JSON_ERROR_KEY, "Agent could not be initialised! Please check logs for more information.");
        }
        return response;
    }
}