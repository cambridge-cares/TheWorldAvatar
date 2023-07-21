package uk.ac.cam.cares.jps.agent.dashboard;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import java.util.concurrent.TimeUnit;

/**
 * This class acts as the entry point of the compiled war.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/status", "/setup"})
public class DashboardAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);
    // Agent starts off in valid state, and will be invalid when running into exceptions
    private static boolean VALID = true;
    private static StackClient SERVICES;
    private static final String INVALID_ROUTE_ERROR_MSG = "Invalid request type! Route ";

    /**
     * Perform required setup.
     */
    @Override
    public synchronized void init() {
        try {
            super.init();
            // Ensure logging are properly working
            LOGGER.debug("This is a test DEBUG message");
            LOGGER.info("This is a test INFO message");
            LOGGER.warn("This is a test WARN message");
            LOGGER.error("This is a test ERROR message");
            LOGGER.fatal("This is a test FATAL message");
            // Ensure that the agent is running on a stack by initialising the services
            SERVICES = new StackClient();
        } catch (ServletException exception) {
            // This error only occurs when super.init() fails
            DashboardAgent.VALID = false;
            LOGGER.error("Could not initialise an agent instance!", exception);
        } catch (Exception exception) {
            // This error will only occur if the agent is not running on a stack, which causes an error in Stack Client
            DashboardAgent.VALID = false;
            LOGGER.error("Agent is not running on a stack and will not be initialised!");
        }
    }

    /**
     * An overloaded method to process all the different HTTP (GET/POST/PULL..) requests.
     * Do note all requests to JPS agents are processed similarly and will only return response objects.
     *
     * @return A response to the request called as a JSON Object.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    /**
     * A method that process all the different HTTP (GET/POST/PULL..) requests.
     * This will validate the incoming request type and parameters against their route options.
     *
     * @return A response to the request called as a JSON Object.
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        // Retrieve the request type and route
        String requestType = requestParams.get("method").toString();
        String route = requestParams.get("requestUrl").toString();
        route = route.substring(route.lastIndexOf("/") + 1);
        LOGGER.info("Passing request to Dashboard Agent...");
        long startTime = System.nanoTime(); // Start timing agent runtime
        // Run logic based on request path
        switch (route) {
            case "status":
                if (requestType.equals("GET")) {
                    jsonMessage = statusRoute();
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                }
                break;
            case "setup":
                if (requestType.equals("GET")) {
                    jsonMessage = setupRoute();
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                }
                break;
        }
        // Total agent run time in nanoseconds
        long duration = System.nanoTime() - startTime;
        // If it can be converted to second, return run time in seconds
        if (TimeUnit.NANOSECONDS.toSeconds(duration) > 0) {
            jsonMessage.accumulate("Runtime", TimeUnit.NANOSECONDS.toSeconds(duration) + "s");
        } else {
            // Else return as nanoseconds
            jsonMessage.accumulate("Runtime", duration + "ns");
        }
        return jsonMessage;
    }

    /**
     * Validates the request parameters.
     *
     * @return true or false depending on valid parameter status.
     */
    @Override
    public boolean validateInput(JSONObject requestParams) {
        return true;
    }

    /**
     * Run logic for the "/status" route that indicates the agent's current status.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected JSONObject statusRoute() {
        JSONObject response = new JSONObject();
        LOGGER.info("Detected request to get agent status...");
        if (DashboardAgent.VALID) {
            response.put("Result", "Agent is ready to receive requests.");
        } else {
            response.put("Result", "Agent could not be initialised! Please ensure it is running on a stack!");
        }
        return response;
    }

    /**
     * Run logic for the "/setup" route to set up the dashboards on the stack.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected JSONObject setupRoute() {
        JSONObject response = new JSONObject();
        if (DashboardAgent.VALID) {
            LOGGER.info("Dashboard has been successfully set up!");
            response.put("Result", "Dashboard has been successfully set up!");
        } else {
            response.put("Result", "Agent could not be initialised! Please check logs for more information...");
        }
        return response;
    }
}