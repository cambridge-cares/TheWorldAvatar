package uk.ac.cam.cares.jps.agent.dashboard;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.dashboard.json.DashboardClient;
import uk.ac.cam.cares.jps.agent.dashboard.stack.StackClient;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;

/**
 * This class acts as the entry point of the compiled war.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/reset", "/status", "/setup"})
public class DashboardAgent extends JPSAgent {
    // Agent starts off in invalid state, and will become valid when initialised without exceptions
    private static boolean valid = false;
    private static StackClient services;
    private static final String JSON_ERROR_KEY = "Error";
    private static final String JSON_RESULT_KEY = "Result";
    private static final String JSON_RUNTIME_KEY = "Runtime";
    private static final String UNDEFINED_ROUTE_ERROR_MSG = "Invalid route! Requested route does not exist for : ";
    private static final String INVALID_ROUTE_ERROR_MSG = "Invalid request type! Route ";
    private static final String INVALID_HTTP_NON_GET_TYPE_ERROR = "{0}{1} can only accept GET request.";
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);

    /**
     * Perform required setup.
     */
    @Override
    public synchronized void init() throws ServletException {
        try {
            super.init();
            // Ensure logging are properly working
            LOGGER.debug("This is a test DEBUG message");
            LOGGER.info("This is a test INFO message");
            LOGGER.warn("This is a test WARN message");
            LOGGER.error("This is a test ERROR message");
            LOGGER.fatal("This is a test FATAL message");
            // Ensure that the agent is running on a stack by initialising the services
            services = new StackClient();
            // When initialisation occurs without error, the agent becomes valid
            valid = true;
        } catch (ServletException exception) {
            // This error only occurs when super.init() fails
            LOGGER.error("Could not initialise an agent instance!", exception);
            throw new ServletException("Could not initialise an agent instance!", exception);
        } catch (Exception exception) {
            // This error will only occur if the agent is not running on a stack, which causes an error in Stack Client
            LOGGER.error("Agent is not running on a stack and will not be initialised!", exception);
            throw new JPSRuntimeException("Agent is not running on a stack and will not be initialised!", exception);
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
            case "reset":
                if (requestType.equals("GET")) {
                    jsonMessage = resetRoute();
                } else {
                    LOGGER.fatal(INVALID_HTTP_NON_GET_TYPE_ERROR, INVALID_ROUTE_ERROR_MSG, route);
                    jsonMessage.put(JSON_ERROR_KEY, MessageFormat.format(INVALID_HTTP_NON_GET_TYPE_ERROR, INVALID_ROUTE_ERROR_MSG, route));
                }
                break;
            case "status":
                if (requestType.equals("GET")) {
                    jsonMessage = statusRoute();
                } else {
                    LOGGER.fatal(INVALID_HTTP_NON_GET_TYPE_ERROR, INVALID_ROUTE_ERROR_MSG, route);
                    jsonMessage.put(JSON_ERROR_KEY, MessageFormat.format(INVALID_HTTP_NON_GET_TYPE_ERROR, INVALID_ROUTE_ERROR_MSG, route));
                }
                break;
            case "setup":
                if (requestType.equals("GET")) {
                    jsonMessage = setupRoute();
                } else {
                    LOGGER.fatal(INVALID_HTTP_NON_GET_TYPE_ERROR, INVALID_ROUTE_ERROR_MSG, route);
                    jsonMessage.put(JSON_ERROR_KEY, MessageFormat.format(INVALID_HTTP_NON_GET_TYPE_ERROR, INVALID_ROUTE_ERROR_MSG, route));
                }
                break;
            default:
                LOGGER.fatal("{}{}", UNDEFINED_ROUTE_ERROR_MSG, route);
                jsonMessage.put(JSON_ERROR_KEY, UNDEFINED_ROUTE_ERROR_MSG + route);
        }
        // Total agent run time in nanoseconds
        long duration = System.nanoTime() - startTime;
        // If it can be converted to second, return run time in seconds
        if (TimeUnit.NANOSECONDS.toSeconds(duration) > 0) {
            jsonMessage.accumulate(JSON_RUNTIME_KEY, TimeUnit.NANOSECONDS.toSeconds(duration) + "s");
        } else {
            // Else return as nanoseconds
            jsonMessage.accumulate(JSON_RUNTIME_KEY, duration + "ns");
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
        if (valid) {
            response.put(JSON_RESULT_KEY, "Agent is ready to receive requests.");
        } else {
            response.put(JSON_ERROR_KEY, "Agent could not be initialised! Please ensure it is running on a stack!");
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
        if (valid) {
            LOGGER.info("Setting up client to interact with dashboard...");
            try {
                DashboardClient client = new DashboardClient(services);
                client.initDashboard();
            } catch (Exception e) {
                LOGGER.fatal("Dashboard could not be set up!", e);
                response.put(JSON_ERROR_KEY, "Dashboard could not be set up! " + e.getMessage());
                return response;
            }
            LOGGER.info("Dashboard has been successfully set up!");
            response.put(JSON_RESULT_KEY, "Dashboard has been successfully set up!");
        } else {
            response.put(JSON_ERROR_KEY, "Agent could not be initialised! Please check logs for more information...");
        }
        return response;
    }

    /**
     * Run logic for the "/reset" route to reset the agent and re-execute all queries from the knowledge graph.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected static JSONObject resetRoute() {
        JSONObject response = new JSONObject();
        if (valid) {
            services = new StackClient();
            response.put(JSON_RESULT_KEY, "Agent has been successfully reset!");
        } else {
            response.put(JSON_ERROR_KEY, "Agent could not be initialised! Please check logs for more information...");
        }
        return response;
    }
}