package uk.ac.cam.cares.jps;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

/**
 * This class acts as the entry point of the agent that accepts parameter requests to specific routes and achieve its task.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/sql", "/sparql", "/status"})
public class DataBridgeAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(DataBridgeAgent.class);
    // Agent starts off in valid state, and will be invalid when running into exceptions
    private static boolean VALID = true;
    private static boolean AGENT_IN_STACK = false;
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
        } catch (Exception exception) {
            DataBridgeAgent.VALID = false;
            LOGGER.error("Could not initialise an agent instance!", exception);
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
        // Retrieve the route name
        route = route.substring(route.lastIndexOf("/") + 1);
        LOGGER.info("Passing request to the DataBridge Agent...");
        // Run logic based on request path
        switch (route) {
            case "sparql":
                if (requestType.equals("GET")) {
                    String[] config = requestParams.has("namespace") ? ConfigStore.retrieveSPARQLConfig(requestParams.get("namespace").toString()) : ConfigStore.retrieveSPARQLConfig();
                    jsonMessage = sparqlRoute(config);
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                }
                break;
            case "sql":
                if (requestType.equals("GET")) {
                    String[] config = requestParams.has("database") ? ConfigStore.retrieveSQLConfig(requestParams.get("database").toString()) : ConfigStore.retrieveSQLConfig();
                    AGENT_IN_STACK = requestParams.has("database") ? true : false;
                    jsonMessage = sqlRoute(config);
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                }
                break;
            case "status":
                if (requestType.equals("GET")) {
                    jsonMessage = statusRoute();
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                }
                break;
            case "timeseries":
                if (requestType.equals("POST")){
                    //TODO change param name
                    String[] config = requestParams.has("database") ? ConfigStore.retrieveSQLConfig(requestParams.get("database").toString()) : ConfigStore.retrieveSQLConfig();
                    AGENT_IN_STACK = false;
                    jsonMessage = updateTimeSeries(config);
                }
                else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                }
        }
        return jsonMessage;
    }

    /**
     * Validates the request parameter.
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
        if (DataBridgeAgent.VALID) {
            response.put("Result", "Agent is ready to receive requests.");
        } else {
            response.put("Result", "Agent could not be initialised!");
        }
        return response;
    }

    /**
     * Run logic for the "/sparql" route to transfer data between SPARQL endpoints.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected JSONObject sparqlRoute(String[] config) {
        JSONObject response = new JSONObject();
        LOGGER.debug("Creating the SPARQL connector..");
        SparqlBridge connector = new SparqlBridge(config[0], config[1]);
        LOGGER.debug("Transfer data from source to target endpoint...");
        connector.transfer();
        LOGGER.info("Triples have been successfully transferred from " + config[0] + " to " + config[1]);
        response.put("Result", "Triples have been successfully transferred from " + config[0] + " to " + config[1]);
        return response;
    }

    /**
     * Run logic for the "/sql" route to transfer data between SPARQL endpoints.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected JSONObject sqlRoute(String[] config) {
        LOGGER.debug("Creating the SQL connector..");
        SqlBridge connector = new SqlBridge(config);
        LOGGER.debug("Transfer data from source to target database...");
        JSONObject response = connector.transfer(AGENT_IN_STACK);
        LOGGER.info("Data have been successfully transferred from " + config[0] + " to " + config[3]);
        if(response.isEmpty()){
            response.put("Result", "Data have been successfully transferred from " + config[0] + " to " + config[3]);
        }
        return response;
    }

    protected JSONObject updateTimeSeries(String[] config) {
        JSONObject response = new JSONObject();
        


        return response;
    }
}
