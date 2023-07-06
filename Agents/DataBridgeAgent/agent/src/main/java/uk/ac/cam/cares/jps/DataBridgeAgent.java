package uk.ac.cam.cares.jps;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.bridge.SparqlBridge;
import uk.ac.cam.cares.jps.bridge.SqlBridge;
import uk.ac.cam.cares.jps.bridge.TimeSeriesBridge;
import uk.ac.cam.cares.jps.util.ConfigStore;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

/**
 * This class acts as the entry point of the agent that accepts parameter requests to specific routes and achieve its task.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/sql", "/sparql", "/status", "/timeseries"})
public class DataBridgeAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(DataBridgeAgent.class);
    // Agent starts off in valid state, and will be invalid when running into exceptions
    private static boolean VALID = true;
    private static boolean AGENT_IN_STACK = false;
    private static final String INVALID_PARAMETER_ERROR_MSG = "Parameters are invalid, please check logs for more details.";
    private static final String INVALID_ROUTE_ERROR_MSG = "Invalid request type! Route ";
    private static final String KEY_SOURCE_NAMESPACE = "source";
    private static final String KEY_TARGET_NAMESPACE = "target";
    private static final String KEY_NAMESPACE = "namespace";
    private static final String KEY_DATABASE = "database";
    private static final String KEY_SOURCE_DATABASE = "srcDbName";
    private static final String KEY_TARGET_DATABASE = "tgtDbName";
    private static final String KEY_TIME_CLASS = "timeClass";
    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_VALUES = "values";

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
        // Validate input and if it is false, do not continue with the task
        if (!validateInput(requestParams)) {
            jsonMessage.put("Result", INVALID_PARAMETER_ERROR_MSG);
            return jsonMessage;
        }
        // Retrieve the request type and route
        String requestType = requestParams.get("method").toString();
        String route = requestParams.get("requestUrl").toString();
        // Retrieve the route name
        route = route.substring(route.lastIndexOf("/") + 1);
        LOGGER.info("Passing request to the DataBridge Agent...");
        // Run logic based on request path
        switch (route) {
            case "sparql":
                if (requestType.equals("POST")) {
                    String[] config = new String[2];
                    config[0] = requestParams.get(KEY_SOURCE_NAMESPACE).toString();
                    config[1] = requestParams.get(KEY_TARGET_NAMESPACE).toString();
                    jsonMessage = sparqlRoute(config);
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                }
                break;
            case "sql":
                if (requestType.equals("GET")) {
                    String[] config = requestParams.has(KEY_SOURCE_DATABASE) ? ConfigStore.retrieveSQLConfig(requestParams.get(KEY_SOURCE_DATABASE).toString(), true) :
                            requestParams.has(KEY_TARGET_DATABASE) ? ConfigStore.retrieveSQLConfig(requestParams.get(KEY_TARGET_DATABASE).toString(), false) :
                                    ConfigStore.retrieveSQLConfig();
                    AGENT_IN_STACK = requestParams.has(KEY_SOURCE_DATABASE) || requestParams.has(KEY_TARGET_DATABASE);
                    jsonMessage = sqlRoute(config);
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                }
                break;
            case "timeseries":
                if (requestType.equals("POST")) {
                    String db = requestParams.has(KEY_DATABASE) ? requestParams.getString(KEY_DATABASE) : "";
                    String[] config = ConfigStore.retrieveTSClientConfig(requestParams.getString(KEY_NAMESPACE), db);
                    jsonMessage = timeSeriesRoute(config, requestParams);
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
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
        boolean validate = false;
        // If request is sent to status route, there are no parameters to validate
        if (requestParams.get("requestUrl").toString().contains("status")) return true;
        // For the time series route, these parameters must be passed, and the validation will end
        if (requestParams.has(KEY_TIME_CLASS) || requestParams.has(KEY_TIMESTAMP) || requestParams.has(KEY_VALUES)) {
            validate = requestParams.has(KEY_TIME_CLASS) && requestParams.has(KEY_TIMESTAMP) && requestParams.has(KEY_VALUES) && requestParams.has(KEY_NAMESPACE) && requestParams.has(KEY_DATABASE);
            if (validate) {
                LOGGER.info("Detected time series parameters...");
                String timeClass = requestParams.getString(KEY_TIME_CLASS);
                LOGGER.info("Validating the timeClass parameter...");
                return timeClass.equals("AVERAGE") || timeClass.equals("STEPWISECUMULATIVE") || timeClass.equals("CUMULATIVETOTAL") || timeClass.equals("INSTANTANEOUS") || timeClass.equals("GENERAL");
            } else {
                LOGGER.fatal("The request is missing at least one of these parameters: " + KEY_TIME_CLASS + " " + KEY_TIMESTAMP + " " + KEY_VALUES);
                return false;
            }
        }
        // Verify if there is a source and target parameter for the `sparql` route
        if (requestParams.get("requestUrl").toString().contains("sparql")) {
            if (requestParams.has(KEY_SOURCE_NAMESPACE)) {
                LOGGER.info("Detected source namespace parameter...");
                String namespace = requestParams.get(KEY_SOURCE_NAMESPACE).toString();
                // As SPARQL endpoints varies based on the triplestore, the agent only validates if HTTP or HTTPS protocol is used
                validate = namespace.startsWith("http://") || namespace.startsWith("https://");
                if (!validate) {
                    LOGGER.fatal("Source namespace does not start with http or https protocol!");
                    return false;
                }
                if (requestParams.has(KEY_TARGET_NAMESPACE)) {
                    LOGGER.info("Detected target namespace parameter...");
                    namespace = requestParams.get(KEY_TARGET_NAMESPACE).toString();
                    return namespace.startsWith("http://") || namespace.startsWith("https://");
                } else {
                    LOGGER.fatal("The request is missing a `target` parameter!");
                    return false;
                }
            } else {
                LOGGER.fatal("The request is missing a `source` parameter!");
                return false;
            }
        }
        // If there are parameters passed for the sql route
        if (requestParams.get("requestUrl").toString().contains("sql")) {
            if (requestParams.has(KEY_SOURCE_DATABASE) && requestParams.has(KEY_TARGET_DATABASE)) {
                    LOGGER.fatal("Detected both `srcDbName` and `tgtDbName` parameters! Only one of these parameters is needed");
                    return false;
            }
            if (requestParams.has(KEY_SOURCE_DATABASE)) {
                if (!(requestParams.get(KEY_SOURCE_DATABASE) instanceof String)) {
                    LOGGER.fatal("`srcDbName` is not a string!");
                    return false;
                }
            }
            if (requestParams.has(KEY_TARGET_DATABASE)) {
                if (!(requestParams.get(KEY_TARGET_DATABASE) instanceof String)) {
                    LOGGER.fatal("`tgtDbName` is not a string!");
                    return false;
                }
            }
            validate = true;
        }
        return validate;
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
        if (response.isEmpty()) {
            response.put("Result", "Data have been successfully transferred from " + config[0] + " to " + config[3]);
        }
        return response;
    }

    /**
     * Run logic for the "/timeseries" route to instantiate time series into the SPARQL endpoint and PostgreSQL database.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected <T> JSONObject timeSeriesRoute(String[] config, JSONObject requestParams) {
        LOGGER.info("Creating bridge to endpoint and database...");
        String timeClass = requestParams.getString(KEY_TIME_CLASS);
        TimeSeriesBridge tsBridge = new TimeSeriesBridge(config, timeClass);
        try {
            LOGGER.info("Instantiating time series...");
            return tsBridge.instantiateTimeSeries(requestParams);
        } catch (Exception e) {
            throw new JPSRuntimeException("Failed to instantiate time series: " + e);
        }
    }
}
