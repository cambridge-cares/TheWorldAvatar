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
    private static final String KEY_NAMESPACE = "namespace";
    private static final String KEY_DATABASE = "database";
    private static final String KEY_TRANSFER = "transfer";
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
        if(!validateInput(requestParams)){
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
                if (requestType.equals("GET")) {
                    String[] config = requestParams.has(KEY_NAMESPACE) ? ConfigStore.retrieveSPARQLConfig(requestParams.get(KEY_NAMESPACE).toString(), requestParams.getString(KEY_TRANSFER)) : ConfigStore.retrieveSPARQLConfig();
                    jsonMessage = sparqlRoute(config);
                } else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept GET request.");
                }
                break;
            case "sql":
                if (requestType.equals("GET")) {
                    String[] config = requestParams.has(KEY_DATABASE) ? ConfigStore.retrieveSQLConfig(requestParams.get(KEY_DATABASE).toString(), requestParams.getString(KEY_TRANSFER)) : ConfigStore.retrieveSQLConfig();
                    AGENT_IN_STACK = requestParams.has(KEY_DATABASE);
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
                    String namespace = null;
                    String db = null;
                    if(requestParams.has(KEY_DATABASE)){
                        db = requestParams.getString(KEY_DATABASE);
                    }

                    if (requestParams.has(KEY_NAMESPACE)){
                        namespace = requestParams.getString(KEY_NAMESPACE);
                    }

                    String[] config = ConfigStore.retrieveTSClientConfig(namespace, db);
                    AGENT_IN_STACK = false;
                    if (requestParams.has(KEY_TIME_CLASS)){
                        String timeClass = requestParams.getString(KEY_TIME_CLASS);
                        jsonMessage = updateTimeSeries(config, requestParams, timeClass);
                    }
                    else{
                        throw new JPSRuntimeException("Missing key: timeClass");
                    }
                }
                else {
                    LOGGER.fatal(INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                    jsonMessage.put("Result", INVALID_ROUTE_ERROR_MSG + route + " can only accept POST request.");
                }
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
            validate = requestParams.has(KEY_TIME_CLASS) && requestParams.has(KEY_TIMESTAMP) && requestParams.has(KEY_VALUES);
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
        // Note that the validation must not continue here for the time series route, as these parameters are optional
        // If there are `namespace` or `database` parameters passed for the sparql or sql route
        if (requestParams.get("requestUrl").toString().contains("sql") || requestParams.get("requestUrl").toString().contains("sparql")) {
            validate = true;
            if (requestParams.has(KEY_NAMESPACE) || requestParams.has(KEY_DATABASE)) {
                LOGGER.info("Detected a namespace or database parameter...");
                // Ensure that a `transfer` parameter is also passed
                if (requestParams.has(KEY_TRANSFER)) {
                    LOGGER.info("Detected a transfer parameter and validating it...");
                    // The transfer parameter must only contain either `in` or `out`
                    String transfer = requestParams.getString(KEY_TRANSFER);
                    validate = transfer.equals("in") || transfer.equals("out");
                    if (!validate) {
                        LOGGER.fatal("Invalid `transfer` value! The parameter must only be either in or out!");
                    }
                } else {
                    LOGGER.fatal("Please include a `transfer` parameter with either in or out!");
                    return false;
                }
            } else if (requestParams.has(KEY_TRANSFER)) {
                LOGGER.fatal("`transfer` parameter is passed without a namespace or database parameter!");
                return false;
            }
            return validate;
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
        if(response.isEmpty()){
            response.put("Result", "Data have been successfully transferred from " + config[0] + " to " + config[3]);
        }
        return response;
    }

    protected <T> JSONObject updateTimeSeries (String[] config, JSONObject data, String timeClass) {
        JSONObject response = new JSONObject();

        LOGGER.info("Instantiating Timeseries...");
        TimeSeriesBridge tsInst = new TimeSeriesBridge(config, timeClass);

        try {
            tsInst.updateTimeSeriesData(data);
        } catch (Exception e) {
            throw new JPSRuntimeException("Failed to instantiate TS: " + e);
        }




        return response;
    }
}
