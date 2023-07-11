package uk.ac.cam.cares.jsp.integration;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;

public class DataIntegration extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(DataIntegrationAgent.class);
    // Agent starts off in valid state, and will be invalid when running into exceptions
    private static boolean VALID = true;
    private static boolean AGENT_IN_STACK = false;
    private static final String INVALID_PARAMETER_ERROR_MSG = "Parameters are invalid, please check logs for more details.";
    private static final String INVALID_ROUTE_ERROR_MSG = "Invalid request type! Route ";
    private static final String KEY_DATABASE = "database";
    private static final String KEY_SOURCE_DATABASE = "srcDbName";
    private static final String DATABASE_2D = "2d";
    private static final String DATABASE_3D = "3d";
    private static final String TABLE_2D = "table2d";
    private static final String KEY_VALUES = "values";

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
            this.VALID = false;
            LOGGER.error("Could not initialise an agent instance!", exception);
        }
    }

    public JSONObject processRequestParameters() {
        JSONObject jsonMessage = new JSONObject();
        String[] config = Config.retrieveSQLConfig();
        jsonMessage = sqlRoute(config);
    
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

        // If there are parameters passed for the sql route
        if (requestParams.get("requestUrl").toString().contains("sql")) {
            if (requestParams.has(KEY_SOURCE_DATABASE) ) {
                LOGGER.fatal("Detected `srcDbName` parameters!");
                return false;
            }
            if (requestParams.has(KEY_SOURCE_DATABASE)) {
                if (!(requestParams.get(KEY_SOURCE_DATABASE) instanceof String)) {
                    LOGGER.fatal("`srcDbName` is not a string!");
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
        if (this.VALID) {
            response.put("Result", "Agent is ready to receive requests.");
        } else {
            response.put("Result", "Agent could not be initialised!");
        }
        return response;
    }

    /**
     * Run logic for the "/sql" route to do spatial link.
     *
     * @return A response to the request called as a JSON Object.
     */
    protected JSONObject sqlRoute(String[] config) {
        LOGGER.debug("Creating the SQL connector..");
        JSONObject response = new JSONObject();
        SpatialLink spatialLink = new SpatialLink();
        spatialLink.SpatialLink(config);
//        SqlBridge connector = new SqlBridge(config);
//        LOGGER.debug("Transfer data from source to target database...");
//        JSONObject response = connector.transfer(AGENT_IN_STACK);
//        LOGGER.info("Data have been successfully transferred from " + config[0] + " to " + config[3]);
        if (response.isEmpty()) {
            response.put("Result", "Data have been successfully integrated");
        }
        return response;
    }

    
}
