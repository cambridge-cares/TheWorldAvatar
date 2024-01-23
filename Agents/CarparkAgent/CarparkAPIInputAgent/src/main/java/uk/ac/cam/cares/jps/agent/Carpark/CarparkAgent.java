package uk.ac.cam.cares.jps.agent.Carpark;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.util.*;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

@WebServlet(urlPatterns = {"/retrieve", "/status"})

public class CarparkAgent extends JPSAgent {
    // Agent starts off in invalid state, and will become valid when initialised without exceptions
    private static boolean valid = false;
    private static final Logger LOGGER = LogManager.getLogger(CarparkAgent.class);
    private static final String JSON_ERROR_KEY = "Error";
    private static final String JSON_RESULT_KEY = "Result";
    private static final String UNDEFINED_ROUTE_ERROR_MSG = "Invalid route! Requested route does not exist for : ";
    private static final String ARGUMENT_MISMATCH_MSG = "Need three properties files in the following order: 1) input agent 2) time series client 3) API connector.";
    private static final String AGENT_ERROR_MSG = "The timeseries handler could not be constructed!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the timeseries handler!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String CONNECTOR_ERROR_MSG = "Could not construct the carpark API connector needed to interact with the API!";
    private static final String GET_READINGS_ERROR_MSG = "Some readings could not be retrieved.";
    String dbUrl;
    String dbUsername;
    String dbPassword;
    String sparqlQueryEndpoint;
    String sparqlUpdateEndpoint;
    String sparqlUsername;
    String sparqlPassword;

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
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {}", datetime);
        JSONObject msg = new JSONObject();
        String route = requestParams.get("requestUrl").toString();
        route = route.substring(route.lastIndexOf("/") + 1);
        switch (route) {
            case "status":
                msg = statusRoute();
                break;
            case "retrieve":
                String agentProperties = System.getenv("Carpark_AGENTPROPERTIES");
                String clientProperties = System.getenv("Carpark_CLIENTPROPERTIES");
                String apiProperties = System.getenv("Carpark_APIPROPERTIES");

                String[] args = new String[]{agentProperties, clientProperties, apiProperties};
                msg = initializeAgent(args);
                break;
            default:
                LOGGER.fatal("{}{}", UNDEFINED_ROUTE_ERROR_MSG, route);
                msg.put(JSON_ERROR_KEY, UNDEFINED_ROUTE_ERROR_MSG + route);
        }
        return msg;
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

    public JSONObject initializeAgent(String[] args) {
        if (args.length != 3) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug(() -> "Launcher called with the following files: " + String.join(" ", args));

        TimeSeriesHandler tsHandler;
        try {
            tsHandler = new TimeSeriesHandler(args[0]);
        } catch (IOException e) {
            LOGGER.error(AGENT_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
        }

        LOGGER.info("Input Agent object initialized");
        JSONObject jsonMessage = new JSONObject();
        jsonMessage.accumulate(JSON_RESULT_KEY, "Input Agent Object Initialized");

        TimeSeriesClient<OffsetDateTime> tsclient;
        try {
            loadTSClientConfigs(args[1]);
            RemoteStoreClient kbClient = new RemoteStoreClient();
            kbClient.setQueryEndpoint(sparqlQueryEndpoint);
            kbClient.setUpdateEndpoint(sparqlUpdateEndpoint);
            kbClient.setUser(sparqlUsername);
            kbClient.setPassword(sparqlPassword);
            tsclient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, dbUrl, dbUsername, dbPassword);
            tsHandler.setTsClient(tsclient);
        } catch (Exception e) {
            LOGGER.error(TSCLIENT_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
        }
        LOGGER.info("Time Series object initialized");
        jsonMessage.accumulate(JSON_RESULT_KEY, "Time Series Client Object Initialized");

        try {
            tsHandler.initializeTimeSeriesIfNotExist();
        } catch (JPSRuntimeException e) {
            LOGGER.error(INITIALIZE_ERROR_MSG, e);
            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
        }

        APIConnector connector;
        try {
            connector = new APIConnector(args[2]);
        } catch (IOException e) {
            LOGGER.error(CONNECTOR_ERROR_MSG, e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG, e);
        }

        LOGGER.info("API Connector Object Initialized");
        jsonMessage.accumulate(JSON_RESULT_KEY, "API Connector object Initialized");

        JSONObject carparkReadings;

        try {
            carparkReadings = connector.getReadings();
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }

        LOGGER.info("Retrieved {} carpark readings", carparkReadings.length());
        jsonMessage.accumulate(JSON_RESULT_KEY, "Retrieved" + carparkReadings.getJSONArray("value").length() + " carpark readings");

        if (!carparkReadings.isEmpty()) {
            tsHandler.updateData(carparkReadings);
            LOGGER.info("Data updated with new API Readings");
            jsonMessage.accumulate(JSON_RESULT_KEY, "Data updated with new API Readings");
        } else if (carparkReadings.isEmpty()) {
            LOGGER.info("No new carpark data recorded");
            jsonMessage.accumulate(JSON_RESULT_KEY, "No new carpark data recorded");
        }

        JSONObject pricingReadings;

        try {
            pricingReadings = connector.getPrices();
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }

        LOGGER.info("Retrieved pricing readings for {} carparks", pricingReadings.length());
        jsonMessage.accumulate(JSON_RESULT_KEY, "Retrieved" + pricingReadings.getJSONObject("result").getJSONArray("records").length() + "carpark price readings");
        //To call APIQueryBuilder

        SparqlHandler sparqlHandler;

        try {
            sparqlHandler = new SparqlHandler(args[0], args[1]);
            LOGGER.info("QueryBuilder constructed");

        } catch (Exception e) {
            LOGGER.error("Could not build the QueryBuilder ", e);
            throw new JPSRuntimeException("Could not successfully initialise the QueryBuilder Object", e);
        }
        sparqlHandler.instantiateIfNotInstantiated(carparkReadings, pricingReadings);
        LOGGER.info("All Data IRIs within Carpark Readings successfully instantiated");
        jsonMessage.accumulate(JSON_RESULT_KEY, "All Data IRIs successfully instantiated");
        return jsonMessage;
    }

    /**
     * Reads the parameters needed for the timeseries client
     *
     * @param filepath Path to the properties file from which to read the parameters
     */
    private void loadTSClientConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }

        // Try-with-resource to ensure closure of input stream
        try (InputStream input = new FileInputStream(file)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            // Get timeseries client parameters from properties file
            if (prop.containsKey("db.url")) {
                this.dbUrl = prop.getProperty("db.url");
            } else {
                throw new IOException("Properties file is missing \"db.url=<db_url>\"");
            }
            if (prop.containsKey("db.user")) {
                this.dbUsername = prop.getProperty("db.user");
            } else {
                throw new IOException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
                this.dbPassword = prop.getProperty("db.password");
            } else {
                throw new IOException("Properties file is missing \"db.password=<db_password>\"");
            }
            if (prop.containsKey("sparql.query.endpoint")) {
                this.sparqlQueryEndpoint = prop.getProperty("sparql.query.endpoint");
            } else {
                throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
                this.sparqlUpdateEndpoint = prop.getProperty("sparql.update.endpoint");
            } else {
                throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
            }
            this.sparqlUsername = prop.getProperty("sparql.username");
            this.sparqlPassword = prop.getProperty("sparql.password");
        }
    }
}
