package uk.ac.cam.cares.jps.agent.carpark;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.carpark.file.ConfigReader;
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
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

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
    private static final String CARPARK_AGENT_PROPERTIES_KEY = "CARPARK_AGENTPROPERTIES";
    private static final String CARPARK_CLIENT_PROPERTIES_KEY = "CARPARK_CLIENTPROPERTIES";
    private static final String CARPARK_API_PROPERTIES_KEY = "CARPARK_APIPROPERTIES";
    private static final String TIMESERIES_IRI_PREFIX = TimeSeriesSparql.TIMESERIES_NAMESPACE + "carpark";

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
                String[] args = new String[]{CARPARK_AGENT_PROPERTIES_KEY, CARPARK_CLIENT_PROPERTIES_KEY, CARPARK_API_PROPERTIES_KEY};
                msg = retrieveRoute(args);
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

    public JSONObject retrieveRoute(String[] args) {
        if (args.length != 3) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.info("Attempting to retrieve mappings...");
        List<JSONKeyToIRIMapper> mappings;
        try {
            mappings = ConfigReader.retrieveKeyToIriMappings(args[0], TIMESERIES_IRI_PREFIX);
        } catch (IOException e) {
            LOGGER.fatal("Failed to retrieve mappings: ", e);
            throw new JPSRuntimeException("Failed to retrieve mappings: ", e);
        }

        LOGGER.info("Setting up the time series and remote store clients...");
        TimeSeriesClient<OffsetDateTime> tsclient;
        RemoteStoreClient kbClient;
        try {
            // Retrieve sparql configurations
            Queue<String> sparqlConfigs = ConfigReader.retrieveSparqlConfig(args[1]);
            kbClient = new RemoteStoreClient();
            // First two configuration should be query and update endpoint
            kbClient.setQueryEndpoint(sparqlConfigs.poll());
            kbClient.setUpdateEndpoint(sparqlConfigs.poll());
            // If there are more configuration, then it should be user and password
            if (!sparqlConfigs.isEmpty()) {
                kbClient.setUser(sparqlConfigs.poll());
                kbClient.setPassword(sparqlConfigs.poll());
            }
            // Retrieve RDB configs and populate it accordingly in sequence of url, user, password
            Queue<String> rdbConfigs = ConfigReader.retrieveRDBConfig(args[1]);
            tsclient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, rdbConfigs.poll(), rdbConfigs.poll(), rdbConfigs.poll());
        } catch (Exception e) {
            LOGGER.error(TSCLIENT_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
        }

        LOGGER.info("Setting up the time series for the instantiation process...");
        TimeSeriesHandler tsHandler;
        try {
            tsHandler = new TimeSeriesHandler(mappings);
            tsHandler.setTsClient(tsclient);
        } catch (IOException e) {
            LOGGER.error(AGENT_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
        }

        try {
            tsHandler.initializeTimeSeriesIfNotExist();
        } catch (JPSRuntimeException e) {
            LOGGER.error(INITIALIZE_ERROR_MSG, e);
            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
        }

        LOGGER.info("Setting up the API connector...");
        APIConnector connector;
        try {
            connector = new APIConnector(args[2]);
        } catch (IOException e) {
            LOGGER.error(CONNECTOR_ERROR_MSG, e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG, e);
        }

        LOGGER.info("Retrieving available carpark lot data from the API...");
        JSONObject carparkReadings;
        try {
            carparkReadings = connector.getReadings();
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }
        JSONObject jsonMessage = new JSONObject();
        LOGGER.info("Retrieved available lot readings for {} carparks", carparkReadings.length());
        jsonMessage.put(JSON_RESULT_KEY, "Retrieved available lot readings for " + carparkReadings.getJSONArray("value").length() + " carparks.");

        LOGGER.info("Instantiating available carpark lot data...");
        if (!carparkReadings.isEmpty()) {
            LOGGER.info("Updating data with new API Readings");
            tsHandler.updateData(carparkReadings);
        }

        LOGGER.info("Retrieving carpark rates from the API...");
        JSONObject pricingReadings;
        try {
            pricingReadings = connector.getPrices();
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }
        LOGGER.info("Retrieved carpark rates for {} carparks", pricingReadings.length());
        jsonMessage.accumulate(JSON_RESULT_KEY, " Retrieved carpark rates for" + pricingReadings.getJSONObject("result").getJSONArray("records").length() + "carparks.");

        LOGGER.info("Setting up the sparql handler...");
        SparqlHandler sparqlHandler;
        try {
            sparqlHandler = new SparqlHandler(mappings, kbClient);
        } catch (Exception e) {
            LOGGER.error("Could not build the QueryBuilder ", e);
            throw new JPSRuntimeException("Could not successfully initialise the QueryBuilder Object", e);
        }
        sparqlHandler.instantiateIfNotInstantiated(carparkReadings, pricingReadings);
        jsonMessage.accumulate(JSON_RESULT_KEY, " Data has been successfully instantiated!");
        return jsonMessage;
    }
}
