package uk.ac.cam.cares.jps.agent.aqmesh;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.util.Date;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class with a main method that is the entry point of the compiled jar and puts all components together to retrieve
 * data from the API and write it into the database.
 * @author Niklas Kasenburg
 */
@WebServlet(urlPatterns = {"/retrieve", "/status"})
public class AQMeshInputAgentLauncher extends JPSAgent {
    private boolean valid = false;
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AQMeshInputAgentLauncher.class);
    /**
     * Logging / error messages
     */
    private static final String ARGUMENT_MISMATCH_MSG = "Need three properties files in the following order: 1) input agent 2) time series client 3) API connector.";
    private static final String AGENT_ERROR_MSG = "The AQMesh input agent could not be constructed!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String CONNECTOR_ERROR_MSG = "Could not construct the AQMesh API connector needed to interact with the API!";
    private static final String GET_READINGS_ERROR_MSG = "One or both readings could not be retrieved, this might have created a mismatch" +
            " in the pointers if one readings was successful and needs to be fixed!";
    private static final String ONE_READING_EMPTY_ERROR_MSG = "One of the readings (gas or particle) is empty, that means there is " +
            "a mismatch in the pointer for each readings. This should be fixed (and might require a clean up of the database)!";
    private static final String UNDEFINED_ROUTE_ERROR_MSG = "Invalid route! Requested route does not exist for : ";
    
    /**
     * Environment Variables
     */
    private static final String AQMESH_AGENT_PROPERTIES_ENV = "AQMESH_AGENT_PROPERTIES";
    private static final String AQMESH_API_POPERTIES_ENV = "AQMESH_API_PROPERTIES";
    private static final String AQMESH_CLIENT_PROPERTIES_ENV = "AQMESH_CLIENT_PROPERTIES";

    /**
     * Keys
     */
    private static final String JSON_ERROR_KEY = "Error";
    private static final String JSON_RESULT_KEY = "Result";

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
     * A method to process all the different HTTP (GET/POST/PULL..) requests.
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
        String[] args = new String[]{AQMESH_AGENT_PROPERTIES_ENV, AQMESH_CLIENT_PROPERTIES_ENV, AQMESH_API_POPERTIES_ENV};
        switch (route) {
            case "status":
                msg = statusRoute();
                break;
            case "retrieve":
                int delay = requestParams.getInt("delay");
                int interval = requestParams.getInt("interval");
                String timeunit = requestParams.getString("timeunit");
                LOGGER.info("Executing retrieve route ...");
                setSchedulerForRetrievedRoute(args, delay, interval, timeunit);
                msg.put("result", "Retrieve route will be executed at the following intervals:" + interval + " " + timeunit);
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

    /**
     * Set up scheduler for the retrieve route
     * @param args contains the environment variables that indicates the location of each config file
     * @param delay time delay before first execution
     * @param interval interval between successive executions
     * @param timeunit the time unit for delay and interval
     */
    private void setSchedulerForRetrievedRoute(String[] args, int delay, int interval, String timeunit) {
        // Create a ScheduledExecutorService with a single thread
        ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();
        
        // Define the task to be scheduled
        Runnable task = new Runnable() {
            public void run() {
                main(args);
            }
        };
        TimeUnit timeUnit = null;
        switch (timeunit) {
            case "seconds":
                timeUnit = TimeUnit.SECONDS;
                break;
            case "minutes":
                timeUnit = TimeUnit.MINUTES;
                break;
            case "hours":
                timeUnit = TimeUnit.HOURS;
                break;
        }
        scheduler.scheduleAtFixedRate(task, delay, interval, timeUnit);
    }

    /**
     * Main method that runs through all steps to update the data received from the AQMesh API.
     * defined in the provided properties file.
     * @param args The command line arguments. Three properties files should be passed here in order: 1) input agent
     *             2) time series client 3) API connector.
     */
    public static void main(String[] args) {

        // Ensure that there are three properties files
        if (args.length != 3) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug("Launcher called with the following files: " + String.join(" ", args));

        // Create the agent
        AQMeshInputAgent agent;
        try {
            agent = new AQMeshInputAgent(System.getenv(args[0]));
        } catch (IOException e) {
            LOGGER.error(AGENT_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
        }
        LOGGER.info("Input agent object initialized.");

        // Create and set the time series client
        TimeSeriesClient<OffsetDateTime> tsClient;
        try {
            tsClient = new TimeSeriesClient<>(OffsetDateTime.class, System.getenv(args[1]));
            agent.setTsClient(tsClient);
        } catch (IOException | JPSRuntimeException e) {
            LOGGER.error(TSCLIENT_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
        }
        LOGGER.info("Time series client object initialized.");

        // Initialize time series'
        try {
            agent.initializeTimeSeriesIfNotExist();
        }
        catch (JPSRuntimeException e) {
            LOGGER.error(INITIALIZE_ERROR_MSG,e);
            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
        }

        // Create the connector to interact with the AQMesh API
        AQMeshAPIConnector connector;
        try {
            connector = new AQMeshAPIConnector(System.getenv(args[2]));
        } catch (IOException e) {
            LOGGER.error(CONNECTOR_ERROR_MSG, e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG, e);
        }
        LOGGER.info("API connector object initialized.");
        connector.connect();

        // Retrieve readings
        JSONArray particleReadings;
        JSONArray gasReadings;
        try {
            particleReadings = connector.getParticleReadings();
            gasReadings = connector.getGasReadings();
        }
        catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }
        LOGGER.info(String.format("Retrieved %d particle readings and %d gas readings.",
                particleReadings.length(), gasReadings.length()));

        // If both readings are not empty there is new data
        if(!particleReadings.isEmpty() && !gasReadings.isEmpty()) {
            // Update the data
            agent.updateData(particleReadings, gasReadings);
            LOGGER.info("Data updated with new readings from API.");
        }
        // If both are empty no new readings are available
        else if(particleReadings.isEmpty() && gasReadings.isEmpty()) {
            LOGGER.info("No new readings are available.");
        }
        // One reading is empty and the other is not. This is likely due to asynchronous access to the readings, which
        // sets the pointers for each reading separately (should not happen when only using the agent unless there is an API error).
        // The pointer should be reset and probably manual clean up in the database is required.
        // Note: This is normally not a problem, but since the AQMeshInputAgent requires that all JSON keys are present in the combined
        // readings, having one reading empty will result in an error when calling the updateData method.
        else {
            LOGGER.error(ONE_READING_EMPTY_ERROR_MSG);
            throw new JPSRuntimeException(ONE_READING_EMPTY_ERROR_MSG);
        }
    }

}
