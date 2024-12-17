package uk.ac.cam.cares.jps.agent.caresWeatherStation;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.time.OffsetDateTime;
import java.util.Properties;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class with a main method that is the entry point of the compiled war and puts all components together to retrieve
 * data from the API and write it into the database.
 * @author GMMajal
 */
@WebServlet(urlPatterns = {"/retrieve", "/status"})
public class CARESWeatherStationInputAgentLauncher extends JPSAgent {
    private boolean valid = false;
    public static final String KEY_AGENTPROPERTIES = "agentProperties";
    public static final String KEY_APIPROPERTIES = "apiProperties";
    public static final String KEY_CLIENTPROPERTIES = "clientProperties";
    private static String sparqlUpdateEndpoint;
    private static String sparqlQueryEndpoint;
    private static String sparqlUser;
    private static String sparqlPassword;
    private static String dbUrl;
    private static String dbUser;
    private static String dbPassword;


    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(CARESWeatherStationInputAgentLauncher.class);
    /**
     * Logging / error messages
     */
    private static final String ARGUMENT_MISMATCH_MSG = "Need three properties files in the following order: 1) input agent 2) time series client 3) API connector.";
    private static final String AGENT_ERROR_MSG = "The CARESWeatherStation input agent could not be constructed!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String CONNECTOR_ERROR_MSG = "Could not construct the CARES weather station API connector needed to interact with the API!";
    private static final String GET_READINGS_ERROR_MSG = "Some readings could not be retrieved.";
    private static final String LOADTSCONFIG_ERROR_MSG = "Unable to load configs from timeseries client properties file";
    private static final String JSON_ERROR_KEY = "Error";
    private static final String JSON_RESULT_KEY = "Result";
    private static final String UNDEFINED_ROUTE_ERROR_MSG = "Invalid route! Requested route does not exist for : ";

    public static String GEOSERVER_WORKSPACE = System.getenv("GEOSERVER_WORKSPACE");
	public static String DATABASE = System.getenv("DATABASE");
	public static String LAYERNAME = System.getenv("LAYERNAME");

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
            case "retrieve":
                String agentProperties = System.getenv(requestParams.getString(KEY_AGENTPROPERTIES));
                String clientProperties = System.getenv(requestParams.getString(KEY_CLIENTPROPERTIES));
                String apiProperties = System.getenv(requestParams.getString(KEY_APIPROPERTIES));
                try {
                    loadTSClientConfigs(clientProperties);
                } catch (IOException e) {
                    throw new JPSRuntimeException(LOADTSCONFIG_ERROR_MSG, e);
                }
                String[] args = new String[] {agentProperties,clientProperties,apiProperties};
                int delay = requestParams.getInt("delay");
                int interval = requestParams.getInt("interval");
                String timeunit = requestParams.getString("timeunit");
                LOGGER.info("Executing retrieve route ...");
                setSchedulerForRetrieveRoute(args, delay, interval, timeunit);
                jsonMessage.put("result", "Retrieve route will be executed at the following intervals:" + interval + " " + timeunit);
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

    /**
     * Set up scheduler for the retrieve route
     * @param args contains the environment variables that indicates the location of each config file
     * @param delay time delay before first execution
     * @param interval interval between successive executions
     * @param timeunit the time unit for delay and interval
     */
    private void setSchedulerForRetrieveRoute(String[] args, int delay, int interval, String timeunit) {
        // Create a ScheduledExecutorService with a single thread
        ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();
        
        // Define the task to be scheduled
        Runnable task = new Runnable() {
            public void run() {
                initializeAgent(args);
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
     * Main method that runs through all steps to update the data received from the CARES weather station API.
     * defined in the provided properties file.
     * @param args The command line arguments. Three properties files should be passed here in order: 1) input agent
     *             2) time series client 3) API connector.
     * @throws IOException
     */

    public static JSONObject initializeAgent(String[] args) {
        // Ensure that there are three properties files
        if (args.length != 3) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug("Launcher called with the following files: " + String.join(" ", args));

        // Create the agent
        CARESWeatherStationInputAgent agent;
        try {
            agent = new CARESWeatherStationInputAgent(args[0]);
        } catch (IOException e) {
            LOGGER.error(AGENT_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
        }
        LOGGER.info("Input agent object initialized.");
        JSONObject jsonMessage = new JSONObject();
        jsonMessage.accumulate("Result", "Input agent object initialized.");

        // Create and set the time series client
        TimeSeriesClient<OffsetDateTime> tsClient;
        RemoteStoreClient kbClient = new RemoteStoreClient(sparqlQueryEndpoint, sparqlUpdateEndpoint, sparqlUser, sparqlPassword);
        
        try {
            tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class);
            tsClient.setRDBClient(dbUrl, dbUser, dbPassword);
            agent.setTsClient(tsClient);
        } catch (JPSRuntimeException e) {
            LOGGER.error(TSCLIENT_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
        }
        LOGGER.info("Time series client object initialized.");
        jsonMessage.accumulate("Result", "Time series client object initialized.");
        // Initialize time series'
        try {
            agent.initializeTimeSeriesIfNotExist();
        }
        catch (JPSRuntimeException e) {
            LOGGER.error(INITIALIZE_ERROR_MSG,e);
            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
        }

        // Create the connector to interact with the CARESWeatherStation API
        CARESWeatherStationAPIConnector connector;
        try {
            connector = new CARESWeatherStationAPIConnector(args[2]);
        } catch (IOException e) {
            LOGGER.error(CONNECTOR_ERROR_MSG, e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG, e);
        }
        LOGGER.info("API connector object initialized.");
        jsonMessage.accumulate("Result", "API connector object initialized.");

        // Retrieve readings
        JSONObject weatherDataReadings = new JSONObject();

        try {
            weatherDataReadings = connector.getWeatherReadings();
        }
        catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }
        LOGGER.info(String.format("Retrieved %d weather station readings.",
                weatherDataReadings.length()));
        jsonMessage.accumulate("Result", "Retrieved " + weatherDataReadings.getJSONArray("observations").length() +
                " weather station readings.");

        // If readings are not empty there is new data
        if(!weatherDataReadings.isEmpty()) {
            // Update the data
            agent.updateData(weatherDataReadings);
            LOGGER.info("Data updated with new readings from API.");
            jsonMessage.accumulate("Result", "Data updated with new readings from API.");
        }
        // If all are empty no new readings are available
        else if(weatherDataReadings.isEmpty()) {
            LOGGER.info("No new readings are available.");
            jsonMessage.accumulate("Result", "No new readings are available.");
        }

        try {
            WeatherQueryClient weatherQueryClient = new WeatherQueryClient(args[0], args[1], args[2]);
            weatherQueryClient.instantiateIfNotExist(weatherDataReadings);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to carry out queries or insert data into the sparql store!", e);
        }


        return jsonMessage;
    }

    /**
     * Reads the parameters needed for the timeseries client
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
                dbUrl = prop.getProperty("db.url");
            } else {
                throw new IOException("Properties file is missing \"db.url=<db_url>\"");
            }
            if (prop.containsKey("db.user")) {
                dbUser = prop.getProperty("db.user");
            } else {
                throw new IOException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
                dbPassword = prop.getProperty("db.password");
            } else {
                throw new IOException("Properties file is missing \"db.password=<db_password>\"");
            }
            if (prop.containsKey("sparql.query.endpoint")) {
                sparqlQueryEndpoint = prop.getProperty("sparql.query.endpoint");
            } else {
                throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
                sparqlUpdateEndpoint = prop.getProperty("sparql.update.endpoint");
            } else {
                throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
            }
            if (prop.containsKey("sparql.username")) {
                sparqlUser = prop.getProperty("sparql.username");
            }
            if (prop.containsKey("sparql.password")) {
                sparqlPassword = prop.getProperty("sparql.password");
            }
        }
    }
}