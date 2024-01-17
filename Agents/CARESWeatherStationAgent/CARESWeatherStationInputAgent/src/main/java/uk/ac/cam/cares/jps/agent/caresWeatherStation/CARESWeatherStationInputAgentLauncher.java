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
@WebServlet(urlPatterns = {"/retrieve"})
public class CARESWeatherStationInputAgentLauncher extends JPSAgent {

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

    public static String GEOSERVER_WORKSPACE = System.getenv("GEOSERVER_WORKSPACE");
	public static String DATABASE = System.getenv("DATABASE");
	public static String LAYERNAME = System.getenv("LAYERNAME");

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        if (validateInput(requestParams)) {
            LOGGER.info("Passing request to CARES Weather Station Input Agent..");
            String agentProperties = System.getenv(requestParams.getString(KEY_AGENTPROPERTIES));
            String clientProperties = System.getenv(requestParams.getString(KEY_CLIENTPROPERTIES));
            String apiProperties = System.getenv(requestParams.getString(KEY_APIPROPERTIES));
            try {
            loadTSClientConfigs(clientProperties);
            } catch (IOException e) {
                throw new JPSRuntimeException(LOADTSCONFIG_ERROR_MSG, e);
            }
            String[] args = new String[] {agentProperties,clientProperties,apiProperties};
            jsonMessage = initializeAgent(args);
            jsonMessage.accumulate("Result", "Timeseries Data has been updated.");
            requestParams = jsonMessage;
        }
        else {
            jsonMessage.put("Result", "Request parameters are not defined correctly.");
            requestParams = jsonMessage;
        }
        return requestParams;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        boolean validate = true;
        String agentProperties;
        String apiProperties;
        String clientProperties;

        if (requestParams.isEmpty()) {
            validate = false;
        }
        else {
            validate = requestParams.has(KEY_AGENTPROPERTIES);
            if (validate == true) {
                validate = requestParams.has(KEY_CLIENTPROPERTIES);
            }
            if (validate == true) {
                validate = requestParams.has(KEY_APIPROPERTIES);
            }
            if (validate == true) {
                agentProperties = (requestParams.getString(KEY_AGENTPROPERTIES));
                clientProperties =  (requestParams.getString(KEY_CLIENTPROPERTIES));
                apiProperties = (requestParams.getString(KEY_APIPROPERTIES));

                if (System.getenv(agentProperties) == null) {
                    validate = false;

                }
                if (System.getenv(apiProperties) == null) {
                    validate = false;

                }
                if (System.getenv(clientProperties) == null) {
                    validate = false;

                }
            }
        }
        return validate;
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