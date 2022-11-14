package uk.ac.cam.cares.jps.agent.thingspeak;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
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
 * @author 
 */
@WebServlet(urlPatterns = {"/retrieve"})
public class ThingspeakInputAgentLauncher extends JPSAgent {
	
	public static final String KEY_AGENTPROPERTIES = "agentProperties";
	public static final String KEY_APIPROPERTIES = "apiProperties";
	public static final String KEY_CLIENTPROPERTIES = "clientProperties";
	
	
	 String agentProperties;
	 String apiProperties;
	 String clientProperties;
	 
	 String dbUrl;
	 String dbUsername;
	 String dbPassword;
	 String sparqlQueryEndpoint;
	 String sparqlUpdateEndpoint;
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ThingspeakInputAgentLauncher.class);
    /**
     * Logging / error messages
     */
    private static final String ARGUMENT_MISMATCH_MSG = "Need three properties files in the following order: 1) input agent 2) time series client 3) API connector.";
    private static final String AGENT_ERROR_MSG = "The Thingspeak input agent could not be constructed!";
    private static final String LOADCONFIG_ERROR_MSG = "Unable to load timeseries client parameters from properties file!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String CONNECTOR_ERROR_MSG = "Could not construct the Thingspeak API connector needed to interact with the API!";
    private static final String GET_READINGS_ERROR_MSG = "Some readings could not be retrieved.";

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    } 
    
    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject jsonMessage = new JSONObject();
      if (validateInput(requestParams)) {
        	LOGGER.info("Passing request to Thingspeak Input Agent..");
            String agentProperties = System.getenv(requestParams.getString(KEY_AGENTPROPERTIES));
            String clientProperties = System.getenv(requestParams.getString(KEY_CLIENTPROPERTIES));
            String apiProperties = System.getenv(requestParams.getString(KEY_APIPROPERTIES));
            String[] args = new String[] {agentProperties,clientProperties,apiProperties};
            jsonMessage = initializeAgent(args);
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
    
 // TODO: Use proper argument parsing
    /**
     * Main method that runs through all steps to update the data received from the Thingspeak API.
     * defined in the provided properties file.
     * @param args The command line arguments. Three properties files should be passed here in order: 1) input agent
     *             2) time series client 3) API connector.
     */
    
    public JSONObject initializeAgent(String[] args) {

        // Ensure that there are three properties files
        if (args.length != 3) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug("Launcher called with the following files: " + String.join(" ", args));

        // Create the agent
        ThingspeakInputAgent agent;
        try {
            agent = new ThingspeakInputAgent(args[0]);
        } catch (IOException e) {
            LOGGER.error(AGENT_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
        }
        LOGGER.info("Input agent object initialized.");
        JSONObject jsonMessage = new JSONObject();
        jsonMessage.accumulate("Result", "Input agent object initialized.");
        
        try {
			loadTSClientConfigs(args[1]);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			throw new JPSRuntimeException(LOADCONFIG_ERROR_MSG, e);
		}
        
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint(sparqlQueryEndpoint);
        kbClient.setUpdateEndpoint(sparqlUpdateEndpoint);
        TimeSeriesClient<OffsetDateTime> tsClient;
        
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
        agent.setRDBClient(rdbStoreClient);
        /*
        // Create and set the time series client
        TimeSeriesClient<OffsetDateTime> tsClient;
        */
        try {
            //tsClient = new TimeSeriesClient<>(OffsetDateTime.class, args[1]);
        	tsClient = new TimeSeriesClient<>(kbClient ,OffsetDateTime.class);
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
        catch (Exception e) {
            LOGGER.error(INITIALIZE_ERROR_MSG,e);
            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
        }

        // Create the connector to interact with the Thingspeak API
        ThingspeakAPIConnector connector;
        try {
            connector = new ThingspeakAPIConnector(args[2]);
        } catch (IOException e) {
            LOGGER.error(CONNECTOR_ERROR_MSG, e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG, e);
        }
        LOGGER.info("API connector object initialized.");
        jsonMessage.accumulate("Result", "API connector object initialized.");

        // Retrieve readings
        JSONObject Readings;
        
        try {
            Readings = connector.getAllReadings();
        }
        catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }
        LOGGER.info(String.format("Retrieved %d readings.",
                Readings.length()));
        jsonMessage.accumulate("Result", "Retrieved " + Readings.length() +  
        		" readings.");
        // If readings are not empty there is new data
        if(!Readings.isEmpty()) {
            // Update the data
            agent.updateData(Readings);
            LOGGER.info("Data updated with new readings from API.");
            jsonMessage.accumulate("Result", "Data updated with new readings from API.");
            jsonMessage.accumulate("Result", "Timeseries Data has been updated.");
        }
        // If all are empty no new readings are available
        else if(Readings.isEmpty()) {
            LOGGER.info("No new readings are available.");
            jsonMessage.accumulate("Result", "No new readings are available.");
        }
		return jsonMessage;
    }
    
    /**
     * Reads the parameters needed to connect to the API from a properties file and saves it in fields.
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

        }
}

}
