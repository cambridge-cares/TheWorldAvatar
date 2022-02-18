package uk.ac.cam.cares.jps.agent.esphome;

import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.time.*;
import java.util.*;
import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


@WebServlet(urlPatterns = {"/toggle"})
public class ESPHomeAgent extends JPSAgent{
	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ESPHomeAgent.class);

    /**
     * The time series client to interact with the knowledge graph and data storage
     */
    private TimeSeriesClient<OffsetDateTime> tsClient;
    
    //timeSeries Object
    TimeSeries<OffsetDateTime> timeseries;
    
    /**
     * Parameters to set rdf client and rdb client
     */
    private String dbUrl;
    private String dbUser;
    private String dbPassword;
    
    //latest timeseries value
    double latestTimeSeriesValue;
    
    //
    public static final String KEY_CLIENTPROPERTIES = "clientProperties";
	public static final String KEY_APIPROPERTIES = "apiProperties";
    
    //data IRI
    private String dataIRI;
    
    //data values
    private List<Double> dataValues;
    
    //timeseries IRI
    private String timeSeriesIRI;
    
    //set kbClient
    RemoteStoreClient kbClient = new RemoteStoreClient();
    
    //JSONObjects
    JSONObject message;
    JSONObject result;
    
    /**
     * Error messages
     */
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String GETLATESTDATA_ERROR_MSG = "Unable to query for latest data!" ;
    private static final String LOADCONFIGS_ERROR_MSG = "Could not load RDB, SPARQL and ESPHome configs from the properties file!";
    private static final String ESPHOME_ERROR_MSG = "Unable to send request to ESPHome web server!";
    private static final String GETDBURL_ERROR_MSG = "Unable to query for db url from this timeseries IRI!";
    private static final String GETTIMESERIESIRI_ERROR_MSG = "Unable to query for timeseries IRI from this data IRI!";
    private static final String ESPHOMEAPI_ERROR_MSG = "Could not construct ESPHomeAPI needed to send POST and GET request to the ESPHome web server.";
    private static final String ESPHOME_CHECKSTATUS_ERROR_MSG = "Could not establish connection with ESPHome web server to check status.";
    private static final String ARGUMENT_MISMATCH_MSG = "Need two properties files in the following order:1) time series client 2) API.";
    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject jsonMessage = new JSONObject();
      if (validateInput(requestParams)) {
        	LOGGER.info("Passing request to ESPHome Agent..");
        	String clientProperties = System.getenv(requestParams.getString(KEY_CLIENTPROPERTIES));
        	String apiProperties = System.getenv(requestParams.getString(KEY_APIPROPERTIES));
            String[] args = new String[] {clientProperties,apiProperties};
            jsonMessage = initializeAgent(args);
            jsonMessage.accumulate("message","POST request has been sent successfully.");
            requestParams = jsonMessage;
            }
      else {
    	  jsonMessage.accumulate("message","Unable to validate request sent to the agent.");
    	  requestParams = jsonMessage;
      }
	return requestParams;
}
    
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
      boolean validate = true;
     
      String apiProperties;
      String clientProperties;
      
      if (requestParams.isEmpty()) {
    	  validate = false;
    	  LOGGER.info("Empty Request!");
    	  
      }
      else {
 		 validate = requestParams.has(KEY_CLIENTPROPERTIES);
 		 if (validate == true) {
 	 		 validate = requestParams.has(KEY_APIPROPERTIES);
 	 		 }
 	 		 if (validate == true) {
 	 		 clientProperties =  (requestParams.getString(KEY_CLIENTPROPERTIES));
 	 		 apiProperties = (requestParams.getString(KEY_APIPROPERTIES));
 	 		if (System.getenv(apiProperties) == null) {
 	 			validate = false;
 	 			LOGGER.info("Environment variable is not assigned to a API properties file!");
 	 		 }
 	 		if (System.getenv(clientProperties) == null) {
 	 			validate = false;
 	 			LOGGER.info("Environment variable is not assigned to a client properties file!");
 	 		 }
 		 }
 	 }
	return validate;
    }
    
    public JSONObject initializeAgent(String[] args) {
    	
    	 // Ensure that there are two properties files
        if (args.length != 2) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug("Launcher called with the following files: " + String.join(" ", args));
    	
    	try {
    		//get db.user, db.password, sparql endpoints, data IRI from properties file
			loadTsClientProperties(args[0]);
			
		} catch (IOException | JPSRuntimeException e) {
			throw new JPSRuntimeException(LOADCONFIGS_ERROR_MSG, e);
		}
    	//set timeseriesClient to only query from the kb
    	tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, null, dbUser, dbPassword);
    	
    	try {
		timeSeriesIRI = tsClient.getTimeSeriesIRI(dataIRI);
		if (timeSeriesIRI == null) {
				throw new JPSRuntimeException(GETTIMESERIESIRI_ERROR_MSG);
			}
    	} catch (JSONException e) {
			throw new JPSRuntimeException(GETTIMESERIESIRI_ERROR_MSG);
		}
		try {
    	dbUrl = tsClient.getDbUrl(timeSeriesIRI);
    	if (dbUrl == null) {
			throw new JPSRuntimeException(GETDBURL_ERROR_MSG);
		}
		} catch (JSONException e) {
			throw new JPSRuntimeException(GETDBURL_ERROR_MSG);
		}
    	
    	//set tsClient to query for latest timeseries data
    	try {
    		tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, dbUrl, dbUser, dbPassword);
			
		} catch (JPSRuntimeException e) {
            LOGGER.error(TSCLIENT_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);    	
		}
    	
    	//retrieve latest data with dataIRI
    	try {
    	timeseries = tsClient.getLatestData(dataIRI);
    	} catch (JPSRuntimeException e) {
    		throw new JPSRuntimeException(GETLATESTDATA_ERROR_MSG, e);
    	}
    		
    	//process timeseries object and convert to a suitable form, if data exceeds a certain value, send POST request to ESPHome
    	dataValues = timeseries.getValuesAsDouble(dataIRI);
    	latestTimeSeriesValue = dataValues.get(dataValues.size() - 1);
    	
    	ESPHomeAPI esphomeApi;
    	boolean state;
		try {
			esphomeApi = new ESPHomeAPI(args[1]);
		} catch (IOException e) {
			throw new JPSRuntimeException(ESPHOMEAPI_ERROR_MSG, e);
		}
		
		try {
			state = esphomeApi.checkStatus();
		} catch (JSONException | IOException e) {
			throw new JPSRuntimeException(ESPHOME_CHECKSTATUS_ERROR_MSG, e);
		}
		
    	try {
		result = esphomeApi.esphomeSwitchControl(latestTimeSeriesValue, state);
		result.accumulate("message","A request has been successfully sent to the ESPHome web server.");
		} catch (JPSRuntimeException | NullPointerException e) {
			throw new JPSRuntimeException(ESPHOME_ERROR_MSG, e);
		}
		return result;
    }

    


	/**
     * Reads the username, password, sparql endpoint from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the username, password, sparql endpoints
     */
    public void loadTsClientProperties(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            if (prop.containsKey("db.user")) {
                this.dbUser = prop.getProperty("db.user");
            } else {
                throw new IOException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
                this.dbPassword = prop.getProperty("db.password");
            } else {
                throw new IOException("Properties file is missing \"db.password=<db_password>\"");
            }
            if (prop.containsKey("sparql.query.endpoint")) {
	        	kbClient.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
	        } else {
	        	throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\" ");
	        }
	        if (prop.containsKey("sparql.update.endpoint")) {
	        	kbClient.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
	        } else {
	        	throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\" ");
	        }
	        if (prop.containsKey("dataIRI")) {
	        	this.dataIRI = prop.getProperty("dataIRI");
	        } else {
	        	throw new IOException("Properties file is missing \"dataIRI=<data_IRI>\"");
	        }
        }
        }
    }

