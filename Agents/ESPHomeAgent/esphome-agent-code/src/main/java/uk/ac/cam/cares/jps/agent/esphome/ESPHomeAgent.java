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
    public static final String KEY_TIMESERIESDATA_CLIENTPROPERTIES = "timeseriesDataClientProperties";
	public static final String KEY_ESPHOMESTATUS_CLIENTPROPERTIES = "esphomeStatusClientProperties";
	public static final String KEY_ESPHOME_APIPROPERTIES = "esphomeAPIProperties";
    
    //data IRI
    private String dataIRI;
    
    //data values
    private List<Double> dataValuesAsDouble;
    private List<String> dataValuesAsString;
    
    //timeseries IRI
    private String timeseriesIRI;
    
    //set kbClient
    RemoteStoreClient kbClient = new RemoteStoreClient();
    
    //JSONObjects
    JSONObject message;
    JSONObject result;

	//ESPHomeAPI
	ESPHomeAPI esphomeApi;

	//QueryBuilder
	QueryBuilder queryBuilder;
    
    /**
     * Error messages
     */
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String GETLATESTDATA_ERROR_MSG = "Unable to query for latest data!" ;
    private static final String LOADCONFIGS_ERROR_MSG = "Could not load RDB, SPARQL and ESPHome configs from the properties file!";
    private static final String ESPHOME_ERROR_MSG = "Unable to send request to ESPHome web server!";
    private static final String GETDBURL_ERROR_MSG = "Unable to query for db url from this timeseries IRI!";
    private static final String GETTIMESERIESIRI_ERROR_MSG = "Unable to query for timeseries IRI from this data IRI!";
    private static final String ARGUMENT_MISMATCH_MSG = "Need three properties files in the following order:1) time series client for timeseries data 2) time series client for esphome status 3)esphome API properties";
    private static final String ESPHOMEAPI_ERROR_MSG = "Could not construct ESPHomeAPI needed to send POST requests to the ESPHome web server.";
    private static final String QUERYBUILDER_ERROR_MSG = "Could not construct query builder needed to construct and send queries to the knowledge graph.";
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject jsonMessage = new JSONObject();
      if (validateInput(requestParams)) {
        	LOGGER.info("Passing request to ESPHome Agent..");
        	String timeseriesDataClientProperties = System.getenv(requestParams.getString(KEY_TIMESERIESDATA_CLIENTPROPERTIES));
        	String esphomeStatusClientProperties = System.getenv(requestParams.getString(KEY_ESPHOMESTATUS_CLIENTPROPERTIES));
        	String esphomeAPIProperties = System.getenv(requestParams.getString(KEY_ESPHOME_APIPROPERTIES));
            String[] args = new String[] {timeseriesDataClientProperties,esphomeStatusClientProperties, esphomeAPIProperties};
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
     
      String esphomeStatusClientProperties;
      String timeseriesDataClientProperties;
      String esphomeAPIProperties;
      
      if (requestParams.isEmpty()) {
    	  validate = false;
    	  LOGGER.info("Empty Request!");
    	  
      }
      else {
 		 validate = requestParams.has(KEY_TIMESERIESDATA_CLIENTPROPERTIES);
 		 if (validate == true) {
 	 		 validate = requestParams.has(KEY_ESPHOMESTATUS_CLIENTPROPERTIES);
 	 		 }
 	 		 if (validate == true) {
 	 			validate = requestParams.has(KEY_ESPHOME_APIPROPERTIES);
 	 		 }
 	 		 if (validate == true) {
 	 		 timeseriesDataClientProperties =  (requestParams.getString(KEY_TIMESERIESDATA_CLIENTPROPERTIES));
 	 		 esphomeStatusClientProperties = (requestParams.getString(KEY_ESPHOMESTATUS_CLIENTPROPERTIES));
 	 		 esphomeAPIProperties = (requestParams.getString(KEY_ESPHOME_APIPROPERTIES));
 	 		if (System.getenv(esphomeStatusClientProperties) == null) {
 	 			validate = false;
 	 			LOGGER.info("Environment variable is not assigned to the client properties file for esphome status!");
 	 		 }
 	 		if (System.getenv(timeseriesDataClientProperties) == null) {
 	 			validate = false;
 	 			LOGGER.info("Environment variable is not assigned to the client properties file for timeseries data!");
 	 		 }
 	 		if (System.getenv(esphomeAPIProperties) == null) {
 	 			validate = false;
 	 			LOGGER.info("Environment variable is not assigned to the esphome api properties file!");
 	 		 }
 		 }
 	 }
	return validate;
    }
    
    public JSONObject initializeAgent(String[] args) {
    	
    	 // Ensure that there are three properties files
        if (args.length != 3) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug("Launcher called with the following files: " + String.join(" ", args));
    	
    	try {
    		//get db.user, db.password, sparql endpoints, data IRI from timeseries data client properties file
			loadTsClientProperties(args[0]);
			
		} catch (IOException | JPSRuntimeException e) {
			throw new JPSRuntimeException(LOADCONFIGS_ERROR_MSG, e);
		}
    	//set timeseriesClient to only query from the kb
    	tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, null, dbUser, dbPassword);
    	//query for timeseries IRI linked to data IRI
    	try {
    		timeseriesIRI = tsClient.getTimeSeriesIRI(dataIRI);
		if (timeseriesIRI == null) {
				throw new JPSRuntimeException(GETTIMESERIESIRI_ERROR_MSG);
			}
    	} catch (JSONException e) {
			throw new JPSRuntimeException(GETTIMESERIESIRI_ERROR_MSG);
		}
    	//query for db URL linked to timeseries IRI
		try {
    	dbUrl = tsClient.getDbUrl(timeseriesIRI);
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
    		
    	//process timeseries object and convert to a suitable form, retrieve values only
    	dataValuesAsDouble = timeseries.getValuesAsDouble(dataIRI);
    	latestTimeSeriesValue = dataValuesAsDouble.get(dataValuesAsDouble.size() - 1);
    	LOGGER.info("The latest timeseries value is " + latestTimeSeriesValue);
    	
    	String status;
    	
    	try {
    		//get db.user, db.password, sparql endpoints, data IRI from esphome status client properties file
			loadTsClientProperties(args[1]);
			
		} catch (IOException | JPSRuntimeException e) {
			throw new JPSRuntimeException(LOADCONFIGS_ERROR_MSG, e);
		}
    	//set timeseriesClient to only query from the kb
    	tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, null, dbUser, dbPassword);
    	//query for timeseries IRI linked to data IRI
    	try {
    		timeseriesIRI = tsClient.getTimeSeriesIRI(dataIRI);
		if (timeseriesIRI == null) {
				throw new JPSRuntimeException(GETTIMESERIESIRI_ERROR_MSG);
			}
    	} catch (JSONException e) {
			throw new JPSRuntimeException(GETTIMESERIESIRI_ERROR_MSG);
		}
    	//query for db URL linked to timeseries IRI
		try {
    	dbUrl = tsClient.getDbUrl(timeseriesIRI);
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
    	
    	//retrieve latest status of the component with dataIRI
    	try {
    	timeseries = tsClient.getLatestData(dataIRI);
    	} catch (JPSRuntimeException e) {
    		throw new JPSRuntimeException(GETLATESTDATA_ERROR_MSG, e);
    	}
    		
    	//process timeseries object and convert to a suitable form, retrieve values only
    	dataValuesAsString = timeseries.getValuesAsString(dataIRI);
    	status = dataValuesAsString.get(0);
    	LOGGER.info("The current status of the component is " + status);

		//query for threshold from KG based on ontodevice
		double esphomeThreshold;

		try {
		queryBuilder = new QueryBuilder(args[1]);
		} catch (Exception e) {
			throw new JPSRuntimeException(QUERYBUILDER_ERROR_MSG, e);
		}

		String queryResult = queryBuilder.queryForDeviceWithStateIRI(dataIRI);
		queryResult = queryBuilder.queryForSetpointWithHasSetpoint(queryResult);
		queryResult = queryBuilder.queryForQuantityWithHasQuantity(queryResult);
		queryResult = queryBuilder.queryForMeasureWithHasValue(queryResult);
		esphomeThreshold = queryBuilder.queryForNumericalValueWithHasNumericalValue(queryResult);

    	//set ESPHome API with properties from ESPHome API properties file
		try {
			esphomeApi = new ESPHomeAPI(args[2]);
		} catch (IOException e) {
			throw new JPSRuntimeException(ESPHOMEAPI_ERROR_MSG, e);
		}
		//determine whether to turn the component on or off
    	try {
		result = esphomeApi.esphomeSwitchControl(latestTimeSeriesValue, status, esphomeThreshold);
		LOGGER.info("A request has been successfully sent to the ESPHome web server.");
		result.accumulate("message","A request has been successfully sent to the ESPHome web server.");
		} catch (JPSRuntimeException e) {
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

