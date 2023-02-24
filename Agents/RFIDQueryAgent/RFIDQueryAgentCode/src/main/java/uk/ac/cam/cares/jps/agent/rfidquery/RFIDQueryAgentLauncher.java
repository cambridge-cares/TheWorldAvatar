package uk.ac.cam.cares.jps.agent.rfidquery;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
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
import javax.servlet.http.HttpServletRequest;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;


@WebServlet(urlPatterns = {"/retrieve"})
public class RFIDQueryAgentLauncher extends JPSAgent{
	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(RFIDQueryAgentLauncher.class);
    
    //timeSeries Object
    TimeSeries<OffsetDateTime> timeseries;
    
    /**
     * Parameters to set rdf client and rdb client
     */
	String dbUrl;
	String dbUsername;
	String dbPassword;
	String sparqlQueryEndpoint;
	String sparqlUpdateEndpoint;
    
    //latest timeseries value
    double latestTimeSeriesValue;
    
    //keys in the request's JSON Object
    public static final String KEY_TIMESERIES_CLIENTPROPERTIES = "timeSeriesClientProperties";
    public static final String KEY_DATAIRIS = "dataIRIs";
	public static final String KEY_NUMOFHOURS = "hours";
	public static final String KEY_SPECIES_PROPERTIES = "speciesProperties";
	public static final String KEY_CONTAINSPECIES = "containSpecies";
    
    //set kbClient
    RemoteStoreClient kbClient = new RemoteStoreClient();

	//tsClient
	TimeSeriesClient<OffsetDateTime> tsClient;
    
    //JSONObjects
    JSONObject message;
    
    /**
     * Error messages
     */
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String LOADCONFIGS_ERROR_MSG = "Could not load configs from the properties file!";
    private static final String GETLATESTSTATUSANDCHECKTHRESHOLD_ERROR_MSG = "Unable to query for latest data and/or check RFID Status Threshold!";
    private static final String ARGUMENT_MISMATCH_MSG = "Need five arguments in the following order:1) time series client for timeseries data 2)list of data IRIs 3)Number of hours 4) species sparql endpoints 5)whether tagged object contains some chemical (true or false)";
    private static final String AGENT_ERROR_MSG = "The RFID Query Agent could not be constructed!";

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    } 

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject jsonMessage = new JSONObject();
		String[] args;
		if (validateInput(requestParams)) {
			LOGGER.info("Passing request to Agent..");
        	String timeseriesDataClientProperties = System.getenv(requestParams.getString(KEY_TIMESERIES_CLIENTPROPERTIES));
        	String DataIRIs = requestParams.getString(KEY_DATAIRIS);
			String numOfHours = requestParams.getString(KEY_NUMOFHOURS);
			String speciesProperties = System.getenv(requestParams.getString(KEY_SPECIES_PROPERTIES));
			String containSpecies = requestParams.getString(KEY_CONTAINSPECIES);
            args = new String[] {timeseriesDataClientProperties, DataIRIs, numOfHours, speciesProperties, containSpecies};
            jsonMessage = initializeAgent(args);
            jsonMessage.accumulate("message","POST request has been sent successfully.");
            requestParams = jsonMessage;
		}
		else {
			LOGGER.info("Unable to validate request sent to the agent!");
			throw new JPSRuntimeException("Unable to validate request sent to the agent!");
		}
		return requestParams;
	}
    
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
      boolean validate = true;

      String timeseriesClientProperties;
      String speciesProperties;
      if (requestParams.isEmpty()) {
    	  validate = false;
    	  LOGGER.info("Empty Request!");  
      }
      else {
		validate = requestParams.has(KEY_TIMESERIES_CLIENTPROPERTIES);
		if (validate == true) {
			validate = requestParams.has(KEY_DATAIRIS);
		}
		if (validate == true) {
			validate = requestParams.has(KEY_NUMOFHOURS);
		} if (validate == true) {
			validate = requestParams.has(KEY_CONTAINSPECIES);
		}
		if (validate == true) {
			timeseriesClientProperties =  (requestParams.getString(KEY_TIMESERIES_CLIENTPROPERTIES));
			if (System.getenv(timeseriesClientProperties) == null) {
				validate = false;
				LOGGER.info("Unable to validate location of client.properties!");
			}
		}
		if (validate == true) {
			speciesProperties =  (requestParams.getString(KEY_SPECIES_PROPERTIES));
			if (System.getenv(speciesProperties) == null) {
				validate = false;
				LOGGER.info("Unable to validate location of species.properties!");
			}
		}
		
	}
	return validate;
    }
    
	 /**
     * @param args A String[] {timeseriesDataClientProperties, DataIRIs, numOfHours, speciesProperties, containSpecies}
     */
    public JSONObject initializeAgent(String[] args) {
    	 // Ensure that there are five arguments provided
        if (args.length != 5) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug("Launcher called with the following files: " + String.join(" ", args));
    	
         // Create the agent
		 RFIDQueryAgent agent;

		 try {
			 agent = new RFIDQueryAgent(args[1], args[2]);
		 } catch (IOException e) {
			 LOGGER.error(AGENT_ERROR_MSG, e);
			 throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
		 }
		 LOGGER.info("Input agent object initialized.");

		 JSONObject jsonMessage = new JSONObject();
		 jsonMessage.accumulate("Result", "Input agent object initialized.");
		 
		 try {
			 loadTSClientConfigs(args[0]);
		 } catch (IOException e) {
			 throw new JPSRuntimeException(LOADCONFIGS_ERROR_MSG, e);
		 }
		 
		 RemoteStoreClient kbClient = new RemoteStoreClient();
		 kbClient.setQueryEndpoint(sparqlQueryEndpoint);
		 kbClient.setUpdateEndpoint(sparqlUpdateEndpoint);
		 
		 RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
		 agent.setRDBClient(rdbStoreClient);
		 
		 // Create and set the time series client
		 try {
			 tsClient = new TimeSeriesClient<>(kbClient ,OffsetDateTime.class);
			 agent.setTsClient(tsClient);
		 } catch (JPSRuntimeException e) {
			 LOGGER.error(TSCLIENT_ERROR_MSG, e);
			 throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
		 }

		 LOGGER.info("Time series client object initialized.");
		 jsonMessage.accumulate("Result", "Time series client object initialized.");

		 JSONObject overallResults = new JSONObject();

		 try {
			overallResults = agent.queriesStatusAndCheckTimeStamps();
			LOGGER.info("Queried for latest RFID tag status and checked timestamp threshold.");
			jsonMessage.accumulate("Result", "Queried for latest RFID tag status and checked timestamp threshold.");
			LOGGER.info(overallResults.toString());
		}
		catch (Exception e) {
			LOGGER.error(GETLATESTSTATUSANDCHECKTHRESHOLD_ERROR_MSG, e);
			throw new JPSRuntimeException(GETLATESTSTATUSANDCHECKTHRESHOLD_ERROR_MSG ,e);
		}

		
		//loop through results to get data IRI, exceedThreshold, timestamp
		
		for (int i = 0; i <= overallResults.length() - 1; i++){
			JSONObject a = overallResults.getJSONObject("iri_"+i);
			String timestamp = a.getString("timestamp");
			Boolean exceedThreshold = a.getBoolean("exceedThreshold");
			String dataIRI = a.getString("dataIRI");

			LOGGER.info("exceedThreshold for " + dataIRI + " is " + exceedThreshold);

			if (exceedThreshold == true && args.length > 3 && args[4].split(",")[i].contains("true")){
				LOGGER.info("Beginning queries...");
				//Create RFIDQueryBuilder
				RFIDQueryBuilder builder ;
				try {
					builder = new RFIDQueryBuilder(args[0], args[3]);
				} catch (IOException e) {
					throw new JPSRuntimeException(LOADCONFIGS_ERROR_MSG, e);
				}

				//query for tag IRI with state IRI via saref:hasState
				String tagIRI = builder.queryForTagWithStateIRI(dataIRI);
				LOGGER.info("The tag IRI retrieved is " + tagIRI);

				//query for bottle IRI with tag IRI via ontodevice:isAttachedTo
				String bottleIRI = builder.queryForTaggedObjectWithIsAttachedTo(tagIRI);
				LOGGER.info("The bottle IRI retrieved is " + bottleIRI);

			    //query for bottle label via rdfs:label
				String objectLabel = builder.queryForTaggedObjectLabel(bottleIRI);
				LOGGER.info("The label of the tagged object is " + objectLabel);

				//query for chemical amount IRI with bottle IRI via ontolab:isFilledWith
				String chemicalAmountIRI = builder.queryForChemicalAmountWithIsFilledWith(bottleIRI);
				LOGGER.info("The chemical amount IRI retrieved is " + chemicalAmountIRI);

				//query for chemical IRI with chemical amount IRI via ontocape_cps_behavior:refersToMaterial
				String chemicalIRI = builder.queryForChemicalWithRefersToMaterial(chemicalAmountIRI);
				LOGGER.info("The material IRI retrieved is " + chemicalIRI);

				String speciesIRI;
				if (builder.queryForChemicalComponentWithIntrinsicCharacteristics(chemicalIRI) != null){
					speciesIRI = builder.queryForChemicalComponentWithIntrinsicCharacteristics(chemicalIRI);
				} else {
				//query for phase IRI with chemical IRI via ontocape_material:thermodynamicBehavior
				String phaseIRI = builder.queryForPhaseWithThermodynamicBehavior(chemicalIRI);
				LOGGER.info("The phase IRI retrieved is " + phaseIRI);

				//query for phase component IRI with phase IRI via ontocape_system:isComposedOfSubsystem
				String phaseComponentIRI = builder.queryForPhaseComponentWithIsComposedOfSubsystem(phaseIRI);
				LOGGER.info("The phase component IRI retrieved is " + phaseComponentIRI);

				//query for species IRI with phase component IRI via ontocape_phase_system:representsOccurenceOf
				speciesIRI = builder.queryForSpeciesWithRepresentsOccurenceOf(phaseComponentIRI);
				LOGGER.info("The species IRI retrieved is " + speciesIRI);
				}

				//query for species label via rdfs:label
				String speciesLabel = builder.queryForSpeciesLabel(speciesIRI);
				LOGGER.info("The label of the species is " + speciesLabel);

				//query for hazard statement IRIs via ontospecies:hasGHSHazardStatements
				JSONArray GHSHazardStatements = builder.queryForGHSHazardStatements(speciesIRI);

				//query for the label and comment of each hazard statement IRI and put them in a hash map
				Map<String, List<String>> map = builder.queryForLabelAndCommentForGHSHazardStatements(GHSHazardStatements);
				
				LOGGER.info("Preparing to send email...");
				agent.sendEmail(dataIRI, objectLabel, speciesLabel, timestamp, map);
				LOGGER.info("Alert Email sent for " + dataIRI);
				jsonMessage.accumulate("Result", "Alert Email sent for " + dataIRI);

			} else if (exceedThreshold == true) {
				LOGGER.info("Beginning queries...");
				//Create RFIDQueryBuilder
				RFIDQueryBuilder builder ;
				try {
					builder = new RFIDQueryBuilder(args[0], args[3]);
				} catch (IOException e) {
					throw new JPSRuntimeException(LOADCONFIGS_ERROR_MSG, e);
				}

				//query for tag IRI with state IRI via saref:hasState
				String tagIRI = builder.queryForTagWithStateIRI(dataIRI);
				LOGGER.info("The tag IRI retrieved is " + tagIRI);

				//query for tagged object IRI with tag IRI via ontodevice:isAttachedTo
				String taggedObjectIRI = builder.queryForTaggedObjectWithIsAttachedTo(tagIRI);
				LOGGER.info("The bottle IRI retrieved is " + taggedObjectIRI);

				//query for tagged object label via rdfs:label
				String label = builder.queryForTaggedObjectLabel(taggedObjectIRI);
				LOGGER.info("The label of the tagged object is " + label);

				LOGGER.info("Preparing to send email...");
				agent.sendEmail(dataIRI, label, null, timestamp, null);
				LOGGER.info("Alert Email sent for " + dataIRI);
				jsonMessage.accumulate("Result", "Alert Email sent for " + dataIRI);
			}
		}
		 return jsonMessage;
	}


	/**
     * Reads the username, password, sparql endpoint from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the username, password, sparql endpoints
     */
    public void loadTSClientConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
		
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
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
	        	kbClient.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
	        } else {
	        	throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
	        }
	        if (prop.containsKey("sparql.update.endpoint")) {
	        	kbClient.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
	        } else {
	        	throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
	        }
        }
        }
    }

