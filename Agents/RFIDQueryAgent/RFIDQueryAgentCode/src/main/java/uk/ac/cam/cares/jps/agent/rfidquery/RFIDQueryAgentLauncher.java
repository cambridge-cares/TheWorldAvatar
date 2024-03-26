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
import java.text.SimpleDateFormat;
import java.time.*;
import java.util.*;
import javax.servlet.annotation.WebServlet;
import org.apache.logging.log4j.LogManager;
import javax.servlet.http.HttpServletRequest;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;


@WebServlet(urlPatterns = {"/check, /retrieveData, /sendNotification"})
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
	String sparqlUsername;
	String sparqlPassword;
    
    //latest timeseries value
    double latestTimeSeriesValue;
    
    //keys in the request's JSON Object
    private static final String KEY_TIMESERIES_CLIENTPROPERTIES = "timeSeriesClientProperties";
    private static final String KEY_DATAIRIS = "dataIRIs";
	private static final String KEY_DATAIRI = "dataIRI";
	private static final String KEY_LATESTSTATUS = "latestStatus";
	private static final String KEY_TAGGEDOBJECTIRI = "taggedObjectIRI";
	private static final String KEY_NUMOFHOURS = "hours";
	private static final String KEY_SPECIES_PROPERTIES = "speciesProperties";
	private static final String KEY_CONTAINSPECIES = "containSpecies";
    
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
    private static final String CHECK_ARGUMENT_MISMATCH_MSG = "Need four arguments in the following order:1) time series client for timeseries data 2)list of data IRIs 3)Number of hours 4) species sparql endpoints";
	private static final String SEND_NOTIF_ARGUMENT_MISMATCH_MSG = "Need four arguments in the following order:1) time series client for timeseries data 2)data 3)IRI latest status 4) species sparql endpoints";
    private static final String AGENT_ERROR_MSG = "The RFID Query Agent could not be constructed!";
	private static final String QUERYBUILDER_ERROR_MSG = "The RFID Query Builder could not be constructed!";

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	JSONObject jsonMessage = new JSONObject();
		LOGGER.info("Passing request to Agent..");
		String originalUrl = request.getRequestURI();
        jsonMessage = initializeAgent(requestParams, originalUrl);
		return jsonMessage;
	}

	/**
	 * Start agent and run the relevant route based on the url
	 * @param requestParams request parameters
	 * @param originalUrl url used to determine the route to execute
	 * @return a JSONObject containing the results
	 */
    public JSONObject initializeAgent(JSONObject requestParams, String originalUrl) {
		String[] args;
		JSONObject result = new JSONObject();
		String url = originalUrl.substring(originalUrl.lastIndexOf("/"), originalUrl.length());
		switch (url) {
			case "/check": {
				try {
					String timeseriesDataClientProperties = System.getenv(requestParams.getString(KEY_TIMESERIES_CLIENTPROPERTIES));
        			String DataIRIs = requestParams.getString(KEY_DATAIRIS);
					String numOfHours = requestParams.getString(KEY_NUMOFHOURS);
					String speciesProperties = System.getenv(requestParams.getString(KEY_SPECIES_PROPERTIES));
            		args = new String[] {timeseriesDataClientProperties, DataIRIs, numOfHours, speciesProperties};
				} catch (JSONException e) {
					throw new JSONException("missing input parameters!", e);
				}
				result = executeCheck(args);
				break;
			}
			case "/retrieveData": {
				try {
					String timeseriesDataClientProperties = System.getenv(requestParams.getString(KEY_TIMESERIES_CLIENTPROPERTIES));
        			String taggedObjectIRI = requestParams.getString(KEY_TAGGEDOBJECTIRI);
					String speciesProperties = System.getenv(requestParams.getString(KEY_SPECIES_PROPERTIES));
            		args = new String[] {timeseriesDataClientProperties, taggedObjectIRI, speciesProperties};
				} catch (JSONException e) {
					throw new JSONException("missing input parameters!", e);
				}
				result = executeRetrieveData(args);
				break;
			}
			case "/sendNotification": {
				try {
					String latestStatus = requestParams.getString(KEY_LATESTSTATUS);
					String timeseriesDataClientProperties = System.getenv(requestParams.getString(KEY_TIMESERIES_CLIENTPROPERTIES));
        			String dataIRI = requestParams.getString(KEY_DATAIRI);
					String speciesProperties = System.getenv(requestParams.getString(KEY_SPECIES_PROPERTIES));
            		args = new String[] {timeseriesDataClientProperties, dataIRI, latestStatus, speciesProperties};
				} catch (JSONException e) {
					throw new JSONException("missing input parameters!", e);
				}
				result = executeSendNotification(args);
				break;
			}
		}
		return result;
	}

	/**
	 * Execute check route
	 * @param args the arguments needed to execute the check route
	 * @return result of check route
	 */
	private JSONObject executeCheck(String[] args) {
		// Ensure that there are four arguments provided
        if (args.length != 4) {
            LOGGER.error(CHECK_ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(CHECK_ARGUMENT_MISMATCH_MSG);
        }
		// Create the agent
		RFIDQueryAgent agent;
		try {
			agent = new RFIDQueryAgent(args[1], args[2]);
		} catch (IOException e) {
			LOGGER.error(AGENT_ERROR_MSG, e);
			throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
		}
		LOGGER.info("Input agent initialized.");

		JSONObject jsonMessage = new JSONObject();
		jsonMessage.accumulate("Result", "Input agent initialized.");
		 
		try {
			loadTSClientConfigs(args[0]);
		} catch (IOException e) {
			throw new JPSRuntimeException(LOADCONFIGS_ERROR_MSG, e);
		}
		 
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

			LOGGER.info("Beginning queries...");
			//Create RFIDQueryBuilder
			RFIDQueryBuilder builder ;
			try {
				builder = new RFIDQueryBuilder(args[0], args[3]);
			} catch (IOException e) {
				throw new JPSRuntimeException(QUERYBUILDER_ERROR_MSG,e);
			}

			//query for tag IRI with state IRI via saref:hasState
			String tagIRI = builder.queryForTagWithStateIRI(dataIRI);
			LOGGER.info("The tag IRI retrieved is " + tagIRI);

			//query for tagged object IRI with tag IRI via ontodevice:isAttachedTo
			String taggedObjectIRI = builder.queryForTaggedObjectWithIsAttachedTo(tagIRI);
			LOGGER.info("The bottle IRI retrieved is " + taggedObjectIRI);

			//query for bottle label via rdfs:label
			String objectLabel = builder.queryForTaggedObjectLabel(taggedObjectIRI);
			LOGGER.info("The label of the tagged object is " + objectLabel);

			//query for chemical amount IRI with bottle IRI via ontolab:isFilledWith
			String chemicalAmountIRI = builder.queryForChemicalAmountWithIsFilledWith(taggedObjectIRI);
			if (!chemicalAmountIRI.contains("This tagged object does not contain any chemicals")){
				LOGGER.info("The chemical amount IRI retrieved is " + chemicalAmountIRI);

				//query for chemical IRI with chemical amount IRI via ontocape_cps_behavior:refersToMaterial
				String chemicalIRI = builder.queryForChemicalWithRefersToMaterial(chemicalAmountIRI);
				LOGGER.info("The material IRI retrieved is " + chemicalIRI);

				String speciesIRI;
				//check whether there are IRIs instantiated via ontocape_material:intrinsicCharacteristics
				//If so continue the queries to retrieve the chemicalComponent IRI which is equivalent to the species IRI
				if (builder.queryForChemicalComponent(chemicalIRI) != null){
					speciesIRI = builder.queryForChemicalComponent(chemicalIRI);
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
				agent.sendExceedThresholdEmail(dataIRI, objectLabel, speciesLabel, timestamp, map);
				LOGGER.info("Alert Email sent for " + dataIRI);
				jsonMessage.accumulate("Result", "Alert Email sent for " + dataIRI);
			} else {
				LOGGER.info("Preparing to send email...");
				agent.sendExceedThresholdEmail(dataIRI, objectLabel, null, timestamp, null);
				LOGGER.info("Alert Email sent for " + dataIRI);
				jsonMessage.accumulate("Result", "Alert Email sent for " + dataIRI);
			}
		}
		 return jsonMessage;
	}

	/**
	 * Execute retrieve data route
	 * @param args the arguments needed to execute the retrieve data route
	 * @return result of retrieve data route
	 */
	private JSONObject executeRetrieveData(String[] args) {
		JSONObject result = new JSONObject();
		// Create the agent
		RFIDQueryAgent agent;
		try {
			agent = new RFIDQueryAgent();
		} catch (IOException e) {
			LOGGER.error(AGENT_ERROR_MSG, e);
			throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
		}
		LOGGER.info("Input agent initialized.");

		JSONObject jsonMessage = new JSONObject();
		jsonMessage.accumulate("Result", "Input agent initialized.");
		 
		try {
			loadTSClientConfigs(args[0]);
		} catch (IOException e) {
			throw new JPSRuntimeException(LOADCONFIGS_ERROR_MSG, e);
		}
		 
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

		//Create RFIDQueryBuilder
		RFIDQueryBuilder builder ;
		try {
			builder = new RFIDQueryBuilder(args[0], args[2]);
		} catch (IOException e) {
			throw new JPSRuntimeException(QUERYBUILDER_ERROR_MSG,e);
		}

		String taggedObjectLabel = builder.queryForTaggedObjectLabel(args[1]);

		String tagStateIRI = builder.queryForTagStateIRIFromTaggedObjectIRI(args[1]);
		OffsetDateTime latestTimeStamp ;
		TimeSeries<OffsetDateTime> LatestTimeSeries = agent.queryLatestRFIDStatus(tagStateIRI);
		List<String> dataValuesAsString = LatestTimeSeries.getValuesAsString(tagStateIRI);
    	String latestTimeSeriesValue = dataValuesAsString.get(dataValuesAsString.size() - 1);
		latestTimeStamp = LatestTimeSeries.getTimes().get(LatestTimeSeries.getTimes().size() - 1);
		Date date = new java.util.Date(latestTimeStamp.toEpochSecond()*1000);
        SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss a z");
        sdf.setTimeZone(TimeZone.getDefault());
        Object ts = sdf.format(date);
		result.put("Latest In/Out Status", latestTimeSeriesValue + " since " + ts.toString());

		//query for tagged chemical container label via rdfs:label
		String objectLabel = builder.queryForTaggedObjectLabel(args[1]);
		LOGGER.info("The label of the tagged object is " + objectLabel);

		//query for chemical amount IRI with tagged chemical container IRI via ontolab:isFilledWith
		String chemicalAmountIRI = builder.queryForChemicalAmountWithIsFilledWith(args[1]);
		if (!chemicalAmountIRI.contains("This tagged object does not contain any chemicals")){
			result.put("Bottle Label", taggedObjectLabel);
			LOGGER.info("The chemical amount IRI retrieved is " + chemicalAmountIRI);

			//query for chemical IRI with chemical amount IRI via ontocape_cps_behavior:refersToMaterial
			String chemicalIRI = builder.queryForChemicalWithRefersToMaterial(chemicalAmountIRI);
			LOGGER.info("The material IRI retrieved is " + chemicalIRI);

			String speciesIRI;
			//check whether there are IRIs instantiated via ontocape_material:intrinsicCharacteristics
			//If so continue the queries to retrieve the chemicalComponent IRI which is equivalent to the species IRI
			if (builder.queryForChemicalComponent(chemicalIRI) != null){
				speciesIRI = builder.queryForChemicalComponent(chemicalIRI);
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

			String molecularFormula = builder.queryForMolecularFormula(speciesIRI);

			if(!molecularFormula.contains("Molecular Formula information not available")) {
				result.put("Molecular Formula", molecularFormula);
			}

			String molecularWeightValue = builder.queryForMolecularWeightValue(speciesIRI);
			String molecularWeightUnit = builder.queryForMolecularWeightUnit(speciesIRI);
			if(!molecularWeightValue.contains("Molecular Weight information not available") && !molecularWeightUnit.contains("Molecular Weight unit information not available")) {
				result.put("Molecular Weight", molecularWeightValue + " " + molecularWeightUnit);
			}

			//query for hazard statement IRIs via ontospecies:hasGHSHazardStatements
			JSONArray GHSHazardStatements = builder.queryForGHSHazardStatements(speciesIRI);

			//query for the label and comment of each hazard statement IRI and put them in a hash map
			Map<String, List<String>> map = builder.queryForLabelAndCommentForGHSHazardStatements(GHSHazardStatements);

			JSONObject ghsHazardStatements = new JSONObject();
			if (map != null) {
				for (int i = 0; i <= map.get("label").size() - 1; i++) {
					String label = map.get("label").get(i);
					String comment = map.get("comment").get(i);
					LOGGER.info("The label from the map is " + label);
					LOGGER.info("The comment from the map is " + comment);
					ghsHazardStatements.put(label, comment.substring(0, comment.indexOf("[") - 1));
				}
				result.put("GHS Hazard Statements", ghsHazardStatements);
			}
		} else {
			result.put("Tagged Object Label", taggedObjectLabel);
		}
		return result;
	}

	/**
	 * Execute send notification route
	 * @param args the arguments needed to execute the send notification route
	 * @return the result of send notification route
	 */
	private JSONObject executeSendNotification(String[] args) {
		// Ensure that there are four arguments provided
        if (args.length != 4) {
            LOGGER.error(SEND_NOTIF_ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(SEND_NOTIF_ARGUMENT_MISMATCH_MSG);
        }
		JSONObject jsonMessage = new JSONObject();
		// Create the agent
		RFIDQueryAgent agent;
		try {
			agent = new RFIDQueryAgent();
		} catch (IOException e) {
			LOGGER.error(AGENT_ERROR_MSG, e);
			throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
		}
		LOGGER.info("Input agent initialized.");
		String dataIRI = args[1];

		try {
			loadTSClientConfigs(args[0]);
		} catch (IOException e) {
			throw new JPSRuntimeException(LOADCONFIGS_ERROR_MSG, e);
		}

		LOGGER.info("Beginning queries...");
		//Create RFIDQueryBuilder
		RFIDQueryBuilder builder ;
		try {
			builder = new RFIDQueryBuilder(args[0], args[3]);
		} catch (IOException e) {
			throw new JPSRuntimeException(QUERYBUILDER_ERROR_MSG,e);
		}

		//query for tag IRI with state IRI via saref:hasState
		String tagIRI = builder.queryForTagWithStateIRI(dataIRI);
		LOGGER.info("The tag IRI retrieved is " + tagIRI);

		//query for tagged object IRI with tag IRI via ontodevice:isAttachedTo
		String taggedObjectIRI = builder.queryForTaggedObjectWithIsAttachedTo(tagIRI);
		LOGGER.info("The bottle IRI retrieved is " + taggedObjectIRI);

		//query for bottle label via rdfs:label
		String objectLabel = builder.queryForTaggedObjectLabel(taggedObjectIRI);
		LOGGER.info("The label of the tagged object is " + objectLabel);

		//query for chemical amount IRI with bottle IRI via ontolab:isFilledWith
		String chemicalAmountIRI = builder.queryForChemicalAmountWithIsFilledWith(taggedObjectIRI);
		if (!chemicalAmountIRI.contains("This tagged object does not contain any chemicals")){
			LOGGER.info("The chemical amount IRI retrieved is " + chemicalAmountIRI);

			//query for chemical IRI with chemical amount IRI via ontocape_cps_behavior:refersToMaterial
			String chemicalIRI = builder.queryForChemicalWithRefersToMaterial(chemicalAmountIRI);
			LOGGER.info("The material IRI retrieved is " + chemicalIRI);

			String speciesIRI;
			//check whether there are IRIs instantiated via ontocape_material:intrinsicCharacteristics
			//If so continue the queries to retrieve the chemicalComponent IRI which is equivalent to the species IRI
			if (builder.queryForChemicalComponent(chemicalIRI) != null){
				speciesIRI = builder.queryForChemicalComponent(chemicalIRI);
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
			agent.sendStatusChangeEmail(dataIRI, objectLabel, speciesLabel, args[2], map);
			LOGGER.info("Alert Email sent for " + dataIRI);
			jsonMessage.accumulate("Result", "Alert Email sent for " + dataIRI);

			LOGGER.info("Beginning queries...");

		} else {
			LOGGER.info("Preparing to send email...");
			agent.sendStatusChangeEmail(dataIRI, objectLabel, null, args[2], null);
			LOGGER.info("Alert Email sent for " + dataIRI);
			jsonMessage.accumulate("Result", "Alert Email sent for " + dataIRI);
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
			if (prop.containsKey("sparql.username")) {
				kbClient.setUser(prop.getProperty("sparql.username"));
			}
			if (prop.containsKey("sparql.password")) {
				kbClient.setPassword(prop.getProperty("sparql.password"));
			}
        }
        }
    }

