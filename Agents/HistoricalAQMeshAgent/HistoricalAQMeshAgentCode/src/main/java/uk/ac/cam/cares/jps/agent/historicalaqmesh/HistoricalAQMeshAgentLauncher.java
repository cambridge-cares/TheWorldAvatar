package uk.ac.cam.cares.jps.agent.historicalaqmesh;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.IOException;
import java.time.OffsetDateTime;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


@WebServlet(urlPatterns = {"/retrieve"})
public class HistoricalAQMeshAgentLauncher extends JPSAgent {
	
	public static final String KEY_AGENTPROPERTIES = "agentProperties";
	public static final String KEY_XLSXCONNECTORPROPERTIES = "xlsxConnectorProperties";
	public static final String KEY_CLIENTPROPERTIES = "clientProperties";
	
	
	 String agentProperties;
	 String xlsxConnectorProperties;
	 String clientProperties;

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(HistoricalAQMeshAgentLauncher.class);
    /**
     * Logging / error messages
     */
    private static final String ARGUMENT_MISMATCH_MSG = "Need three properties files in the following order: 1) input agent 2) time series client 3) xlsx connector.";
    private static final String AGENT_ERROR_MSG = "The Historical AQMesh agent could not be constructed!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String CONNECTOR_ERROR_MSG = "Could not construct the AQMesh XLSX connector needed to interact with the Excel file!";
    private static final String GET_READINGS_ERROR_MSG = "One or both readings could not be retrieved, this might have created a mismatch" +
            " in the pointers if one readings was successful and needs to be fixed!";
    private static final String ONE_READING_EMPTY_ERROR_MSG = "One of the readings (gas or particle) is empty, that means there is " +
            "a mismatch in the pointer for each readings. This should be fixed (and might require a clean up of the database)!";
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    } 
    
    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject jsonMessage = new JSONObject();
      if (validateInput(requestParams)) {
        	LOGGER.info("Passing request to Historical AQMesh Agent..");
            String agentProperties = System.getenv(requestParams.getString(KEY_AGENTPROPERTIES));
            String clientProperties = System.getenv(requestParams.getString(KEY_CLIENTPROPERTIES));
            String xlsxConnectorProperties = System.getenv(requestParams.getString(KEY_XLSXCONNECTORPROPERTIES));
            String[] args = new String[] {agentProperties,clientProperties, xlsxConnectorProperties};
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
      String xlsxConnectorProperties;
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
 		 validate = requestParams.has(KEY_XLSXCONNECTORPROPERTIES);
 		 }
 		 if (validate == true) {
 		 agentProperties = (requestParams.getString(KEY_AGENTPROPERTIES));
 		 clientProperties =  (requestParams.getString(KEY_CLIENTPROPERTIES));
 		 xlsxConnectorProperties = (requestParams.getString(KEY_XLSXCONNECTORPROPERTIES));
 			 
 		if (System.getenv(agentProperties) == null) {
 			validate = false;
 		 
 		 }
 		if (System.getenv(xlsxConnectorProperties) == null) {
 			validate = false;
 		 
 		 }
 		if (System.getenv(clientProperties) == null) {
 			validate = false;
 		
 		 }
 	 }
 		 }
	return validate;
    }
    public static JSONObject initializeAgent(String[] args) {
    	JSONObject jsonMessage = new JSONObject();
        // Ensure that there are three properties files
        if (args.length != 3) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }
        LOGGER.debug("Launcher called with the following files: " + String.join(" ", args));

        // Create the agent
        HistoricalAQMeshAgent agent;
        try {
            agent = new HistoricalAQMeshAgent(args[0]);
        } catch (IOException e) {
            LOGGER.error(AGENT_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
        }
        LOGGER.info("Input agent object initialized.");

        // Create and set the time series client
        TimeSeriesClient<OffsetDateTime> tsClient;
        try {
            tsClient = new TimeSeriesClient<>(OffsetDateTime.class, args[1]);
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
        HistoricalAQMeshAgentXLSXConnector connector;
        try {
            connector = new HistoricalAQMeshAgentXLSXConnector(System.getenv("GAS_READINGS"), System.getenv("PARTICLEGENERAL_READINGS"), args[2]);
        } catch (IOException e) {
            LOGGER.error(CONNECTOR_ERROR_MSG, e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG, e);
        }
        LOGGER.info("xlsx connector object initialized.");

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
            jsonMessage.put("Result", "Data updated with new readings from API.");
        }
        // If both are empty no new readings are available
        else if(particleReadings.isEmpty() && gasReadings.isEmpty()) {
            LOGGER.info("No new readings are available.");
            jsonMessage.put("Result", "No new readings are available.");
        }
        // One reading is empty and the other is not. This is likely due to asynchronous access to the readings, which
        // sets the pointers for each reading separately (should not happen when only using the agent unless there is an API error).
        // The pointer should be reset and probably manual clean up in the database is required.
        // Note: This is normally not a problem, but since the AQMesh Agent requires that all JSON keys are present in the combined
        // readings, having one reading empty will result in an error when calling the updateData method.
        else {
            LOGGER.error(ONE_READING_EMPTY_ERROR_MSG);
            throw new JPSRuntimeException(ONE_READING_EMPTY_ERROR_MSG);
        }
        return jsonMessage;
    }

}
