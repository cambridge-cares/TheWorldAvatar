package uk.ac.cam.cares.jps.agent.devinst;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
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
public class DevInstAgentLauncher extends JPSAgent {
	
	public static final String KEY_MICROCONTROLLER = "MicroController";
	public static final String KEY_IRIMAPPER = "IRIMapper";
	public static final String KEY_ADDITRIONALQUERY = "AdditionalQuery";
	
	
	 String agentProperties;
	 String apiProperties;
	 String clientProperties;
	 String launcherProperties;
	
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(DevInstAgentLauncher.class);
    /**
     * Logging / error messages
     */
    private static final String ARGUMENT_MISMATCH_MSG = "Need four properties files in the following order: 1) input agent 2) time series client 3) API connector 4) launcher properties.";
    private static final String AGENT_ERROR_MSG = "The RFID Update agent could not be constructed!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String CONNECTOR_ERROR_MSG = "Could not construct the RFID API connector needed to interact with the API!";
    private static final String GET_READINGS_ERROR_MSG = "Some readings could not be retrieved.";

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    } 
    
    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject jsonMessage = new JSONObject();
      if (validateInput(requestParams)) {
        	LOGGER.info("Passing request to RFID Update Agent..");
            
            jsonMessage = initializeAgent(requestParams);
			
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
      String launcherProperties;
      //TODO Validate keys on every level of the request
      if (requestParams.isEmpty()) {
    	  validate = false;
      }
      else {
 		 validate = requestParams.has(KEY_MICROCONTROLLER);
 		 if (validate == true) {
 		 validate = requestParams.has(KEY_IRIMAPPER);
 		 }
 		 if (validate == true) {
 		 validate = requestParams.has(KEY_ADDITRIONALQUERY);
 		 }
 		}
	return validate;
      
      }
    
 // TODO: Use proper argument parsing
    /**
     * Main method that runs through all steps to update the data received from the RFID API.
     * defined in the provided properties file.
     * @param args The command line arguments. Four properties files should be passed here in order: 1) input agent
     *             2) time series client 3) API connector 4)Launcher Properties .
     * @throws FileNotFoundException 
     */
    
    public static JSONObject initializeAgent(JSONObject args) {

        JSONObject MicroController = args.getJSONObject(KEY_MICROCONTROLLER);
        JSONObject IRIMapper = args.getJSONObject(KEY_IRIMAPPER);
        JSONObject AdditionalQuery = args.getJSONObject(KEY_ADDITRIONALQUERY);

        // Ensure that there are three properties files
        if (args.keySet().size() != 3) {
            LOGGER.error(ARGUMENT_MISMATCH_MSG);
            throw new JPSRuntimeException(ARGUMENT_MISMATCH_MSG);
        }

        LOGGER.info("Input agent object initialized.");
        JSONObject jsonMessage = new JSONObject();
        

        
		return jsonMessage;
        
    }

}
