package uk.ac.cam.cares.jps.agent.bmsinstantiation;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.time.OffsetDateTime;

/**
 * Class with a main method that is the entry point of the compiled war and puts all components together to retrieve
 * data from the CSV file and write it into the database.
 * @author 
 */
@WebServlet(urlPatterns = {"/instantiate"})
public class BMSInstantiationAgentLauncher extends JPSAgent {
    public static final String KEY_CLIENTPROPERTIES = "clientProperties";


    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(BMSInstantiationAgentLauncher.class);
    /**
     * Logging / error messages
     */
    private static final String AGENT_ERROR_MSG = "The agent could not be constructed!";
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }


    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        if (validateInput(requestParams)) {
            LOGGER.info("Passing request to Agent..");
            String clientProperties = System.getenv(requestParams.getString(KEY_CLIENTPROPERTIES));
            String csvFilePath = System.getenv("CSVFILEPATH");
            String device = requestParams.getString("Device");
            String[] args = new String[] {csvFilePath, clientProperties, device};
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

        String clientProperties;
        if (requestParams.isEmpty()) {
            validate = false;
        }
        else {
            validate = requestParams.has(KEY_CLIENTPROPERTIES);
        }
        
        if (validate == true) {
                clientProperties =  (requestParams.getString(KEY_CLIENTPROPERTIES));
                if (System.getenv(clientProperties) == null) {
                    validate = false;

                }
            }
            return validate;
        }
    
    /**
     * Main method that runs through all steps to update the data received from the CSV file.
     * defined in the provided properties file.
     * @param args The command line arguments. Three properties files should be passed here in order: 1) time series client 3) type of device
     * @throws FileNotFoundException
     */

    public static JSONObject initializeAgent(String[] args) {

        // Create the agent
        BMSInstantiationAgent agent;
        try {
            agent = new BMSInstantiationAgent(args[0], args[1]);
        } catch (IOException e) {
            LOGGER.error(AGENT_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
        }
        LOGGER.info("Input agent object initialized.");
        JSONObject jsonMessage = new JSONObject();
        jsonMessage.accumulate("Result", "Input agent object initialized.");

        switch (args[2]) {
            case "FumeHood":
                try {
                    agent.instantiateFH();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate FumeHood instance!", e);
                }
                break;
            case "WalkInFumeHood":
                try {
                    agent.instantiateWFH();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate WalkIn-FumeHood instance!", e);
                }
                break;
            case "CanopyHood_VAV":
                try {
                    agent.instantiateCH_VAV();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate CanopyHood-VAV instance!", e);
                }
                break;
            case "CanopyHood_CAV":
                try {
                    agent.instantiateCH_CAV();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate CanopyHood-CAV instance!", e);
                }
                break;
            case "Pipe":
                try {
                    agent.instantiatePipe();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate Pipe instance!", e);
                }
                break;
            case "Duct":
                try {
                    agent.instantiateDuct();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate Duct instance!", e);
                }
                break;
            case "Valve":
                try {
                    agent.instantiateValve();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate Valve instance!", e);
                }
                break;
            case "PreCoolCoil":
                try {
                    agent.instantiatePreCoolCoil();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate PreCoolCoil instance!", e);
                }
                break;
            case "MidCoil":
                try {
                    agent.instantiateMidCoil();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate MidCoil instance!", e);
                }
                break;
            case "OffCoil":
                try {
                    agent.instantiateOffCoil();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate OffCoil instance!", e);
                }
                break;
            case "Filter":
                try {
                    agent.instantiateFilter();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate Filter instance!", e);
                }
                break;
            case "Fan":
                try {
                    agent.instantiateFan();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate Fan instance!", e);
                }
                break;
            case "T-Joint":
                try {
                    agent.instantiateTJoint();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate T-Joint instance!", e);
                }
                break;
            case "Cooling":
                try {
                    agent.instantiateCooling();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate Cooling instance!", e);
                }
                break;
            case "MakeupAirUnit":
                try {
                    agent.instantiateMAU();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate MakeupAirUnit instance!", e);
                }
                break;
            case "VAV-S":
                try {
                    agent.instantiateVAV_S();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate VAV-S instance!", e);
                }
                break;
            case "VAV-E":
                try {
                    agent.instantiateVAV_E();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate VAV_E instance!", e);
                }
                break;
            case "Damper":
                try {
                    agent.instantiateDamper();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate Damper instance!", e);
                }
                break;
            case "ExhaustFan":
                try {
                    agent.instantiateEAF();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate ExhaustFan instance!", e);
                }
                break;
            case "FCU-HE":
                try {
                    agent.instantiateFCU_HE();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate FCU-HE instance!", e);
                }
                break;
            case "FanCoilUnit":
                try {
                    agent.instantiateFCU();
                } catch (FileNotFoundException e) {
                    throw new JPSRuntimeException("Could not instantiate FanCoilUnit instance!", e);
                }
                break;
        }
        LOGGER.info("The device has been instantiated.");
        jsonMessage.accumulate("Result", "The device has been instantiated.");
        return jsonMessage;
    	
        
    }
}
