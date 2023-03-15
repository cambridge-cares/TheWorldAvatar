package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.io.IOException;
import java.time.OffsetDateTime;


@WebServlet(urlPatterns = {"/retrieve/ts", "/status", "/retrieve/equipment"})
public class BMSQueryAgentLauncher extends JPSAgent {


    private static final Logger LOGGER = LogManager.getLogger(BMSQueryAgentLauncher.class);

    private static final String KEY_DATAIRI = "dataIRI";
    private static final String KEY_CLIENT_PROPERTIES = "clientProperties";

    public static final String PARAMETERS_VALIDATION_ERROR_MSG = "Unable to validate request sent to the agent.";
    public static final String EMPTY_PARAMETER_ERROR_MSG = "Empty Request.";
    public static final String AGENT_Construction_ERROR_MSG = "The BMSQueryAgent could not be constructed.";
    public static final String TSCLIENT_CONSTRUCTION_ERROR_MSG = "Could not construct the time series client needed by the input agent.";

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {

        return processRequestParameters(requestParams);
    }

    // sample request POST http://localhost:<port>/bms-query-agent/retrieve
    //Content-Type: application/json
    //{"dataIRI":"iri_1", "clientProperties":"CLIENT_PROPERTIES"}

    // sample input: http://www.theworldavatar.com/BMS/CaresLab#V_SashOpeningOfWFH-04
    //http://www.theworldavatar.com/BMS/CaresLab#V_StatusOfWFH-04
    //http://www.theworldavatar.com/BMS/CaresLab#V_AirFlowofVAV-E/7-12
    //http://www.theworldavatar.com/BMS/CaresLab#V_DamperStateOfVAV-E/7-12
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (!validateInput(requestParams)) {
            LOGGER.error(PARAMETERS_VALIDATION_ERROR_MSG);
            throw new JPSRuntimeException(PARAMETERS_VALIDATION_ERROR_MSG);
        }

        String dataIRI = requestParams.getString(KEY_DATAIRI);
        String clientProperties = requestParams.getString(KEY_CLIENT_PROPERTIES);
        String clientPropertiesFile = System.getenv(clientProperties);

        JSONObject jsonMessage = new JSONObject();
        BMSQueryAgent agent = initializeAgent(clientPropertiesFile, jsonMessage);

        JSONObject queryResult = agent.queryTimeSeriesWithinBound(dataIRI);
        jsonMessage.accumulate("Result", queryResult);
        jsonMessage.accumulate("Message", "POST request has been sent successfully.");

        return jsonMessage;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {

        if (requestParams.isEmpty()) {
            LOGGER.error(EMPTY_PARAMETER_ERROR_MSG);
            return false;
        }

        if (!requestParams.has(KEY_DATAIRI)) {
            LOGGER.error(KEY_DATAIRI + "is missing.");
            return false;
        }

        if (!requestParams.has(KEY_CLIENT_PROPERTIES)) {
            LOGGER.error(KEY_CLIENT_PROPERTIES + "is missing.");
            return false;
        }

        String clientProperties = requestParams.getString(KEY_CLIENT_PROPERTIES);
        if (System.getenv(clientProperties) == null) {
            LOGGER.error("Client property file is not found in the environment variable.");
            return false;
        }

        return true;
    }

    public BMSQueryAgent initializeAgent(String clientPropertyFile, JSONObject jsonMessage) {
        BMSQueryAgent agent;

        try {
            agent = new BMSQueryAgent();
        } catch (Exception e) {
            LOGGER.error(AGENT_Construction_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_Construction_ERROR_MSG, e);
        }
        LOGGER.info("Input agent object initialized.");
        jsonMessage.accumulate("Message", "Input agent object initialized.");

        TimeSeriesClient<OffsetDateTime> tsClient;
        try {
            tsClient = new TimeSeriesClient<>(OffsetDateTime.class, clientPropertyFile);
            agent.setTsClient(tsClient);
        } catch (IOException | JPSRuntimeException e) {
            LOGGER.error(TSCLIENT_CONSTRUCTION_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_CONSTRUCTION_ERROR_MSG, e);
        }
        LOGGER.info("Time series client object initialized.");
        jsonMessage.accumulate("Message", "Time series client object initialized.");

        return agent;
    }
}
