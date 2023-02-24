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


@WebServlet(urlPatterns = {"/retrieve"})
public class BMSQueryAgentLauncher extends JPSAgent {


    private static final Logger LOGGER = LogManager.getLogger(BMSQueryAgentLauncher.class);

    private static final String KEY_DATAIRI = "dataIRI";
    private static final String KEY_CLIENT_PROPERTIES = "clientProperties";

    private static final String AGENT_ERROR_MSG = "The BMSQueryAgent could not be constructed!";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";

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
            LOGGER.error("Unable to validate request sent to the agent!");
            throw new JPSRuntimeException("Unable to validate request sent to the agent!");
        }

        String dataIRI = requestParams.getString(KEY_DATAIRI);
        String clientProperties = requestParams.getString(KEY_CLIENT_PROPERTIES);
        String clientPropertiesFile = System.getenv(clientProperties);

        JSONObject result = initializeAgent(dataIRI, clientPropertiesFile);
        result.accumulate("Message", "POST request has been sent successfully.");

        return result;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {

        if (requestParams.isEmpty()) {
            LOGGER.error("Empty Request.");
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
            LOGGER.error("Client property file at " + clientProperties + "is not found.");
            return false;
        }

        return true;
    }

    public JSONObject initializeAgent(String dataIRI, String clientPropertyFile) {
        BMSQueryAgent agent;

        try {
            agent = new BMSQueryAgent();
        } catch (Exception e) {
            LOGGER.error(AGENT_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_ERROR_MSG, e);
        }
        LOGGER.info("Input agent object initialized.");
        JSONObject jsonMessage = new JSONObject();
        jsonMessage.accumulate("Message", "Input agent object initialized.");

        TimeSeriesClient<OffsetDateTime> tsClient;
        try {
            tsClient = new TimeSeriesClient<>(OffsetDateTime.class, clientPropertyFile);
            agent.setTsClient(tsClient);
        } catch (IOException | JPSRuntimeException e) {
            LOGGER.error(TSCLIENT_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
        }
        LOGGER.info("Time series client object initialized.");
        jsonMessage.accumulate("Message", "Time series client object initialized.");

        JSONObject queryResult = agent.queryTimeSeriesWithinBound(dataIRI);
        jsonMessage.accumulate("Result", queryResult);

        return jsonMessage;
    }
}
