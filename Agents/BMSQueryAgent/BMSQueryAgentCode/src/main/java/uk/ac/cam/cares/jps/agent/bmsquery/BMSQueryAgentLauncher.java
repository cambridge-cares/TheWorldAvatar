package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.OffsetDateTime;
import java.util.Properties;
import java.util.regex.Pattern;


@WebServlet(urlPatterns = {"/retrieve/ts", "/status", "/retrieve/equipment"})
public class BMSQueryAgentLauncher extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(BMSQueryAgentLauncher.class);

    private static final String KEY_DATAIRI = "dataIRI";
    private static final String KEY_CLIENT_PROPERTIES = "clientProperties";

    public static final String PARAMETERS_VALIDATION_ERROR_MSG = "Unable to validate request sent to the agent.";
    public static final String EMPTY_PARAMETER_ERROR_MSG = "Empty Request.";
    public static final String AGENT_Construction_ERROR_MSG = "The BMSQueryAgent could not be constructed.";
    public static final String TSCLIENT_CONSTRUCTION_ERROR_MSG = "Could not construct the time series client needed by the input agent.";
    public static final String RSCLIENT_CONSTRUCTION_ERROR_MSG = "Could not construct the remote store client needed by the input agent.";

    @Override
    public void init() throws ServletException {
        super.init();
        LOGGER.debug("This is a debug message.");
        LOGGER.info("This is an info message.");
        LOGGER.warn("This is a warn message.");
        LOGGER.error("This is an error message.");
        LOGGER.fatal("This is a fatal message.");
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {

        String url = request.getRequestURI();
        if (url.contains("?")) url = url.split(Pattern.quote("?"))[0];
        JSONObject jsonMessage = new JSONObject();

        if (url.contains("status")) {
            getStatus(jsonMessage);
            return jsonMessage;
        }

        if (url.contains("retrieve")) {
            if (!validateInput(requestParams)) {
                LOGGER.error(PARAMETERS_VALIDATION_ERROR_MSG);
                throw new JPSRuntimeException(PARAMETERS_VALIDATION_ERROR_MSG);
            }

            String dataIRI = requestParams.getString(KEY_DATAIRI);
            String clientProperties = requestParams.getString(KEY_CLIENT_PROPERTIES);
            String clientPropertiesFile = System.getenv(clientProperties);

            BMSQueryAgent agent = initializeAgent(clientPropertiesFile, jsonMessage);

            if (url.contains("ts")) {
                // handle the case "retrieve/ts", return the timeseries data and time only
                JSONObject queryResult = agent.queryTimeSeriesWithinBound(dataIRI);
                jsonMessage.accumulate("Result", queryResult);
            } else if (url.contains("equipment")) {
                // handle the case "retrieve/equipment", return the list of equipment of the selected type
                JSONObject queryResult = agent.queryEquipmentInstance(dataIRI);
                jsonMessage.accumulate("Result", queryResult);
            }

            jsonMessage.accumulate("Message", "POST request has been sent successfully.");
            return jsonMessage.append("test message", request.getRequestURI() + "; getURL " + request.getRequestURL());
        }

        throw new JPSRuntimeException("Route: " + url + " does not exist");
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        LOGGER.debug("Getting requestParams: " + requestParams.toString());

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

    // TODO: refactor everything with clientpropertyfile, which comes from local container configuration
    public BMSQueryAgent initializeAgent(String clientPropertyFile, JSONObject jsonMessage) {
        EndpointConfig endpointConfig = new EndpointConfig();

        RemoteStoreClient rsClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        TimeSeriesClient<OffsetDateTime> tsClient = new TimeSeriesClient<>(rsClient, OffsetDateTime.class, endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());

        BMSQueryAgent agent = createBMSQueryAgent(jsonMessage);
        agent.setRSClient(rsClient);
        agent.setTSClient(tsClient);

        LOGGER.info("Input agent object initialized.");
        jsonMessage.accumulate("Message", "Input agent object initialized.");
        return agent;
    }

    private BMSQueryAgent createBMSQueryAgent(JSONObject jsonMessage) {
        BMSQueryAgent agent;
        try {
            agent = new BMSQueryAgent();
        } catch (Exception e) {
            LOGGER.error(AGENT_Construction_ERROR_MSG, e);
            throw new JPSRuntimeException(AGENT_Construction_ERROR_MSG, e);
        }
        return agent;
    }

    public void getStatus(JSONObject jsonMessage) {
        jsonMessage.accumulate("Message", "BMSQueryAgent is ready.");
    }


    // ****************************** endpoint set up for non-stack version ********************************************
//    private TimeSeriesClient<OffsetDateTime> createTimeSeriesClient(JSONObject jsonMessage, String clientPropertyFile) {
//        TimeSeriesClient<OffsetDateTime> tsClient;
//        try {
//            tsClient = new TimeSeriesClient<>(OffsetDateTime.class, clientPropertyFile);
//        } catch (IOException | JPSRuntimeException e) {
//            LOGGER.error(TSCLIENT_CONSTRUCTION_ERROR_MSG, e);
//            throw new JPSRuntimeException(TSCLIENT_CONSTRUCTION_ERROR_MSG, e);
//        }
//        LOGGER.info("Time series client object initialized.");
//        jsonMessage.accumulate("Message", "Time series client object initialized.");
//        return tsClient;
//    }
//
//    private RemoteStoreClient createRemoteStoreClient(JSONObject jsonMessage, String clientPropertyFile) {
//        RemoteStoreClient kbClient = new RemoteStoreClient();
//        try {
//            setSparqlConfig(clientPropertyFile, kbClient);
//        } catch (IOException e) {
//            throw new JPSRuntimeException(RSCLIENT_CONSTRUCTION_ERROR_MSG, e);
//        }
//        LOGGER.info("Remote store client object initialized.");
//        jsonMessage.accumulate("Message", "Remote store client object initialized.");
//        return kbClient;
//    }
//
//    private void setSparqlConfig(String filepath, TripleStoreClientInterface kbClient) throws IOException {
//        File file = new File(filepath);
//        if (!file.exists()) {
//            throw new JPSRuntimeException("No properties file found at specified filepath: " + filepath);
//        }
//
//        // Try-with-resource to ensure closure of input stream
//        try (InputStream input = new FileInputStream(file)) {
//
//            // Load properties file from specified path
//            Properties prop = new Properties();
//            prop.load(input);
//
//            // Get the property values and assign
//            if (prop.containsKey("sparql.query.endpoint")) {
//                kbClient.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
//            } else {
//                throw new JPSRuntimeException("Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\" ");
//            }
//            if (prop.containsKey("sparql.update.endpoint")) {
//                kbClient.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
//            } else {
//                throw new JPSRuntimeException("Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\" ");
//            }
//        }
//    }
    // *****************************************************************************************************************

}
