package uk.ac.cam.cares.jps.agent.School;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import java.util.*;
import java.io.IOException;
import java.text.SimpleDateFormat;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@WebServlet(urlPatterns = { "/status, /create" })

public class SchoolAgent extends JPSAgent {
    public static final String Key_APIProp = "apiProperties";

    private static final Logger LOGGER = LogManager.getLogger(SchoolAgent.class);

    private static final String CONNECTOR_ERROR_MSG = "Could not construct the School API Connector needed to interact with the API!";
    private static final String GET_READINGS_ERROR_MSG = "Some readings could not be retrieved.";
    private static final String UNDEFINED_ROUTE_ERROR_MSG = "Invalid route! Requested route does not exist for : ";

    private static final String SCHOOL_API_PROPERTIES = "SCHOOL_API_PROPERTIES";
    private static final String JSON_ERROR_KEY = "Error";
    private static final String JSON_RESULT_KEY = "Result";

    /**
     * Servlet init.
     *
     * @throws ServletException
     */
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
    public JSONObject processRequestParameters(JSONObject requestParams) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        LOGGER.info("Request received at: {}", datetime);
        JSONObject msg = new JSONObject();
        String route = requestParams.get("requestUrl").toString();
        route = route.substring(route.lastIndexOf("/") + 1);
        switch (route) {
            case "status":
                msg = statusRoute();
                break;
            case "create":
                LOGGER.info("Executing create route ...");
                msg = createRoute();
                break;
            default:
                LOGGER.fatal("{}{}", UNDEFINED_ROUTE_ERROR_MSG, route);
                msg.put(JSON_ERROR_KEY, UNDEFINED_ROUTE_ERROR_MSG + route);
        }
        return msg;
    }

    /**
     * Handle GET /status route and return the status of the agent.
     * @return Status of the agent
     */
    private JSONObject statusRoute() {
        JSONObject response = new JSONObject();
        LOGGER.info("Detected request to get agent status...");
        response.put(JSON_RESULT_KEY, "Agent is ready to receive requests.");
        return response;
    }

    /**
     * Handle POST /create route and return the execution status.
     * @return status of create route execution
     */
    private JSONObject createRoute() {
        JSONObject jsonMessage = new JSONObject();
        APIConnector connector;
        try {
            connector = new APIConnector(System.getenv(SCHOOL_API_PROPERTIES));
        } catch (IOException e) {
            LOGGER.error(CONNECTOR_ERROR_MSG, e);
            throw new JPSRuntimeException(CONNECTOR_ERROR_MSG, e);
        }

        LOGGER.info("API Connector Object Initialized");
        jsonMessage.accumulate("Result", "API Connector object Initialized");

        JSONObject general_readings;
        JSONObject programme_readings;
        JSONObject cca_readings;

        try {
            general_readings = connector.getGeneralReadings();
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }

        LOGGER.info(String.format("Retrieved %d school readings",
        general_readings.getJSONObject("result").getJSONArray("records").length()));
        jsonMessage.accumulate("Result", "Retrieved"
                + general_readings.getJSONObject("result").getJSONArray("records").length() + " school readings");

        //SqlHandler to instantiate general information

        try {
            programme_readings = connector.getProgrammes();
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG, e);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }

        LOGGER.info(String.format("Retrieved programmes readings for %d schools",
        programme_readings.getJSONObject("result").getJSONArray("records").length()));
        jsonMessage.accumulate("Result", "Retrieved"
                + programme_readings.getJSONObject("result").getJSONArray("records").length() + "programme readings");

        try {
            cca_readings = connector.getCCA();
        } catch (Exception e) {
            LOGGER.error(GET_READINGS_ERROR_MSG);
            throw new JPSRuntimeException(GET_READINGS_ERROR_MSG, e);
        }

        LOGGER.info(String.format("Retrieved CCA Readings"));
        jsonMessage.accumulate("Result",
                "Retrieved" + cca_readings.getJSONObject("result").getJSONArray("records").length() + "CCA Readings");

        return jsonMessage;
    }
}
