package uk.ac.cam.cares.jps.agent.pips;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.sql.SQLException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.HttpResponseException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@WebServlet(urlPatterns = {"/status", "/timeseries"})
public class PIPSTimeSeriesAgent extends JPSAgent {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(PIPSTimeSeriesAgent.class);

    /**
     * Logging / error messages
     */
    private static final String UNDEFINED_ROUTE_ERROR_MSG = "Invalid route! Requested route does not exist for : ";
    private static final String UNAUTHORIZED_ERROR_MSG = "Missing or invalid credentials!";
    private static final String PARAMETERS_ERROR_MSG = "Missing or incorrect parameters!";
    private static final String SQL_ERROR_MSG = "Unable to execute certain SQL actions or queries: ";

    /**
     * Keys
     */
    private static final String JSON_ERROR_KEY = "Error";
    private static final String JSON_RESULT_KEY = "Result";
    private static final String REQUEST_PARAM_SOURCE = "source";
    private static final String REQUEST_PARAM_NUM = "num";
    private boolean valid = false;
    
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
        valid = true;
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        JSONObject jsonMessage = new JSONObject();
        LOGGER.info("Passing request to PIPS Agent..");
        String route = requestParams.get("requestUrl").toString();
        route = route.substring(route.lastIndexOf("/") + 1);
        switch (route) {
            case "status":
            jsonMessage = statusRoute();
            break;
            case "timeseries":
            if (requestParams.has(REQUEST_PARAM_SOURCE) && requestParams.has(REQUEST_PARAM_NUM)) {
                // Get the Authorization header
                String authHeader = request.getHeader("Authorization");
                if (authHeader != null && authHeader.startsWith("Bearer ")) {
                    String token = authHeader.substring("Bearer".length()).trim();
                    try {
                        jsonMessage = timeseriesRoute(token, requestParams.getString(REQUEST_PARAM_SOURCE), requestParams.getInt(REQUEST_PARAM_NUM));
                    } catch (Exception e) {
                        throw new JPSRuntimeException(e);
                    }  
                } else {
                    jsonMessage.put(JSON_ERROR_KEY, UNAUTHORIZED_ERROR_MSG);
                }
            } else {
                jsonMessage.put(JSON_ERROR_KEY, PARAMETERS_ERROR_MSG);
            }
            break;
            default:
                LOGGER.fatal("{}{}", UNDEFINED_ROUTE_ERROR_MSG, route);
                jsonMessage.put(JSON_ERROR_KEY, UNDEFINED_ROUTE_ERROR_MSG + route);
        }
        return jsonMessage;
    }

    /**
     * Handle GET /status route and return the status of the agent.
     *
     * @return Status of the agent
     */
    private JSONObject statusRoute() {
        JSONObject message = new JSONObject();
        LOGGER.info("Detected request to get agent status...");
        if (valid) {
            message.put(JSON_RESULT_KEY, "Agent is ready to receive requests.");
        } else {
            message.put(JSON_ERROR_KEY, "Agent could not be initialised! Please check logs for more information.");
        }
        return message;
    }


    /**
     * Handle GET /timeseries route
     *
     * @return timeseries data
     * @throws IOException 
     */
    private JSONObject timeseriesRoute(String token, String schema, int number) throws IOException {
        LOGGER.info("Detected request to get timeseries ...");
        LOGGER.info("Checking validity of client...");
        JSONObject message = new JSONObject();
        KeycloakConnector keycloakConnector = new KeycloakConnector();
        try {
            message = keycloakConnector.checkAuthorizationViaUMA(token);
        } catch (HttpResponseException e) {
            LOGGER.info("The status code is " + e.getStatusCode());
            LOGGER.info("The reason phrase is " + e.getReasonPhrase());
            if (e.getStatusCode() == 403 || e.getStatusCode() == 401) {
                if (e.getReasonPhrase().contains("Invalid bearer token")) {
                    message = new JSONObject();
                    message.put("message", "invalid token");
                } else if (e.getReasonPhrase().contains("not_authorized")) {
                    //send not authorized message
                    message = new JSONObject();
                    message.put("message", "unauthorized");
                }
            }
        }
        
        if (message.has("access_token") && message.getString("access_token").length() > 1) {
            // query for timeseries data from postgreSQL
            PostgreSQLConnector postgreSQLConnector = new PostgreSQLConnector();
            try {
                message = postgreSQLConnector.retrieveTimeSeries(schema, number);
            } catch (SQLException e) {
                throw new JPSRuntimeException(SQL_ERROR_MSG, e);
            }
        }
        return message;
    }
}