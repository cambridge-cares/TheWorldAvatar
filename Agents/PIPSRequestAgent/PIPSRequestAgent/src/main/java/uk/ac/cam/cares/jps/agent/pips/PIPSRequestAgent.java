package uk.ac.cam.cares.jps.agent.pips;

import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@WebServlet(urlPatterns = {"/status", "/retrieve"})
public class PIPSRequestAgent extends JPSAgent {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(PIPSRequestAgent.class);

    /**
     * Logging / error messages
     */
    private static final String PARAMS_ERROR_MSG = "Missing or invalid parameters!";
    private static final String UNDEFINED_ROUTE_ERROR_MSG = "Invalid route! Requested route does not exist for : ";
    private static final String UNAUTHORIZED_ERROR_MSG = "Missing or invalid credentials!";

    /**
     * Variables
     */
    private static final String JSON_ERROR_KEY = "Error";
    private static final String JSON_RESULT_KEY = "Result";
    private static final String TOKEN_KEY = "access_token";
    private static final String REFRESH_TOKEN_KEY = "refresh_token";
    private static final String MESSAGE_KEY = "message";
    private static final String REQUEST_PARAM_SOURCE = "source";
    private static final String REQUEST_PARAM_NUM = "num";
    private static final String CLIENT_CERT_AUTH = "client_cert_auth";
    private boolean valid = false;
    private String accessToken;
    private String refreshToken;

    // KeycloakConnector instance
    KeycloakConnector keycloakConnector;

    
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
        accessToken = null;
        refreshToken = null;
    }

    /**
     * Process request parameters and determine which route to execute
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        JSONObject jsonMessage = new JSONObject();
        LOGGER.info("Passing request to Agent..");
        String route = requestParams.get("requestUrl").toString();
        route = route.substring(route.lastIndexOf("/") + 1);
        switch (route) {
            case "status":
            jsonMessage = statusRoute();
            break;
            case "retrieve":
                if (requestParams.has(REQUEST_PARAM_SOURCE) && requestParams.has(REQUEST_PARAM_NUM) && requestParams.has(CLIENT_CERT_AUTH)) {
                    String source = requestParams.getString(REQUEST_PARAM_SOURCE);
                    int num = requestParams.getInt(REQUEST_PARAM_NUM);
                    Boolean client_cert_auth = requestParams.getBoolean(CLIENT_CERT_AUTH);
                    try {
                        jsonMessage = retrieveRoute(source, num, client_cert_auth);
                    } catch (JSONException e) {
                        throw new JPSRuntimeException(e.toString());
                    } catch (IOException e) {
                        throw new JPSRuntimeException(e.toString());
                    }
                } else {
                    jsonMessage.put(JSON_ERROR_KEY, PARAMS_ERROR_MSG);
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
     * Handle GET /retrieve route and return the retrieved results.
     *
     * @return results of /retrieve route
     * @throws IOException 
     * @throws JSONException 
     */
    private JSONObject retrieveRoute(String source, int num, Boolean client_cert_auth) throws JSONException, IOException {
        keycloakConnector  = new KeycloakConnector();
        JSONObject message = new JSONObject();
        //request for Token if token is null
        if (accessToken == null & refreshToken == null) {
            try {
                JSONObject response = keycloakConnector.getTokens("password");
                accessToken = response.getString(TOKEN_KEY);
                refreshToken = response.getString(REFRESH_TOKEN_KEY);
            } catch (Exception e) {
                message.put(MESSAGE_KEY, UNAUTHORIZED_ERROR_MSG);
                return message;
            }
        }

        // send access token to pips-timeseries-agent
        PIPSTSAgentAPIConnector pipsTsAgentAPIConnector = new PIPSTSAgentAPIConnector();
        JSONObject response = pipsTsAgentAPIConnector.getTimeSeries(accessToken, source, num, client_cert_auth);

        // invalid token
        if (response.has(MESSAGE_KEY) && response.getString(MESSAGE_KEY).contains("invalid token")) {
            LOGGER.info("The access token is invalid!");
            //check refresh token is still valid
            LOGGER.info("Check refresh token validity...");
            if (keycloakConnector.checkTokenStatus(refreshToken)) {
                LOGGER.info("Refresh token is valid...");
                // if valid, use refresh token to renew access token
                response = keycloakConnector.refreshToken(refreshToken);
                accessToken = response.getString(TOKEN_KEY);
                refreshToken = response.getString(REFRESH_TOKEN_KEY);
                LOGGER.info("Renewed tokens...");
            } else {
                LOGGER.info("Refresh token is invalid...");
                // get new tokens via credentials
                try {
                    response = keycloakConnector.getTokens("password");
                    accessToken = response.getString(TOKEN_KEY);
                    refreshToken = response.getString(REFRESH_TOKEN_KEY);
                    LOGGER.info("Received new tokens...");
                } catch (Exception e) {
                    message.put(MESSAGE_KEY, UNAUTHORIZED_ERROR_MSG);
                    return message;
                }
            }
            // try again with new access token
            response = pipsTsAgentAPIConnector.getTimeSeries(accessToken, source, num, client_cert_auth);
        }

        message = response;
        
        return message;
    }
}