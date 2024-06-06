package uk.ac.cam.cares.jps.agent.pips;

import org.json.JSONObject;
import org.keycloak.authorization.client.AuthzClient;
import org.keycloak.authorization.client.representation.TokenIntrospectionResponse;
import org.keycloak.representations.idm.authorization.AuthorizationRequest;
import org.keycloak.representations.idm.authorization.AuthorizationResponse;
import org.keycloak.representations.idm.authorization.Permission;
import org.keycloak.representations.idm.authorization.ResourceRepresentation;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.util.Base64;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class with a main method that is the entry point of the compiled war and puts all components together to retrieve
 * data from the API and write it into the database.
 * @author GMMajal
 */
@WebServlet(urlPatterns = {"/status"})
public class PIPSAgent extends JPSAgent {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(PIPSAgent.class);

    /**
     * Logging / error messages
     */
    private static final String UNDEFINED_ROUTE_ERROR_MSG = "Invalid route! Requested route does not exist for : ";
    private static final String UNAUTHORIZED_ERROR_MSG = "Missing or invalid credentials!";

    /**
     * Keys
     */
    private static final String JSON_ERROR_KEY = "Error";
    private static final String JSON_RESULT_KEY = "Result";

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
            // Get the Authorization header
            String authHeader = request.getHeader("Authorization");
            String username = null;
            String password = null;
            if (authHeader != null && authHeader.startsWith("Basic ")) {
                // Extract and decode the base64 encoded credentials
                String base64Credentials = authHeader.substring("Basic".length()).trim();
                String credentials = new String(Base64.getDecoder().decode(base64Credentials));
                        
                // credentials format: "username:password"
                final String[] values = credentials.split(":", 2);
                        
                if (values.length == 2) {
                    username = values[0];
                    password = values[1];
                    LOGGER.info("The username is " + username + " and the password is " + password);
                } else {
                    LOGGER.info("Invalid");
                }
                jsonMessage = timeseriesRoute(username, password);
            } else {
                jsonMessage.put(JSON_ERROR_KEY, UNAUTHORIZED_ERROR_MSG);
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
     * Handle GET /timeseries route and return the status of the agent.
     *
     * @return timeseries of the agent
     */
    private JSONObject timeseriesRoute(String username, String password) {
        String token = null;
        JSONObject message = new JSONObject();
        AuthzClient authzClient = AuthzClient.create();
        AuthorizationRequest request = new AuthorizationRequest();
        request.addPermission("testing-resource");
        AuthorizationResponse response = authzClient.authorization(username, password).authorize(request);
        token = response.getToken();

        // introspect the token
        TokenIntrospectionResponse requestingPartyToken = authzClient.protection().introspectRequestingPartyToken(token);

        System.out.println("Token status is: " + requestingPartyToken.getActive());
        System.out.println("Permissions granted by the server: ");

        for (Permission granted : requestingPartyToken.getPermissions()) {
            LOGGER.info("Detected request to get timeseries ...");
            if (valid) {
                message.put(JSON_RESULT_KEY, "Agent is ready to retrieve timeseries.");
            } else {
                message.put(JSON_ERROR_KEY, "Agent could not be initialised! Please check logs for more information.");
            }
        }
        return message;
    }
}