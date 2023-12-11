package uk.ac.cam.cares.jps.agent.dashboard.utils;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Base64;

/**
 * A class that provides methods to facilitate communication between agents through handling requests and responses.
 *
 * @author qhouyee
 */
public class AgentCommunicationClient {
    private static final String AUTHORISATION_HEADER = "Authorization";
    private static final String BAD_REQUEST_ERROR = "Unable to connect or send request. Please ensure the url is valid. If valid, check the message for more details:";
    private static final String INTERRUPTED_ERROR = "Thread has been interrupted!";
    private static final HttpClient HTTP_CLIENT = HttpClient.newHttpClient();
    private static final Logger LOGGER = LogManager.getLogger(AgentCommunicationClient.class);

    // Private constructor to prevent instantiation.
    private AgentCommunicationClient() {
    }

    /**
     * Verifies the success of the request, which should return status code of 200.
     * If unsuccessful, agent will stop processing the request.
     *
     * @param response     The response returned from the API.
     * @param errorMessage Error message to display if unsuccessful.
     */
    public static void verifySuccessfulRequest(HttpResponse<String> response, String errorMessage) {
        if (response.statusCode() == 400) {
            LOGGER.fatal(errorMessage);
            throw new IllegalArgumentException(errorMessage);
        } else if (response.statusCode() != 200) {
            LOGGER.fatal(errorMessage);
            throw new JPSRuntimeException(errorMessage);
        }
    }

    /**
     * Retrieves the body content of the response (usually in JSON) as either a JSON Object or Array.
     *
     * @param response The response returned from the API.
     * @return The response body as either a JSON Object or Array.
     */
    public static JsonElement retrieveResponseBody(HttpResponse<String> response) {
        return JsonParser.parseString(response.body());
    }

    /**
     * Sends a GET request to a specific API endpoint without username or password.
     *
     * @param url API endpoint.
     */
    public static HttpResponse<String> sendGetRequest(String url) {return sendGetRequest(url, null, "", "");}

    /**
     * Sends a GET request to a specific API endpoint authenticated with security tokens.
     *
     * @param url         API endpoint.
     * @param bearerToken API token to access endpoint.
     */
    public static HttpResponse<String> sendGetRequest(String url, String bearerToken) {return sendGetRequest(url, bearerToken, "", "");}

    /**
     * Sends a GET request to a specific API endpoint with basic authentication.
     *
     * @param url      API endpoint.
     * @param userName Username to access endpoint.
     * @param password Password to access endpoint.
     */
    public static HttpResponse<String> sendGetRequest(String url, String userName, String password) {return sendGetRequest(url, null, userName, password);}

    /**
     * Sends a GET request to a specific API endpoint with no, basic or token authentication.
     *
     * @param url         API endpoint.
     * @param bearerToken API token to access endpoint.
     * @param userName    Username to access endpoint.
     * @param password    Password to access endpoint.
     */
    public static HttpResponse<String> sendGetRequest(String url, String bearerToken, String userName, String password) {
        try {
            HttpRequest.Builder requestBuilder = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .GET()
                    .timeout(Duration.ofSeconds(3600));
            if (bearerToken != null) {
                requestBuilder.header(AUTHORISATION_HEADER, "Bearer " + bearerToken);
            } else if (!userName.isEmpty() && !password.isEmpty())
                // If username and password are provided, add the authentication
                requestBuilder.header(AUTHORISATION_HEADER, getBasicAuthenticationHeader(userName, password));
            // Await response before continue executing the rest of the code
            return HTTP_CLIENT.send(requestBuilder.build(), HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            LOGGER.fatal(BAD_REQUEST_ERROR, e);
            throw new JPSRuntimeException(BAD_REQUEST_ERROR, e);
        } catch (InterruptedException e) {
            LOGGER.fatal(INTERRUPTED_ERROR, e);
            Thread.currentThread().interrupt();
            throw new JPSRuntimeException(INTERRUPTED_ERROR, e);
        }
    }

    /**
     * An overloaded method that sends a POST request with JSON parameters to a specific API endpoint with basic authentication.
     *
     * @param url        API endpoint.
     * @param jsonParams JSON parameters to be sent in the request body.
     * @param userName   Username for basic authentication.
     * @param password   Password for basic authentication.
     */
    public static HttpResponse<String> sendPostRequest(String url, String jsonParams, String userName, String password) {return sendPostRequest(url, jsonParams, null, userName, password);}

    /**
     * An overloaded method that sends a POST request with JSON parameters to a specific API endpoint authenticated with security tokens.
     *
     * @param url         API endpoint.
     * @param jsonParams  JSON parameters to be sent in the request body.
     * @param bearerToken API token to access endpoint.
     */
    public static HttpResponse<String> sendPostRequest(String url, String jsonParams, String bearerToken) {return sendPostRequest(url, jsonParams, bearerToken, null, null);}

    /**
     * A method that sends a POST request with JSON parameters to a specific API endpoint with basic or token authentication.
     *
     * @param url         API endpoint.
     * @param jsonParams  JSON parameters to be sent in the request body.
     * @param bearerToken API token to access endpoint.
     * @param userName    Username for basic authentication.
     * @param password    Password for basic authentication.
     */
    public static HttpResponse<String> sendPostRequest(String url, String jsonParams, String bearerToken, String userName, String password) {
        try {
            HttpRequest.Builder request = HttpRequest.newBuilder()
                    .header("Content-Type", "application/json")
                    // URL
                    .uri(URI.create(url))
                    .POST(HttpRequest.BodyPublishers.ofString(jsonParams))
                    .timeout(Duration.ofSeconds(3600));
            if (bearerToken != null) {
                request.header(AUTHORISATION_HEADER, "Bearer " + bearerToken);
            } else if (userName != null && password != null) {
                request.header(AUTHORISATION_HEADER, getBasicAuthenticationHeader(userName, password));
            }
            // Await response before continue executing the rest of the code
            return HTTP_CLIENT.send(request.build(), HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            LOGGER.fatal(BAD_REQUEST_ERROR, e);
            throw new JPSRuntimeException(BAD_REQUEST_ERROR, e);
        } catch (InterruptedException e) {
            LOGGER.fatal(INTERRUPTED_ERROR, e);
            Thread.currentThread().interrupt();
            throw new JPSRuntimeException(INTERRUPTED_ERROR, e);
        }
    }

    /**
     * Formats the username and password for a basic authentication header.
     *
     * @param username Username credentials for request.
     * @param password Password credentials for request.
     */
    private static String getBasicAuthenticationHeader(String username, String password) {
        String valueToEncode = username + ":" + password;
        return "Basic " + Base64.getEncoder().encodeToString(valueToEncode.getBytes());
    }
}
