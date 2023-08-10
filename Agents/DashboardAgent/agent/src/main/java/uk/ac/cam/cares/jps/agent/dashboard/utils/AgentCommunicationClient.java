package uk.ac.cam.cares.jps.agent.dashboard.utils;

import com.google.gson.Gson;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

/**
 * A class that provides methods to facilitate communication between agents through handling requests and responses.
 *
 * @author qhouyee
 */
public class AgentCommunicationClient {
    private static final HttpClient HTTP_CLIENT = HttpClient.newHttpClient();
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);

    /**
     * Verifies the success of the request, which should return status code of 200.
     * If unsuccessful, agent will stop processing the request.
     *
     * @param response     The response returned from the API.
     * @param errorMessage Error message to display if unsuccessful.
     */
    public static void verifySuccessfulRequest(HttpResponse response, String errorMessage) {
        if (response.statusCode() != 200) {
            LOGGER.fatal(errorMessage);
            throw new JPSRuntimeException(errorMessage);
        }
    }

    /**
     * Retrieves the body content of the response (usually in JSON), and process it into a Java Map.
     *
     * @param response The response returned from the API.
     * @return The response body as a map for easy access.
     */
    public static Map<String, Object> retrieveResponseBodyAsMap(HttpResponse response) {
        Gson gson = new Gson();
        // Although the method returns the result as a HashMap, it can be abstracted to its parent Map object
        return gson.fromJson(response.body().toString(), HashMap.class);
    }

    /**
     * Sends a GET request to a specific API endpoint without username or password.
     *
     * @param url API endpoint.
     */
    public static HttpResponse sendGetRequest(String url) {
        return sendGetRequest(url, "", "");
    }

    /**
     * Sends a GET request to a specific API endpoint with the username and password.
     *
     * @param url      API endpoint.
     * @param userName Username to access endpoint.
     * @param password Password to access endpoint.
     */
    public static HttpResponse sendGetRequest(String url, String userName, String password) {
        try {
            HttpRequest.Builder requestBuilder = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .GET()
                    .timeout(Duration.ofSeconds(3600));
            // If username and password are provided, add the authentication
            if (!userName.isEmpty() && !password.isEmpty())
                requestBuilder.header("Authorization", getBasicAuthenticationHeader(userName, password));
            // Await response before continue executing the rest of the code
            return HTTP_CLIENT.send(requestBuilder.build(), HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            LOGGER.fatal("Unable to connect or send request. Please ensure the url is valid. If valid, check the message for more details: " + e.getMessage());
            throw new JPSRuntimeException("Unable to connect or send request. Please ensure the url is valid. If valid, check the message for more details: " + e.getMessage());
        } catch (InterruptedException e) {
            LOGGER.fatal("Thread has been interrupted! " + e.getMessage());
            throw new JPSRuntimeException("Thread has been interrupted! " + e.getMessage());
        }
    }

    /**
     * An overloaded method that sends a POST request with JSON parameters to a specific API endpoint authenticated with security tokens.
     *
     * @param url        API endpoint.
     * @param jsonParams JSON parameters to be sent in the request body.
     */
    public static HttpResponse sendPostRequest(String url, String jsonParams, String bearerToken) {
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .header("Content-Type", "application/json")
                    // Authorization for accessing the endpoint
                    .header("Authorization", "Bearer " + bearerToken)
                    // URL
                    .uri(URI.create(url))
                    .POST(HttpRequest.BodyPublishers.ofString(jsonParams))
                    .timeout(Duration.ofSeconds(3600))
                    .build();
            // Await response before continue executing the rest of the code
            return HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            LOGGER.fatal("Unable to connect or send request. Please ensure the url is valid. If valid, check the message for more details: " + e.getMessage());
            throw new JPSRuntimeException("Unable to connect or send request. Please ensure the url is valid. If valid, check the message for more details: " + e.getMessage());
        } catch (InterruptedException e) {
            LOGGER.fatal("Thread has been interrupted! " + e.getMessage());
            throw new JPSRuntimeException("Thread has been interrupted! " + e.getMessage());
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
    public static HttpResponse sendPostRequest(String url, String jsonParams, String userName, String password) {
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .header("Content-Type", "application/json")
                    // Authorization for accessing the endpoint
                    .header("Authorization", getBasicAuthenticationHeader(userName, password))
                    // URL
                    .uri(URI.create(url))
                    .POST(HttpRequest.BodyPublishers.ofString(jsonParams))
                    .timeout(Duration.ofSeconds(3600))
                    .build();
            // Await response before continue executing the rest of the code
            return HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            LOGGER.fatal("Unable to connect or send request. Please ensure the url is valid. If valid, check the message for more details: " + e.getMessage());
            throw new JPSRuntimeException("Unable to connect or send request. Please ensure the url is valid. If valid, check the message for more details: " + e.getMessage());
        } catch (InterruptedException e) {
            LOGGER.fatal("Thread has been interrupted! " + e.getMessage());
            throw new JPSRuntimeException("Thread has been interrupted! " + e.getMessage());
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
