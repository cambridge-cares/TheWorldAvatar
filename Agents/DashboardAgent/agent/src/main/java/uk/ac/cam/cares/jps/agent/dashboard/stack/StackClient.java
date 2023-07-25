package uk.ac.cam.cares.jps.agent.dashboard.stack;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
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
import java.util.List;

/**
 * A client that checks if the agent is running on a stack and retrieve the required endpoints.
 *
 * @author qhouyee
 */
public class StackClient {
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);
    private final String STACK_SPARQL_ENDPOINT;
    private final String STACK_RDB_DOMAIN;
    private final String STACK_JDBC_URL;
    private final String DASHBOARD_URL;
    private final HttpClient HTTP_CLIENT = HttpClient.newHttpClient();
    private final PostGisClient POSTGIS_CLIENT;

    /**
     * Standard Constructor.
     */
    public StackClient() {
        LOGGER.debug("Attempting to retrieve services from the stack...");
        ContainerClient client = new ContainerClient();
        BlazegraphEndpointConfig blazeConfig = client.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class);
        PostGISEndpointConfig postConfig = client.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        this.STACK_SPARQL_ENDPOINT = "http://" + blazeConfig.getHostName() + ":" + blazeConfig.getPort() + "/blazegraph/namespace/";
        this.STACK_RDB_DOMAIN = postConfig.getHostName() + ":" + postConfig.getPort();
        this.STACK_JDBC_URL = "jdbc:postgresql://" + this.STACK_RDB_DOMAIN + "/";
        this.POSTGIS_CLIENT = new PostGisClient(this.STACK_JDBC_URL, postConfig.getUsername(), postConfig.getPassword());
        // Note that the container name and port number is dependent on the custom setup - This may change when we have a built-in container for grafana
        this.DASHBOARD_URL = "http://" + getStackNameFromHost(blazeConfig.getHostName()) + "-grafana:3000";
        LOGGER.debug("Services have been successfully retrieved from the stack...");
    }

    /**
     * Get the stack name from the container's host name in the stack.
     *
     * @param hostName The container's host name.
     */
    private static String getStackNameFromHost(String hostName) {
        // Host names for stack's SPARQL endpoint are usually in the format: stackName-blazegraph
        // Code here retrieves stackName
        int stackIndex = hostName.lastIndexOf("-blazegraph");
        // If the host name does not exit, it probably is invalid
        if (stackIndex != -1) {
            return hostName.substring(0, stackIndex);
        }
        LOGGER.fatal("Invalid host name! Please ensure the stack container name is correct in: " + hostName);
        throw new JPSRuntimeException("Invalid host name! Please ensure the container name is correct in: " + hostName);
    }

    /**
     * Get the SPARQL endpoint within this stack.
     *
     * @param stackNamespace The stack namespace of interest.
     * @return The SPARQL endpoint and namespace to query from.
     */
    public String getEndpoint(String stackNamespace) {
        return this.STACK_SPARQL_ENDPOINT + stackNamespace + "/sparql";
    }

    /**
     * Get the list of database names that is available in this stack.
     */
    public List<String> getDatabaseNames() {
        return this.POSTGIS_CLIENT.getDatabaseNames();
    }

    /**
     * Get the PostGIS credentials for the dashboard.
     *
     * @return An array containing the credentials in sequence of domain name, username, and password.
     */
    public String[] getPostGisCredentials() {
        String[] credentials = new String[3];
        credentials[0] = this.STACK_RDB_DOMAIN;
        credentials[1] = this.POSTGIS_CLIENT.getUsername();
        credentials[2] = this.POSTGIS_CLIENT.getPassword();
        return credentials;
    }


    /**
     * Get the dashboard service within this stack.
     */
    public String getDashboardUrl() {
        return this.DASHBOARD_URL;
    }

    /**
     * Sends a GET request to a specific API endpoint.
     *
     * @param url API endpoint.
     */
    public HttpResponse sendGetRequest(String url) {
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .GET()
                    .timeout(Duration.ofSeconds(3600))
                    .build();
            // Await response before continue executing the rest of the code
            return this.HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
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
    public HttpResponse sendPostRequest(String url, String jsonParams, String bearerToken) {
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
            return this.HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
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
    public HttpResponse sendPostRequest(String url, String jsonParams, String userName, String password) {
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
            return this.HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
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
    private String getBasicAuthenticationHeader(String username, String password) {
        String valueToEncode = username + ":" + password;
        return "Basic " + Base64.getEncoder().encodeToString(valueToEncode.getBytes());
    }
}
