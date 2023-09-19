package uk.ac.cam.cares.jps.agent.dashboard.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.agent.dashboard.stack.StackClient;
import uk.ac.cam.cares.jps.agent.dashboard.utils.AgentCommunicationClient;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.net.http.HttpResponse;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * A client that interacts with the dashboard container to set it up.
 *
 * @author qhouyee
 */
public class DashboardClient {
    private String SERVICE_ACCOUNT_TOKEN;
    private final String DASHBOARD_ACCOUNT_USER;
    private final String DASHBOARD_ACCOUNT_PASSWORD;
    private final StackClient SERVICE_CLIENT;
    private final Map<String, String> DATABASE_CONNECTION_MAP = new HashMap<>();
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);
    private static final String CONNECTION_NAME_PREFIX = "Postgis";
    private static final String SERVICE_ACCOUNT_ROUTE = "/api/serviceaccounts";
    private static final String SERVICE_ACCOUNT_SEARCH_SUB_ROUTE = "/search";
    private static final String DATA_SOURCE_ROUTE = "/api/datasources";
    private static final String DASHBOARD_CREATION_ROUTE = "/api/dashboards/db";
    private static final String DASHBOARD_UNAVAILABLE_ERROR = "Dashboard container has not been set up within the stack. Please set it up first!";
    private static final String FAILED_REQUEST_ERROR = "Unable to send request! See response for more details: ";

    /**
     * Standard Constructor.
     */
    public DashboardClient(StackClient serviceClient, String dashboardContainerUsername, String dashboardContainerPassword) {
        this.SERVICE_CLIENT = serviceClient;
        this.DASHBOARD_ACCOUNT_USER = dashboardContainerUsername;
        this.DASHBOARD_ACCOUNT_PASSWORD = dashboardContainerPassword;
        // Verify if the dashboard container has been set up, and throws an error if not
        // A GET request to the endpoint should return a valid status code with an HTML file
        HttpResponse response = AgentCommunicationClient.sendGetRequest(this.SERVICE_CLIENT.getDashboardUrl(), this.DASHBOARD_ACCOUNT_USER, this.DASHBOARD_ACCOUNT_PASSWORD);
        AgentCommunicationClient.verifySuccessfulRequest(response, DASHBOARD_UNAVAILABLE_ERROR);
    }

    /**
     * Initialise a new dashboard through HTTP API.
     */
    public void initDashboard() {
        this.createServiceAccountToken();
        this.createDataSources();
        // For each spatial zone, a separate dashboard should be generated
        String[] spatialZoneArray = this.SERVICE_CLIENT.getAllSpatialZones();
        for (String spatialZone : spatialZoneArray) {
            this.createDashboard(spatialZone);
        }
    }

    /**
     * Creates a service account token for creating connections and dashboards.
     */
    private void createServiceAccountToken() {
        LOGGER.info("Checking for valid service accounts...");
        String route = this.SERVICE_CLIENT.getDashboardUrl() + SERVICE_ACCOUNT_ROUTE;
        HttpResponse response = AgentCommunicationClient.sendGetRequest(route + SERVICE_ACCOUNT_SEARCH_SUB_ROUTE, this.DASHBOARD_ACCOUNT_USER, this.DASHBOARD_ACCOUNT_PASSWORD);
        JsonObject responseBody = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonObject();
        // This should return a JSON array of objects, which may or may not have any existing accounts
        JsonArray accountInfo = responseBody.get("serviceAccounts").getAsJsonArray();
        int accountId;
        // Verify if there are any service accounts, and ensure that one of these accounts are equivalent to this agent's service account name
        if (accountInfo.size() > 0 && accountInfo.get(0).getAsJsonObject().get("name").getAsString().equals(StringHelper.SERVICE_ACCOUNT_NAME)) {
            LOGGER.info("Valid service account detected...");
            // Retrieve the account ID as int to make it easier to generate the token
            accountId = accountInfo.get(0).getAsJsonObject().get("id").getAsInt();
        } else {
            LOGGER.info("No valid account detected! Creating a new service account...");
            // Create a new service account
            String params = "{ \"name\": \"" + StringHelper.SERVICE_ACCOUNT_NAME + "\", \"role\": \"Admin\", \"isDisabled\" : false}";
            response = AgentCommunicationClient.sendPostRequest(route, params, this.DASHBOARD_ACCOUNT_USER, this.DASHBOARD_ACCOUNT_PASSWORD);
            // Retrieve the account ID as int to facilitate token creation process
            responseBody = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonObject();
            accountId = responseBody.get("id").getAsInt();
        }
        LOGGER.info("Generating a new token...");
        // ID must be appended to the route in the following syntax
        route = route + "/" + accountId + "/tokens";
        // Generate a new token with randomised name
        String params = "{ \"name\": \"" + UUID.randomUUID() + "\", \"role\": \"Admin\", \"isDisabled\" : false}";
        response = AgentCommunicationClient.sendPostRequest(route, params, this.DASHBOARD_ACCOUNT_USER, this.DASHBOARD_ACCOUNT_PASSWORD);
        // Retrieve the key from the response
        responseBody = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonObject();
        this.SERVICE_ACCOUNT_TOKEN = responseBody.get("key").getAsString();
        LOGGER.debug("Token for the service account has been successfully generated!");
    }

    /**
     * Creates all the connections from the dashboard to the PostGIS database.
     */
    private void createDataSources() {
        LOGGER.info("Retrieving existing data source connections if any...");
        String route = this.SERVICE_CLIENT.getDashboardUrl() + DATA_SOURCE_ROUTE;
        // Retrieves any existing data connections if available
        HttpResponse response = AgentCommunicationClient.sendGetRequest(route, this.SERVICE_ACCOUNT_TOKEN);
        JsonArray responseArray = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonArray();
        // A data source map to make it easier to check for its existence
        Map<String, String> existingDataSources = new HashMap<>();
        // For each existing data source,
        for (JsonElement dataSource : responseArray) {
            if (dataSource.isJsonObject()) {
                JsonObject dataSourceObject = dataSource.getAsJsonObject();
                String url = dataSourceObject.get("url").getAsString();
                String databaseName = dataSourceObject.get("database").getAsString();
                String databaseConnectionID = dataSourceObject.get("uid").getAsString();
                // Store their name and url for checking if this database does not need to be reconnected in the dashboard
                existingDataSources.put(databaseName, url);
                // Store their existing metadata for generating the json model syntax
                this.DATABASE_CONNECTION_MAP.put(databaseName, databaseConnectionID);
                LOGGER.debug("Detected existing connection to " + databaseName);
            }
        }
        LOGGER.info("Retrieving RDB databases...");
        List<String> dbList = this.SERVICE_CLIENT.getDatabaseNames();
        // For each database, verify if they exist and create their dashboard connection when required
        for (String database : dbList) {
            String[] credentials = this.SERVICE_CLIENT.getPostGisCredentials();
            // If the existing data connections does not include this database or the url does not match it,
            // Create a new connection and store its metadata
            if (!existingDataSources.containsKey(database) || !existingDataSources.get(database).equals(credentials[0])) {
                LOGGER.debug("Creating connection to " + database + "...");
                // Generate a source name with prefix and a random uuid
                String sourceName = CONNECTION_NAME_PREFIX + UUID.randomUUID();
                // Format the syntax into valid json
                PostgresDataSource source = new PostgresDataSource(sourceName, credentials[0], credentials[1], credentials[2], database);
                // Execute request to create new connection
                response = AgentCommunicationClient.sendPostRequest(route, source.construct(), this.SERVICE_ACCOUNT_TOKEN);
                AgentCommunicationClient.verifySuccessfulRequest(response, FAILED_REQUEST_ERROR + response.body());
                // Retrieve the connection ID generated for the database connection and link it to the database
                JsonObject responseBody = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonObject().get("datasource").getAsJsonObject();
                String databaseConnectionID = responseBody.get("uid").getAsString();
                String databaseName = responseBody.get("database").getAsString();
                this.DATABASE_CONNECTION_MAP.put(databaseName, databaseConnectionID);
            }
        }
        LOGGER.debug("All connections have been successfully established!");
    }

    /**
     * Create the dashboard required for the specified spatial zone.
     *
     * @param spatialZone The name of the spatial zone.
     */
    private void createDashboard(String spatialZone) {
        LOGGER.info("Initialising a new dashboard...");
        String route = this.SERVICE_CLIENT.getDashboardUrl() + DASHBOARD_CREATION_ROUTE;
        // Generate title
        String title = "Overview for " + spatialZone;
        // Retrieve all time series for the model
        Map<String, Map<String, List<String[]>>> timeSeries = this.SERVICE_CLIENT.getAllTimeSeries(spatialZone);
        LOGGER.debug("Generating JSON model syntax...");
        String jsonSyntax;
        try {
            jsonSyntax = new GrafanaModel(title, this.DATABASE_CONNECTION_MAP, timeSeries).construct();
        } catch (Exception e) {
            LOGGER.fatal("Failed to construct grafana model syntax. See error message for more details: " + e.getMessage());
            throw new JPSRuntimeException("Failed to construct grafana model syntax. See error message for more details: " + e.getMessage());
        }
        LOGGER.debug("Sending request to create dashboard...");
        // Create a new dashboard based on the JSON model using a POST request with security token
        AgentCommunicationClient.sendPostRequest(route, jsonSyntax, this.SERVICE_ACCOUNT_TOKEN);
    }
}
