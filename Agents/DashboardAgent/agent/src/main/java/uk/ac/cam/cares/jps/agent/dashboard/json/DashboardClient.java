package uk.ac.cam.cares.jps.agent.dashboard.json;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.stack.StackClient;
import uk.ac.cam.cares.jps.agent.dashboard.utils.AgentCommunicationClient;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.net.http.HttpResponse;
import java.util.*;

/**
 * A client that interacts with the dashboard container to set it up.
 *
 * @author qhouyee
 */
public class DashboardClient {
    private String serviceAccountToken;
    private final StackClient serviceClient;
    private final Map<String, String> databaseConnectionMap = new HashMap<>();
    private static final String CONNECTION_NAME_PREFIX = "Postgis";
    private static final String SERVICE_ACCOUNT_ROUTE = "/api/serviceaccounts";
    private static final String SERVICE_ACCOUNT_SEARCH_SUB_ROUTE = "/search";
    private static final String DATA_SOURCE_ROUTE = "/api/datasources";
    private static final String DASHBOARD_CREATION_ROUTE = "/api/dashboards/db";
    private static final String DASHBOARD_SEARCH_ROUTE = "/api/search?folderIds=0&query=&starred=false";
    private static final String DASHBOARD_UNAVAILABLE_ERROR = "Dashboard container has not been set up within the stack. Please set it up first!";
    private static final String FAILED_REQUEST_ERROR = "Unable to send request! See response for more details: ";
    private static final String INVALID_DATA_ERROR = "Bad request data! The json model is not compliant with Grafana standards!";
    private static final Logger LOGGER = LogManager.getLogger(DashboardClient.class);

    /**
     * Standard Constructor.
     */
    public DashboardClient(StackClient serviceClient) {
        this.serviceClient = serviceClient;
        // Verify if the dashboard container has been set up, and throws an error if not
        // A GET request to the endpoint should return a valid status code with an HTML file
        HttpResponse<String> response = AgentCommunicationClient.sendGetRequest(this.serviceClient.getDashboardUrl(), this.serviceClient.getDashboardUser(), this.serviceClient.getDashboardPassword());
        AgentCommunicationClient.verifySuccessfulRequest(response, DASHBOARD_UNAVAILABLE_ERROR);
    }

    /**
     * Initialise a new dashboard through HTTP API.
     */
    public Queue<String> initDashboard() {
        this.createServiceAccountToken();
        this.createDataSources();
        // For each organisation, a separate dashboard should be generated
        String[] orgArray = this.serviceClient.getAllOrganisations();
        Queue<String> dashboardUids = new ArrayDeque<>();
        for (String organisation : orgArray) {
            String uid = this.createDashboard(organisation);
            dashboardUids.offer(uid);
        }
        return dashboardUids;
    }

    /**
     * Creates a service account token for creating connections and dashboards.
     */
    private void createServiceAccountToken() {
        LOGGER.info("Checking for valid service accounts...");
        String route = this.serviceClient.getDashboardUrl() + SERVICE_ACCOUNT_ROUTE;
        String dashboardAccountUser = this.serviceClient.getDashboardUser();
        String dashboardAccountPassword = this.serviceClient.getDashboardPassword();
        HttpResponse<String> response = AgentCommunicationClient.sendGetRequest(route + SERVICE_ACCOUNT_SEARCH_SUB_ROUTE, dashboardAccountUser, dashboardAccountPassword);
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
            response = AgentCommunicationClient.sendPostRequest(route, params, dashboardAccountUser, dashboardAccountPassword);
            // Retrieve the account ID as int to facilitate token creation process
            responseBody = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonObject();
            accountId = responseBody.get("id").getAsInt();
        }
        LOGGER.info("Generating a new token...");
        // ID must be appended to the route in the following syntax
        route = route + "/" + accountId + "/tokens";
        // Generate a new token with randomised name
        String params = "{ \"name\": \"" + UUID.randomUUID() + "\", \"role\": \"Admin\", \"isDisabled\" : false}";
        response = AgentCommunicationClient.sendPostRequest(route, params, dashboardAccountUser, dashboardAccountPassword);
        // Retrieve the key from the response
        responseBody = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonObject();
        this.serviceAccountToken = responseBody.get("key").getAsString();
        LOGGER.debug("Token for the service account has been successfully generated!");
    }

    /**
     * Creates all the connections from the dashboard to the PostGIS database.
     */
    private void createDataSources() {
        LOGGER.info("Retrieving existing data source connections if any...");
        String route = this.serviceClient.getDashboardUrl() + DATA_SOURCE_ROUTE;
        // Retrieves any existing data connections if available
        HttpResponse<String> response = AgentCommunicationClient.sendGetRequest(route, this.serviceAccountToken);
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
                this.databaseConnectionMap.put(databaseName, databaseConnectionID);
                LOGGER.debug("Detected existing connection to {}", databaseName);
            }
        }
        LOGGER.info("Retrieving RDB databases...");
        List<String> dbList = this.serviceClient.getDatabaseNames();
        String rdbDomainUrl = this.serviceClient.getRdbDomain();
        String rdbUsername = this.serviceClient.getRdbUser();
        String rdbPassword = this.serviceClient.getRdbPassword();
        // For each database, verify if they exist and create their dashboard connection when required
        for (String database : dbList) {
            // If the existing data connections does not include this database or the url does not match it,
            // Create a new connection and store its metadata
            if (!existingDataSources.containsKey(database) || !existingDataSources.get(database).equals(rdbDomainUrl)) {
                LOGGER.debug("Creating connection to {}...", database);
                // Generate a source name with prefix and a random uuid
                String sourceName = CONNECTION_NAME_PREFIX + UUID.randomUUID();
                // Format the syntax into valid json
                PostgresDataSource source = new PostgresDataSource(sourceName, rdbDomainUrl, rdbUsername, rdbPassword, database);
                // Execute request to create new connection
                response = AgentCommunicationClient.sendPostRequest(route, source.construct(), this.serviceAccountToken);
                AgentCommunicationClient.verifySuccessfulRequest(response, FAILED_REQUEST_ERROR + response.body());
                // Retrieve the connection ID generated for the database connection and link it to the database
                JsonObject responseBody = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonObject().get("datasource").getAsJsonObject();
                String databaseConnectionID = responseBody.get("uid").getAsString();
                String databaseName = responseBody.get("database").getAsString();
                this.databaseConnectionMap.put(databaseName, databaseConnectionID);
            }
        }
        LOGGER.debug("All connections have been successfully established!");
    }

    /**
     * Create the dashboard required for the specified organisation and its facilities.
     *
     * @param organisation The name of the organisation.
     */
    private String createDashboard(String organisation) {
        LOGGER.info("Initialising a new dashboard...");
        String route = this.serviceClient.getDashboardUrl() + DASHBOARD_CREATION_ROUTE;
        // Generate title
        String title = "Overview for " + organisation;
        // Retrieve all time series for the model
        Map<String, Map<String, List<String[]>>> timeSeries = this.serviceClient.getAllTimeSeries(organisation);
        LOGGER.debug("Generating JSON model syntax...");
        String jsonSyntax;
        try {
            GrafanaModel dataModel = new GrafanaModel(title, this.databaseConnectionMap, timeSeries);
            dataModel = this.overwriteExistingModel(title, dataModel);
            jsonSyntax = dataModel.construct();
        } catch (Exception e) {
            LOGGER.fatal("Failed to construct grafana model syntax. See error message for more details: ", e);
            throw new JPSRuntimeException("Failed to construct grafana model syntax. See error message for more details: ", e);
        }
        LOGGER.debug("Sending request to create dashboard...");
        // Create a new dashboard based on the JSON model using a POST request with security token
        HttpResponse<String> response = AgentCommunicationClient.sendPostRequest(route, jsonSyntax, this.serviceAccountToken);
        JsonObject responseBody = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonObject();
        try {
            AgentCommunicationClient.verifySuccessfulRequest(response, FAILED_REQUEST_ERROR + responseBody);
        } catch (IllegalArgumentException e) {
            LOGGER.fatal(INVALID_DATA_ERROR);
            throw new IllegalArgumentException(INVALID_DATA_ERROR);
        }
        // Retrieve the connection ID generated for the database connection and link it to the database
        return responseBody.get("uid").getAsString();
    }

    /**
     * Overwrite the existing dashboard with a different model syntax if required. This method will be ignored if there are no dashboards with the same title.
     *
     * @param title     The title of the data model.
     * @param dataModel The Grafana data model containing the dashboard format.
     */
    private GrafanaModel overwriteExistingModel(String title, GrafanaModel dataModel) {
        String searchRoute = this.serviceClient.getDashboardUrl() + DASHBOARD_SEARCH_ROUTE;
        HttpResponse<String> response = AgentCommunicationClient.sendGetRequest(searchRoute, this.serviceAccountToken);
        JsonArray dashboards = AgentCommunicationClient.retrieveResponseBody(response).getAsJsonArray();
        for (JsonElement dashboard : dashboards) {
            JsonObject dashboardData = dashboard.getAsJsonObject();
            // Only overwrite the existing model if the dashboard title already exists
            if (dashboardData.get("title").getAsString().equals(title)) {
                int id = dashboardData.get("id").getAsInt();
                String uid = dashboardData.get("uid").getAsString();
                dataModel.setExistingIds(id, uid);
                break;
            }
        }
        return dataModel;
    }
}
