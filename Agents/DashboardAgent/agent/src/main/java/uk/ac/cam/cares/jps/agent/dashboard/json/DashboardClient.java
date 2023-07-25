package uk.ac.cam.cares.jps.agent.dashboard.json;

import com.google.gson.Gson;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.agent.dashboard.stack.StackClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.net.http.HttpResponse;
import java.util.HashMap;
import java.util.List;

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
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);
    private static final String CONNECTION_NAME_PREFIX = "Postgis";
    private static final String SERVICE_ACCOUNT_ROUTE = "/api/serviceaccounts";
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
        HttpResponse response = this.SERVICE_CLIENT.sendGetRequest(this.SERVICE_CLIENT.getDashboardUrl());
        if (response.statusCode() != 200) {
            LOGGER.fatal(DASHBOARD_UNAVAILABLE_ERROR);
            throw new JPSRuntimeException(DASHBOARD_UNAVAILABLE_ERROR);
        }
    }

    /**
     * Initialise a new dashboard through HTTP API.
     */
    public void initDashboard() {
        this.createServiceAccount();
        this.createDataSources();
        this.createDashboard();
    }

    /**
     * Creates a service account for creating connections and dashboards. Those requests require API keys generated from these service accounts.
     */
    private void createServiceAccount() {
        LOGGER.info("Creating service account...");
        String route = this.SERVICE_CLIENT.getDashboardUrl() + SERVICE_ACCOUNT_ROUTE;
        String params = "{ \"name\": \"grafana\", \"role\": \"Admin\", \"isDisabled\" : false}";
        // Create a new service account
        HttpResponse response = this.SERVICE_CLIENT.sendPostRequest(route, params, this.DASHBOARD_ACCOUNT_USER, this.DASHBOARD_ACCOUNT_PASSWORD);
        LOGGER.info("Generating a new token...");
        // Retrieve the account ID to facilitate token creation process
        HashMap<String, Object> responseMap = transformToMap(response.body().toString());
        // ID is in Double format due to how GSON parses it's number
        // For our use case, we require it to be transformed into a non-decimal number
        Double idDoubleFormat = (Double) responseMap.get("id");
        int accountId = idDoubleFormat.intValue();
        // ID must be appended to the route in the following syntax
        route = route + "/" + accountId + "/tokens";
        // Generate a new token
        response = this.SERVICE_CLIENT.sendPostRequest(route, params, this.DASHBOARD_ACCOUNT_USER, this.DASHBOARD_ACCOUNT_PASSWORD);
        responseMap = transformToMap(response.body().toString());
        this.SERVICE_ACCOUNT_TOKEN = responseMap.get("key").toString();
        LOGGER.debug("Token for service account has been successfully generated!");
    }

    /**
     * Creates all the connections from the dashboard to the PostGIS database.
     */
    private void createDataSources() {
        LOGGER.info("Creating data source connections...");
        String route = this.SERVICE_CLIENT.getDashboardUrl() + DATA_SOURCE_ROUTE;
        List<String> dbList = this.SERVICE_CLIENT.getDatabaseNames();
        int counter = 1;
        for (String database : dbList) {
            LOGGER.debug("Creating connection to " + database + "...");
            // Generate a source name with prefix and a counter number
            String sourceName = CONNECTION_NAME_PREFIX + counter++; // The counter will increase after execution
            String[] credentials = this.SERVICE_CLIENT.getPostGisCredentials();
            // Format the syntax into valid json
            PostgresDataSource source = new PostgresDataSource(sourceName, credentials[0], credentials[1], credentials[2], database);
            // Execute request to create new connection
            HttpResponse response = this.SERVICE_CLIENT.sendPostRequest(route, source.construct(), this.SERVICE_ACCOUNT_TOKEN);
            if (response.statusCode() != 200) {
                LOGGER.fatal(FAILED_REQUEST_ERROR + response.body());
                throw new JPSRuntimeException(FAILED_REQUEST_ERROR + response.body());
            }
            HashMap<String, Object> responseMap = transformToMap(response.body().toString());
            LOGGER.info(response.body());
        }
        LOGGER.debug("All connections have been successfully established!");
    }

    /**
     * Create the dashboard required.
     */
    private void createDashboard() {
        LOGGER.info("Initialising a new dashboard...");
        String route = this.SERVICE_CLIENT.getDashboardUrl() + DASHBOARD_CREATION_ROUTE;
        // Generate JSON model syntax
        String jsonSyntax = new GrafanaModel().construct();
        // Create a new dashboard based on the JSON model using a POST request with security token
        HttpResponse response = this.SERVICE_CLIENT.sendPostRequest(route, jsonSyntax, this.SERVICE_ACCOUNT_TOKEN);
        // WIP: Retrieve the required information to form the URL
        // URL key is available as /EXPOSED_URL_NAME/d/DASHBOARD_ID/DASHBOARD_TITLE
        HashMap<String, Object> responseMap = transformToMap(response.body().toString());
        String dashboardId = responseMap.get("uid").toString();
        String dashboardTitle = responseMap.get("slug").toString();
    }

    /**
     * Transform a JSON object in string format into a Hashmap.
     *
     * @param jsonString A JSON object in String object.
     */
    private HashMap<String, Object> transformToMap(String jsonString) {
        Gson gson = new Gson();
        return gson.fromJson(jsonString, HashMap.class);
    }
}
