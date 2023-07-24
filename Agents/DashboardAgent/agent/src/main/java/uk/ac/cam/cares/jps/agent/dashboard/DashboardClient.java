package uk.ac.cam.cares.jps.agent.dashboard;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.net.http.HttpResponse;

/**
 * A client that interacts with the dashboard container to set it up.
 *
 * @author qhouyee
 */
public class DashboardClient {
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);
    private final StackClient SERVICE_CLIENT;
    private static final String SERVICE_ACCOUNT_ROUTE = "/api/serviceaccounts";
    private static final String DASHBOARD_UNAVAILABLE_ERROR = "Dashboard container has not been set up within the stack. Please set it up first!";

    /**
     * Standard Constructor.
     */
    public DashboardClient(StackClient serviceClient) {
        this.SERVICE_CLIENT = serviceClient;
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
    }

    /**
     * Creates a service account for creating connections and dashboards. Those requests require API keys generated from these service accounts.
     */
    private void createServiceAccount() {
        LOGGER.info("Creating service account...");
        String route = this.SERVICE_CLIENT.getDashboardUrl() + SERVICE_ACCOUNT_ROUTE;
        String params = "{ \"name\": \"grafana\", \"role\": \"Admin\", \"isDisabled\" : false}";
        this.SERVICE_CLIENT.sendPostRequest(route, params);
        LOGGER.info("Service account has been successfully created...");
    }
}
