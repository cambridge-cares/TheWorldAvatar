package com.cmclinnovations.stack.clients.grafana;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class GrafanaEndpointConfig extends PasswordEndpointConfig {
    private final String HOST_NAME;
    private final String PORT;
    private final String USERNAME;
    private static final String DEFAULT_USER = "admin";

    public GrafanaEndpointConfig(String name, String hostName, String port, String passwordFile) {
        this(name, hostName, port, "", passwordFile);
    }

    public GrafanaEndpointConfig(String name, String hostName, String port, String username, String passwordFile) {
        super(name, passwordFile);
        this.HOST_NAME = hostName;
        this.PORT = port;
        this.USERNAME = username.isEmpty() ? DEFAULT_USER : username;
    }

    public String getHostName() {
        return this.HOST_NAME;
    }

    public String getPort() {
        return this.PORT;
    }

    public String getUsername() {
        return this.USERNAME;
    }

    @JsonIgnore
    public String getServiceUrl() {
        return "http://" + HOST_NAME + ":" + PORT;
    }

    /**
     * Retrieves the service url to perform requests for creating, updating, deleting, and searching of service accounts.
     * Please refer to the Grafana HTTP API documents for the specific sub routes and their actions.
     */
    public String getServiceAccountServiceUrl() {return getServiceUrl() + "/api/serviceaccounts";}

    /**
     * Retrieves the service url to perform requests for creating, updating, deleting, and searching of data sources.
     * Please refer to the Grafana HTTP API documents for the specific sub routes and their actions.
     */
    public String getDataSourceServiceUrl() {return getServiceUrl() + "/api/datasources";}

    /**
     * Retrieves the service url to perform requests for creating, updating, deleting, and searching of the dashboards.
     * Please refer to the Grafana HTTP API documents for the specific sub routes and their actions.
     */
    public String getDashboardServiceUrl() {return getServiceUrl() + "/api/dashboards/db";}
}