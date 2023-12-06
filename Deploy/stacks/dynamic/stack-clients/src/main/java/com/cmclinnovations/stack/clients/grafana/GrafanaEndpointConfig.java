package com.cmclinnovations.stack.clients.grafana;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class GrafanaEndpointConfig extends PasswordEndpointConfig {
    private final String hostName;
    private final String port;
    private final String username;
    private final String serviceUrl;

    protected GrafanaEndpointConfig() {
        this(null, null, null, null, null);
    }

    public GrafanaEndpointConfig(String name, String hostName, String port, String username, String passwordFile) {
        super(name, passwordFile);
        this.hostName = hostName;
        this.port = port;
        this.username = username;
        this.serviceUrl = "http://" + this.hostName + ":" + this.port;
    }

    public String getHostName() {
        return hostName;
    }

    public String getPort() {
        return port;
    }

    public String getUsername() {
        return username;
    }

    public String getServiceUrl() {
        return serviceUrl;
    }

    /**
     * Retrieves the service url to perform requests for creating, updating, deleting, and searching of service accounts.
     * Please refer to the Grafana HTTP API documents for the specific sub routes and their actions.
     */
    @JsonIgnore
    public String getServiceAccountServiceUrl() {return getServiceUrl() + "/api/serviceaccounts";}

    /**
     * Retrieves the service url to perform requests for creating, updating, deleting, and searching of data sources.
     * Please refer to the Grafana HTTP API documents for the specific sub routes and their actions.
     */
    @JsonIgnore
    public String getDataSourceServiceUrl() {return getServiceUrl() + "/api/datasources";}

    /**
     * Retrieves the service url to perform requests for creating, updating, deleting, and searching of the dashboards.
     * Please refer to the Grafana HTTP API documents for the specific sub routes and their actions.
     */
    @JsonIgnore
    public String getDashboardServiceUrl() {return getServiceUrl() + "/api/dashboards/db";}
}