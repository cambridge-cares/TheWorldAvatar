package com.cmclinnovations.stack.clients.grafana;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class GrafanaEndpointConfig extends PasswordEndpointConfig {

    private final String hostName;
    private final String port;
    private final String username;

    protected GrafanaEndpointConfig() {
        this(null, null, null, null, null);
    }

    public GrafanaEndpointConfig(String name, String hostName, String port, String username, String passwordFile) {
        super(name, passwordFile);
        this.hostName = hostName;
        this.port = port;
        this.username = username;
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
}