package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.grafana.GrafanaEndpointConfig;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public class GrafanaService extends ContainerService {
    public static final String TYPE = "grafana";
    private static final String PASSWORD_FILE_KEY = "GRAFANA_PASSWORD_FILE";
    private static final String PASSWORD_SECRET_NAME = "grafana_password";
    private static final String DEFAULT_PORT = "3000";
    private static final String DEFAULT_USER = "admin";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/" + PASSWORD_SECRET_NAME;

    public GrafanaService(String stackName, ServiceConfig config) {
        super(stackName, config);
    }

    @Override
    protected void doPreStartUpConfiguration() {
        setEnvironmentVariableIfAbsent(PASSWORD_FILE_KEY, DEFAULT_PASSWORD_FILE);
        // Before running the container, set the following grafana configuration
        // parameters
        // Set default username
        setEnvironmentVariableIfAbsent("GF_SECURITY_ADMIN_USER", DEFAULT_USER);
        // Set default password
        setEnvironmentVariableIfAbsent("GF_SECURITY_ADMIN_PASSWORD__FILE", getEnvironmentVariable(PASSWORD_FILE_KEY));
        // Configure the container's url
        setEnvironmentVariableIfAbsent("GF_SERVER_ROOT_URL", "%(protocol)s://%(domain)s:%(http_port)s/analytics/");
        setEnvironmentVariableIfAbsent("GF_SERVER_SERVE_FROM_SUB_PATH", "false");
        // Enable public access without credentials
        setEnvironmentVariableIfAbsent("GF_AUTH_ANONYMOUS_ENABLED", "true");
        // Allow users to embed the dashboard and panel outside of Grafana
        setEnvironmentVariableIfAbsent("GF_SECURITY_ALLOW_EMBEDDING", "true");

        // Write and expose the config within the stack for other agents to employ
        // Set up an endpoint config for Grafana with a default username of admin
        GrafanaEndpointConfig endpointConfig = new GrafanaEndpointConfig(
                EndpointNames.GRAFANA, getHostName(), DEFAULT_PORT, DEFAULT_USER,
                getEnvironmentVariable(PASSWORD_FILE_KEY));
        addEndpointConfig(endpointConfig);
    }
}