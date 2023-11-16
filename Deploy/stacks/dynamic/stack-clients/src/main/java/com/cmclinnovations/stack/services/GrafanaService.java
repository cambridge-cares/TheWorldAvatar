package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.grafana.GrafanaEndpointConfig;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public class GrafanaService extends ContainerService {
    private final GrafanaEndpointConfig ENDPOINT_CONFIG;
    public static final String TYPE = "grafana";
    private static final String PASSWORD_FILE_KEY = "GRAFANA_PASSWORD_FILE";
    private static final String PASSWORD_SECRET_NAME = "grafana_password";
    private static final String DEFAULT_PORT = "3000";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/" + PASSWORD_SECRET_NAME;
    private static final String GRAFANA_USER_CONFIG = "GF_SECURITY_ADMIN_USER";
    private static final String GRAFANA_PASSWORD_CONFIG = "GF_SECURITY_ADMIN_PASSWORD__FILE";

    public GrafanaService(String stackName, ServiceConfig config) {
        super(stackName, config);
        // Set up an endpoint config for Grafana with a default username of admin
        setEnvironmentVariableIfAbsent(PASSWORD_FILE_KEY, DEFAULT_PASSWORD_FILE);
        this.ENDPOINT_CONFIG = new GrafanaEndpointConfig(
                TYPE, getHostName(), DEFAULT_PORT, getEnvironmentVariable(PASSWORD_FILE_KEY));
    }

    @Override
    protected void doPreStartUpConfigurationImpl() {
        // Before running the container, set the following grafana configuration parameters
        setEnvironmentVariable(GRAFANA_USER_CONFIG, this.ENDPOINT_CONFIG.getUsername()); // Set default username
        setEnvironmentVariable(GRAFANA_PASSWORD_CONFIG, DEFAULT_PASSWORD_FILE); // Set default password
        // Configure the container's url
        setEnvironmentVariable("GF_SERVER_ROOT_URL", "%(protocol)s://%(domain)s:%(http_port)s/analytics/");
        setEnvironmentVariable("GF_SERVER_SERVE_FROM_SUB_PATH", "false");
        // Enable public access without credentials
        setEnvironmentVariable("GF_AUTH_ANONYMOUS_ENABLED", "true");
        // Allow users to embed the dashboard and panel outside of Grafana
        setEnvironmentVariable("GF_SECURITY_ALLOW_EMBEDDING", "true");
    }

    @Override
    public void doPostStartUpConfiguration() {
        // After the container is running, write and expose the config within the stack for other agents to employ
        writeEndpointConfig(this.ENDPOINT_CONFIG);
    }
}