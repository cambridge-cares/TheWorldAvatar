package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.grafana.GrafanaEndpointConfig;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public final class GrafanaService extends ContainerService {

    public static final String TYPE = "grafana";

    private static final String DEFAULT_USER = "admin";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/grafana_password";
    private static final String DEFAULT_PORT = "3000";

    private final GrafanaEndpointConfig endpointConfig;

    public GrafanaService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        setEnvironmentVariableIfAbsent("GF_SECURITY_ADMIN_USER", DEFAULT_USER);
        setEnvironmentVariableIfAbsent("GF_SECURITY_ADMIN_PASSWORD__FILE", DEFAULT_PASSWORD_FILE);

        endpointConfig = new GrafanaEndpointConfig(EndpointNames.GRAFANA, getHostName(), DEFAULT_PORT,
                getEnvironmentVariable("GF_SECURITY_ADMIN_USER"),
                getEnvironmentVariable("GF_SECURITY_ADMIN_PASSWORD__FILE"));
    }

    @Override
    public void doPostStartUpConfiguration() {
        writeEndpointConfig(endpointConfig);
    }

}
