package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public class BlazegraphService extends ContainerService {

    public static final String TYPE = "blazegraph";

    private static final String DEFAULT_USERNAME = "bg_user";
    private static final String DEFAULT_PORT = "8080";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/blazegraph_password";

    private final BlazegraphEndpointConfig endpointConfig;

    public BlazegraphService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        setEnvironmentVariableIfAbsent("BLAZEGRAPH_USER", DEFAULT_USERNAME);
        setEnvironmentVariableIfAbsent("BLAZEGRAPH_PASSWORD_FILE", DEFAULT_PASSWORD_FILE);

        endpointConfig = new BlazegraphEndpointConfig(
                EndpointNames.BLAZEGRAPH, getHostName(), DEFAULT_PORT,
                getEnvironmentVariable("BLAZEGRAPH_USER"), getEnvironmentVariable("BLAZEGRAPH_PASSWORD_FILE"));
    }

    @Override
    public void doPreStartUpConfiguration() {
        addOptionalSecret("blazegraph_password");
    }

    @Override
    public void doPostStartUpConfiguration() {
        writeEndpointConfig(endpointConfig);
    }
}
