package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public class BlazegraphService extends ContainerService {

    public static final String TYPE = "blazegraph";

    private static final String DEFAULT_USERNAME = "bg_user";
    private static final String DEFAULT_PORT = "8080";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/blazegraph_password";

    public BlazegraphService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    @Override
    public void doPreStartUpConfiguration() {
        if (addOptionalSecret("blazegraph_password")) {
            setEnvironmentVariableIfAbsent("BLAZEGRAPH_USER", DEFAULT_USERNAME);
            setEnvironmentVariableIfAbsent("BLAZEGRAPH_PASSWORD_FILE", DEFAULT_PASSWORD_FILE);
        }
    }

    @Override
    public void doPostStartUpConfiguration() {
        BlazegraphEndpointConfig endpointConfig = new BlazegraphEndpointConfig(
                EndpointNames.BLAZEGRAPH, getHostName(), DEFAULT_PORT,
                getEnvironmentVariable("BLAZEGRAPH_USER"), getEnvironmentVariable("BLAZEGRAPH_PASSWORD_FILE"));
        writeEndpointConfig(endpointConfig);
    }
}
