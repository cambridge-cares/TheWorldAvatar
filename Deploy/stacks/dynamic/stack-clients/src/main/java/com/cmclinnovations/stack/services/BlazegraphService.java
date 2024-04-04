package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public class BlazegraphService extends ContainerService {

    public static final String TYPE = "blazegraph";

    private static final String BLAZEGRAPH_USER_KEY = "BLAZEGRAPH_USER";
    private static final String BLAZEGRAPH_PASSWORD_FILE_KEY = "BLAZEGRAPH_PASSWORD_FILE";

    private static final String DEFAULT_USERNAME = "bg_user";
    private static final String DEFAULT_PORT = "8080";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/blazegraph_password";

    public BlazegraphService(String stackName, ServiceConfig config) {
        super(stackName, config);
    }

    @Override
    protected void doPreStartUpConfiguration() {

        if (ensureOptionalSecret("blazegraph_password")) {
            setEnvironmentVariableIfAbsent(BLAZEGRAPH_USER_KEY, DEFAULT_USERNAME);
            setEnvironmentVariableIfAbsent(BLAZEGRAPH_PASSWORD_FILE_KEY, DEFAULT_PASSWORD_FILE);
        } else {
            removeEnvironmentVariable(BLAZEGRAPH_USER_KEY);
            removeEnvironmentVariable(BLAZEGRAPH_PASSWORD_FILE_KEY);
        }

        BlazegraphEndpointConfig endpointConfig = new BlazegraphEndpointConfig(
                EndpointNames.BLAZEGRAPH, getHostName(), DEFAULT_PORT,
                getEnvironmentVariable(BLAZEGRAPH_USER_KEY), getEnvironmentVariable(BLAZEGRAPH_PASSWORD_FILE_KEY));

        addEndpointConfig(endpointConfig);

    }
}
