package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public final class PostGISService extends ContainerService {

    public static final String TYPE = "postgres";

    private static final String DEFAULT_USERNAME = "postgres";
    private static final String DEFAULT_PORT = "5432";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/postgis_password";

    private final PostGISEndpointConfig endpointConfig;

    public PostGISService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        setEnvironmentVariableIfAbsent("POSTGRES_USER", DEFAULT_USERNAME);
        setEnvironmentVariableIfAbsent("POSTGRES_PASSWORD_FILE", DEFAULT_PASSWORD_FILE);
        endpointConfig = new PostGISEndpointConfig("postgis", getContainerName(), DEFAULT_PORT,
                getEnvironmentVariable("POSTGRES_USER"), getEnvironmentVariable("POSTGRES_PASSWORD_FILE"));
    }

    @Override
    public void doPostStartUpConfiguration() {
        writeEndpointConfig(endpointConfig);
    }

}
