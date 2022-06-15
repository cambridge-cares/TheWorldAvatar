package com.cmclinnovations.services;

import com.cmclinnovations.services.config.ServiceConfig;

public final class PostGISService extends ContainerService {

    public static final String TYPE = "postgres";

    public PostGISService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        setEnvironmentVariable("POSTGRES_USER", getUsername());
        setEnvironmentVariable("POSTGRES_PASSWORD_FILE", getPasswordFile());
    }

}
