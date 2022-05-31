package com.cmclinnovations.services;

import com.cmclinnovations.services.config.ServiceConfig;

public class PostGISService extends ContainerService {

    public static final String TYPE = "postgres";

    public PostGISService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        setEnvironmentVariable("POSTGRES_USER", getUsername());
        setEnvironmentVariable("POSTGRES_PASSWORD", getPassword());
    }

}
