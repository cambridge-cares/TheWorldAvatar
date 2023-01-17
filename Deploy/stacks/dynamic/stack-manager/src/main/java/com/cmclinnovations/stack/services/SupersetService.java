package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.services.config.ServiceConfig;

public class SupersetService extends ContainerService {

    public static final String TYPE = "superset";

    public SupersetService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

}
