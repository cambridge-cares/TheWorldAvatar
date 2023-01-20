package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.services.config.ServiceConfig;

public class FeatureInfoAgentService extends ContainerService {

    public static final String TYPE = "feature-info-agent";

    public FeatureInfoAgentService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

}
