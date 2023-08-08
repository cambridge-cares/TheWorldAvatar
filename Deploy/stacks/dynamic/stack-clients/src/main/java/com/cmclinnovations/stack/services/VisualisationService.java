package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.services.config.ServiceConfig;

public class VisualisationService extends ContainerService {

    public static final String TYPE = "visualisation";

    public VisualisationService(String stackName, ServiceConfig config) {
        super(stackName, config);
    }

    @Override
    protected void doPreStartUpConfigurationImpl() {
        ensureOptionalSecret("mapbox_username");
        ensureOptionalSecret("mapbox_api_key");
    }
}
