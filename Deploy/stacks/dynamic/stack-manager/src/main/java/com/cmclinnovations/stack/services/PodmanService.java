package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.services.config.ServiceConfig;

public class PodmanService extends DockerService {

    public static final String TYPE = "podman";

    public PodmanService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    @Override
    protected void startDockerSwarm() {
    }

    @Override
    protected void addStackConfigs() {
    }

}
