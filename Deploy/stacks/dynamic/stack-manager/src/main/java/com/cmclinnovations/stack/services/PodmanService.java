package com.cmclinnovations.stack.services;

import java.util.Optional;

import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.model.Container;

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

    @Override
    protected void removeSwarmService(ContainerService service) {
    }

    @Override
    protected Optional<Container> startSwarmService(ContainerService service) {
        return Optional.empty();
    }

}
