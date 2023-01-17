package com.cmclinnovations.stack.services;

import java.net.URI;
import java.util.Optional;

import com.cmclinnovations.stack.clients.docker.PodmanClient;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.model.Container;

public class PodmanService extends DockerService {

    public static final String TYPE = "podman";

    public PodmanService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    @Override
    public PodmanClient initClient(URI dockerUri) {
        return new PodmanClient(dockerUri);
    }

    @Override
    public PodmanClient getClient() {
        return (PodmanClient) super.getClient();
    }

    @Override
    protected void initialise(String stackName) {

        addStackSecrets();

        addStackConfigs();

        createNetwork(stackName);
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
