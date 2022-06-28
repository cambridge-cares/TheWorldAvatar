package com.cmclinnovations.stack.clients.docker;

import com.cmclinnovations.stack.clients.core.AbstractEndpointConfig;

public abstract class ContainerClient extends BaseClient {

    private final DockerClient dockerClient;

    protected ContainerClient() {
        this.dockerClient = new DockerClient();
    }

    protected ContainerClient(DockerClient dockerClient) {
        this.dockerClient = dockerClient;
    }

    protected DockerClient getDockerClient() {
        return dockerClient;
    }

    @Override
    protected final <E extends AbstractEndpointConfig> void writeEndpointConfig(E endpointConfig) {
        writeEndpointConfig(endpointConfig, dockerClient);
    }

    @Override
    protected final <E extends AbstractEndpointConfig> E readEndpointConfig(String endpointName,
            Class<E> endpointConfigClass) {
        return readEndpointConfig(endpointName, endpointConfigClass, dockerClient);
    }

}
