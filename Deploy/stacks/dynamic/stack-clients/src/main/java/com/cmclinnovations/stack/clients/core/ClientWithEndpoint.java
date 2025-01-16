package com.cmclinnovations.stack.clients.core;

import javax.annotation.Nonnull;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.docker.DockerConfigHandler;

public abstract class ClientWithEndpoint<E extends EndpointConfig> extends ContainerClient {

    private final String containerName;
    private final Class<E> endpointConfigClass;

    protected ClientWithEndpoint(String containerName, Class<@Nonnull E> endpointConfigClass) {
        this.containerName = containerName;
        this.endpointConfigClass = endpointConfigClass;
    }

    public final String getContainerName() {
        return containerName;
    }

    public final E readEndpointConfig() {
        return endpointConfigClass.cast(
                endpointsConfigs.computeIfAbsent(containerName,
                        dummy -> DockerConfigHandler.readEndpointConfig(containerName, endpointConfigClass)));
    }
}
