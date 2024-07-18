package com.cmclinnovations.stack.clients.core;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.Nonnull;

import com.cmclinnovations.stack.clients.docker.ContainerClient;

public abstract class ClientWithEndpoint<E extends EndpointConfig> extends ContainerClient {

    private static final Map<String, EndpointConfig> endpointsConfigs = new HashMap<>();

    private final String containerName;
    private final Class<E> endpointClass;

    protected ClientWithEndpoint(String containerName, Class<@Nonnull E> endpointConfigClass) {
        this.containerName = containerName;
        this.endpointClass = endpointConfigClass;
    }

    public final String getContainerName() {
        return containerName;
    }

    public final E getEndpointConfig() {
        return endpointClass.cast(
                endpointsConfigs.computeIfAbsent(containerName,
                        dummy -> readEndpointConfig(containerName, endpointClass)));
    }

    @Override
    public <F extends @Nonnull EndpointConfig> void writeEndpointConfig(F endpointConfig) {
        endpointsConfigs.remove(endpointConfig.getName());
        super.writeEndpointConfig(endpointConfig);
    }

}
