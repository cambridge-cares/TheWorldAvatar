package com.cmclinnovations.stack.clients.docker;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.Nonnull;

import com.cmclinnovations.stack.clients.core.EndpointConfig;

abstract class BaseClient {

    protected static final Map<String, EndpointConfig> endpointsConfigs = new HashMap<>();

    public static final <O extends EndpointConfig> O readEndpointConfig(String otherContainerName,
            Class<O> otherEndpointConfigClass) {
        return otherEndpointConfigClass.cast(
                endpointsConfigs.computeIfAbsent(otherContainerName,
                        dummy -> DockerConfigHandler.readEndpointConfig(otherContainerName, otherEndpointConfigClass)));
    }

    public static final <F extends @Nonnull EndpointConfig> void writeEndpointConfig(F endpointConfig) {
        endpointsConfigs.remove(endpointConfig.getName());
        DockerConfigHandler.writeEndpointConfig(endpointConfig);
    }
}
