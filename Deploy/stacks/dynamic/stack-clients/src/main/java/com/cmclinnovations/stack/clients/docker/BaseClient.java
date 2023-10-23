package com.cmclinnovations.stack.clients.docker;

import java.io.File;
import java.io.IOException;

import com.cmclinnovations.stack.clients.core.EndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

abstract class BaseClient {

    private final ObjectMapper objectMapper = new ObjectMapper();

    protected abstract <E extends EndpointConfig> void writeEndpointConfig(E endpointConfig);

    protected <E extends EndpointConfig> void writeEndpointConfig(E endpointConfig,
            DockerClient injectedDockerClient) {
        String endpointName = endpointConfig.getName();
        if (!StackClient.isInStack()) {
            // Hack for when container is not in the swarm
            try {
                objectMapper.writeValue(new File("/" + endpointName), endpointConfig);
            } catch (IOException ex) {
                throw new RuntimeException("Failed to write to Docker config file with name '" + endpointName + "'.",
                        ex);
            }
        }
        if (!injectedDockerClient.configExists(endpointName)) {
            try {
                injectedDockerClient.addConfig(endpointName,
                        objectMapper.writeValueAsBytes(endpointConfig));
            } catch (JsonProcessingException ex) {
                throw new RuntimeException("Failed to add Docker config file with name '" + endpointName + "'.",
                        ex);
            }
        } else {
            /**
             * TODO Decide what to do if the config already exists.
             * See https://docs.docker.com/engine/swarm/secrets/#example-rotate-a-secret
             * for details on how to update a secret/config that is in use.
             */
        }
    }

    protected abstract <E extends EndpointConfig> E readEndpointConfig(String endpointName,
            Class<E> endpointConfigClass);

    protected final <E extends EndpointConfig> E readEndpointConfig(String endpointName,
            Class<E> endpointConfigClass,
            DockerClient injectedDockerClient) {
        if (injectedDockerClient.configExists(endpointName)) {
            try {
                return objectMapper.readValue(new File("/" + endpointName), endpointConfigClass);
            } catch (IOException ex) {
                throw new RuntimeException("Failed to read Docker config file with name '" + endpointName + "'.", ex);
            }
        }
        throw new RuntimeException("No Docker config file with name '" + endpointName + "' exists.");
    }
}
