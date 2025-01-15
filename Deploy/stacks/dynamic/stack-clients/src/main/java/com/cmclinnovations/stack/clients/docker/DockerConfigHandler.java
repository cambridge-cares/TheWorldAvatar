package com.cmclinnovations.stack.clients.docker;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.annotation.Nonnull;

import com.cmclinnovations.stack.clients.core.EndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class DockerConfigHandler {
    private static final ObjectMapper objectMapper = JsonHelper.getMapper();
    private static final Path configsDir;

    private DockerConfigHandler() {
    }

    static {
        if (StackClient.isInTest()) {
            try {
                configsDir = Files.createTempDirectory("configs");
            } catch (IOException ex) {
                throw new RuntimeException("Failed to create directories for test Docker/Podman config files.", ex);
            }
        } else {
            configsDir = Path.of("/");
        }
    }

    public static final <E extends @Nonnull EndpointConfig> void writeEndpointConfig(E endpointConfig) {
        String endpointName = endpointConfig.getName();
        Path configFilePath = configsDir.resolve(endpointName);

        // Ensure this container can access the endpoint file.
        try {
            objectMapper.writeValue(configFilePath.toFile(), endpointConfig);
        } catch (IOException ex) {
            throw new RuntimeException("Failed to write to Docker config file with name '" + endpointName + "'.",
                    ex);
        }

        if (!StackClient.isInTest()) {
            DockerClient dockerClient = DockerClient.getInstance();
            if (!dockerClient.configExists(endpointName)) {
                try {
                    dockerClient.addConfig(endpointName, objectMapper.writeValueAsBytes(endpointConfig));
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
    }

    public static final <E extends EndpointConfig> E readEndpointConfig(String endpointName,
            Class<E> endpointConfigClass) {
        Path configFilePath = configsDir.resolve(endpointName);
        if (Files.exists(configFilePath)) {
            try {
                return objectMapper.readValue(configFilePath.toFile(), endpointConfigClass);
            } catch (IOException ex) {
                throw new RuntimeException("Failed to read Docker config file with name '" + endpointName + "'.", ex);
            }
        }
        throw new RuntimeException("No Docker config file with name '" + endpointName + "' exists.");
    }

}
