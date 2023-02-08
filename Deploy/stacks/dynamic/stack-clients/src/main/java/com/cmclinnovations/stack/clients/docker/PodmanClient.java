package com.cmclinnovations.stack.clients.docker;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.swagger.podman.ApiClient;
import com.cmclinnovations.swagger.podman.ApiException;
import com.cmclinnovations.swagger.podman.api.SecretsApi;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.dockerjava.api.model.Config;
import com.github.dockerjava.jaxrs.ApiClientExtension;

public class PodmanClient extends DockerClient {

    private final ApiClient apiClient = new ApiClientExtension(URI.create("unix:///var/run/docker.sock"))
            .setBasePath("unix://localhost:2375/v4.0.0/");

    protected PodmanClient() {
    }

    public PodmanClient(URI dockerUri) {
        super(dockerUri);
    }

    public ApiClient getPodmanClient() {
        return apiClient;
    }

    @Override
    protected Map<String, String> getSecretLabels() {
        // Podman does not support labels for secrets
        return null;
    }

    @Override
    public void addSecret(String secretName, String data) {
        String fullSecretName = StackClient.prependStackName(secretName);
        SecretsApi secretsApi = new SecretsApi(getPodmanClient());
        try {
            secretsApi.secretCreateLibpod(fullSecretName, null, null, null, data);
        } catch (ApiException ex) {
            throw new RuntimeException("Failed to create Config/Secret '" + secretName + "'.", ex);
        }
    }

    @Override
    public void addConfig(String configName, String data) {
        addSecret(configName, data);
    }

    @Override
    public void addConfig(String configName, byte[] data) {
        addSecret(configName, new String(data));
    }

    @Override
    public boolean configExists(String configName) {
        return secretExists(configName);
    }

    @Override
    public List<Config> getConfigs() {
        ObjectMapper mapper = new ObjectMapper();
        return getSecrets().stream().map(secret -> {
            try {
                return mapper.readValue("{\"ID\":\"" + secret.getId() + "\", \"Spec\":{\"Name\":\""
                        + secret.getSpec().getName() + "\" }}", Config.class);
            } catch (JsonProcessingException e) {
                throw new RuntimeException("Failed to convert Secret to Config.");
            }
        }).collect(Collectors.toList());
    }

}
