package com.cmclinnovations.stack.clients.docker;

import java.net.URI;
import java.util.Map;

import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.PodmanClientImpl;
import com.github.dockerjava.transport.DockerHttpClient;

public class PodmanClient extends DockerClient {

    protected PodmanClient() {
    }

    public PodmanClient(URI dockerUri) {
        super(dockerUri);
    }

    @Override
    public com.github.dockerjava.api.PodmanClient buildInternalClient(DockerClientConfig dockerConfig,
            DockerHttpClient httpClient) {
        return PodmanClientImpl.getInstance(super.buildInternalClient(dockerConfig, httpClient), dockerConfig,
                httpClient);
    }

    @Override
    public com.github.dockerjava.api.PodmanClient getInternalClient() {
        return (com.github.dockerjava.api.PodmanClient) super.getInternalClient();
    }

    @Override
    protected Map<String, String> getSecretLabels() {
        // Podman does not support labels for secrets
        return null;
    }

}
