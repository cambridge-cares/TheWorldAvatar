package com.cmclinnovations.stack.clients.docker;

import java.net.URI;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.github.dockerjava.api.command.CreateSecretCmd;
import com.github.dockerjava.api.model.SecretSpec;
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
    public void addSecret(String secretName, String data) {
        SecretSpec secretSpec = new SecretSpec()
                .withName(StackClient.prependStackName(secretName))
                .withData(data)
                .withLabels(null);
        try (CreateSecretCmd createSecretCmd = getInternalClient().createSecretCmd(secretSpec)) {
            createSecretCmd.exec();
        }
    }
}
