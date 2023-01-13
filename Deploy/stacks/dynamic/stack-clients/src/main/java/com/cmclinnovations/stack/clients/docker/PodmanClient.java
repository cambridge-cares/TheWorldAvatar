package com.cmclinnovations.stack.clients.docker;

import java.net.URI;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.github.dockerjava.api.command.CreateSecretCmd;
import com.github.dockerjava.api.model.SecretSpec;

public class PodmanClient extends DockerClient {

    protected PodmanClient() {
    }

    public PodmanClient(URI dockerUri) {
        super(dockerUri);
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
