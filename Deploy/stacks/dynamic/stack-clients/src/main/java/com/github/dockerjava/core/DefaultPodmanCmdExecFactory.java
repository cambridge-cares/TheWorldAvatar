package com.github.dockerjava.core;

import java.io.IOException;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.dockerjava.transport.DockerHttpClient;

public final class DefaultPodmanCmdExecFactory extends AbstractPodmanCmdExecFactory {

    private final DefaultDockerCmdExecFactory dockerCmdExecFactory;

    public DefaultPodmanCmdExecFactory(
            DockerHttpClient dockerHttpClient,
            ObjectMapper objectMapper) {
        this.dockerCmdExecFactory = new DefaultDockerCmdExecFactory(dockerHttpClient, objectMapper);
    }

    public DockerHttpClient getDockerHttpClient() {
        return dockerCmdExecFactory.getDockerHttpClient();
    }

    @Override
    protected WebTarget getBaseResource() {
        return dockerCmdExecFactory.getBaseResource();
    }

    @Override
    public void close() throws IOException {
        dockerCmdExecFactory.close();
    }

}
