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

    @Override
    public void init(DockerClientConfig dockerClientConfig) {
        dockerCmdExecFactory.init(dockerClientConfig);
    }

    @Override
    protected DockerClientConfig getDockerClientConfig() {
        return dockerCmdExecFactory.getDockerClientConfig();
    }

    public DockerHttpClient getDockerHttpClient() {
        return dockerCmdExecFactory.getDockerHttpClient();
    }

    @Override
    protected WebTarget getBaseResource() {
        return dockerCmdExecFactory.getBaseResource();
    }

    @Override
    protected WebTarget getPodmanBaseResource() {
        return getBaseResource().path("v4.0.0", "libpod");
    }

    @Override
    public void close() throws IOException {
        dockerCmdExecFactory.close();
    }

}
