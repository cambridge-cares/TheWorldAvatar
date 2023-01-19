package com.github.dockerjava.core;

import java.io.IOException;

import com.cmclinnovations.swagger.podman.JSON;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.dockerjava.transport.DockerHttpClient;

public final class DefaultPodmanCmdExecFactory extends AbstractPodmanCmdExecFactory {

    private static final JSON swaggerObjectMapper = new JSON();

    private final DefaultDockerCmdExecFactory dockerCmdExecFactory;

    public static JSON getSwaggerObjectMapper() {
        return swaggerObjectMapper;
    }

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
    public void close() throws IOException {
        dockerCmdExecFactory.close();
    }

}
