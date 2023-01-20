package com.github.dockerjava.core;

import static com.google.common.base.Preconditions.checkNotNull;

import java.io.IOException;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.DockerClientDelegate;
import com.github.dockerjava.api.PodmanClient;
import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.api.command.PodmanCmdExecFactory;
import com.github.dockerjava.api.command.RemovePodCmd;
import com.github.dockerjava.core.command.ListPodsCmdImpl;
import com.github.dockerjava.core.command.RemovePodCmdImpl;
import com.github.dockerjava.transport.DockerHttpClient;

public class PodmanClientImpl extends DockerClientDelegate implements PodmanClient {

    private final DockerClient baseClient;

    private final DockerClientConfig dockerClientConfig;

    PodmanCmdExecFactory podmanCmdExecFactory;

    PodmanClientImpl(DockerClient baseClient, DockerClientConfig dockerClientConfig) {
        checkNotNull(dockerClientConfig, "config was not specified");
        this.dockerClientConfig = dockerClientConfig;
        checkNotNull(baseClient, "baseClient was not specified");
        this.baseClient = baseClient;
    }

    public static PodmanClient getInstance(DockerClient baseClient, DockerClientConfig dockerClientConfig,
            DockerHttpClient dockerHttpClient) {
        return new PodmanClientImpl(baseClient, dockerClientConfig)
                .withHttpClient(dockerHttpClient);
    }

    PodmanClientImpl withHttpClient(DockerHttpClient httpClient) {
        return withPodmanCmdExecFactory(
                new DefaultPodmanCmdExecFactory(httpClient, dockerClientConfig.getObjectMapper()));
    }

    /**
     * @deprecated use {@link #getInstance(DockerClientConfig, DockerHttpClient)}
     */
    @Deprecated
    public PodmanClientImpl withPodmanCmdExecFactory(PodmanCmdExecFactory podmanCmdExecFactory) {
        checkNotNull(podmanCmdExecFactory, "dockerCmdExecFactory was not specified");
        this.podmanCmdExecFactory = podmanCmdExecFactory;
        if (podmanCmdExecFactory instanceof DockerClientConfigAware) {
            ((DockerClientConfigAware) podmanCmdExecFactory).init(dockerClientConfig);
        }
        return this;
    }

    @Deprecated
    private PodmanCmdExecFactory getPodmanCmdExecFactory() {
        checkNotNull(podmanCmdExecFactory, "dockerCmdExecFactory was not specified");
        return podmanCmdExecFactory;
    }

    @Override
    protected DockerClient getDockerClient() {
        return baseClient;
    }

    @Override
    public ListPodsCmd listPodsCmd() {
        return new ListPodsCmdImpl(getPodmanCmdExecFactory().createListPodsCmdExec());
    }

    @Override
    public RemovePodCmd removePodCmd(String podId) {
        return new RemovePodCmdImpl(getPodmanCmdExecFactory().createRemovePodCmdExec(), podId);
    }

    @Override
    public void close() throws IOException {
        getPodmanCmdExecFactory().close();
    }

}
