package com.github.dockerjava.core;

import static com.google.common.base.Preconditions.checkNotNull;

import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.api.command.PodmanCmdExecFactory;
import com.github.dockerjava.core.exec.ListPodsCmdExec;

public abstract class AbstractPodmanCmdExecFactory implements PodmanCmdExecFactory, DockerClientConfigAware {

    private DockerClientConfig dockerClientConfig;

    protected Integer connectTimeout;
    protected Integer readTimeout;

    protected DockerClientConfig getDockerClientConfig() {
        checkNotNull(dockerClientConfig,
                "Factor not initialized, dockerClientConfig not set. You probably forgot to call init()!");
        return dockerClientConfig;
    }

    @Override
    public void init(DockerClientConfig dockerClientConfig) {
        checkNotNull(dockerClientConfig, "config was not specified");
        this.dockerClientConfig = dockerClientConfig;
    }

    /**
     * Configure connection timeout in milliseconds
     */
    public AbstractPodmanCmdExecFactory withConnectTimeout(Integer connectTimeout) {
        this.connectTimeout = connectTimeout;
        return this;
    }

    /**
     * Configure read timeout in milliseconds
     */
    public AbstractPodmanCmdExecFactory withReadTimeout(Integer readTimeout) {
        this.readTimeout = readTimeout;
        return this;
    }

    // pods
    @Override
    public ListPodsCmd.Exec createListPodsCmdExec() {
        return new ListPodsCmdExec(getBaseResource(), getDockerClientConfig());
    }

    protected abstract WebTarget getBaseResource();
}
