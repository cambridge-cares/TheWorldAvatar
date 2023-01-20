package com.github.dockerjava.core;

import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.api.command.PodmanCmdExecFactory;
import com.github.dockerjava.api.command.RemovePodCmd;
import com.github.dockerjava.core.exec.ListPodsCmdExec;
import com.github.dockerjava.core.exec.RemovePodCmdExec;

public abstract class AbstractPodmanCmdExecFactory implements PodmanCmdExecFactory, DockerClientConfigAware {

    protected abstract DockerClientConfig getDockerClientConfig();

    protected abstract WebTarget getBaseResource();

    protected abstract WebTarget getPodmanBaseResource();

    // pods
    @Override
    public ListPodsCmd.Exec createListPodsCmdExec() {
        return new ListPodsCmdExec(getPodmanBaseResource(), getDockerClientConfig());
    }

    @Override
    public RemovePodCmd.Exec createRemovePodCmdExec() {
        return new RemovePodCmdExec(getPodmanBaseResource(), getDockerClientConfig());
    }
}
