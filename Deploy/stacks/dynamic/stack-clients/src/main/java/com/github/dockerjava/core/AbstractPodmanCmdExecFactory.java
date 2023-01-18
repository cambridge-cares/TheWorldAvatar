package com.github.dockerjava.core;

import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.api.command.PodmanCmdExecFactory;
import com.github.dockerjava.core.exec.ListPodsCmdExec;

public abstract class AbstractPodmanCmdExecFactory implements PodmanCmdExecFactory, DockerClientConfigAware {


    protected abstract DockerClientConfig getDockerClientConfig();

    protected abstract WebTarget getBaseResource();

    // pods
    @Override
    public ListPodsCmd.Exec createListPodsCmdExec() {
        return new ListPodsCmdExec(getBaseResource(), getDockerClientConfig());
    }
}
