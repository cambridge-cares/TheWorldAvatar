package com.github.dockerjava.core.command;

import com.github.dockerjava.api.command.RemovePodCmd;
import com.github.dockerjava.api.exception.NotFoundException;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Remove a pod.
 */
public class RemovePodCmdImpl extends AbstrDockerCmd<RemovePodCmd, Void> implements RemovePodCmd {

    private String podId;

    public RemovePodCmdImpl(Exec exec, String podId) {
        super(exec);
        withPodId(podId);
    }

    @Override
    public String getPodId() {
        return podId;
    }

    @Override
    public RemovePodCmd withPodId(String podId) {
        checkNotNull(podId, "podId was not specified");
        this.podId = podId;
        return this;
    }

    /**
     * @throws NotFoundException
     *                           No such pod
     */
    @Override
    public Void exec() throws NotFoundException {
        return super.exec();
    }
}