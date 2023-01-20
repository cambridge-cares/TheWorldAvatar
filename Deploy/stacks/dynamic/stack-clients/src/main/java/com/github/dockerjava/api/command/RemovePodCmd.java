package com.github.dockerjava.api.command;

import com.github.dockerjava.api.exception.NotFoundException;

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;

/**
 * Remove a pod.
 */
public interface RemovePodCmd extends SyncDockerCmd<Void> {

    @CheckForNull
    String getPodId();

    RemovePodCmd withPodId(@Nonnull String podId);

    /**
     * @throws NotFoundException
     *                           No such pod
     */
    @Override
    Void exec() throws NotFoundException;

    interface Exec extends DockerCmdSyncExec<RemovePodCmd, Void> {
    }

}
