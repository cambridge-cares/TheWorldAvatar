package com.github.dockerjava.api;

import com.github.dockerjava.api.command.ListPodsCmd;

public interface PodmanClient extends DockerClient {

    /**
     * Command to list all pods.
     *
     * @return command
     */
    ListPodsCmd listPodsCmd();

}
