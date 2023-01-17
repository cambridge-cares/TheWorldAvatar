package com.github.dockerjava.api.command;

import java.util.List;
import java.util.Map;

import javax.annotation.CheckForNull;

import io.kubernetes.client.openapi.models.V1Pod;

public interface ListPodsCmd extends SyncDockerCmd<List<V1Pod>> {

    @CheckForNull
    Map<String, List<String>> getFilters();

    /**
     * @param names - Show only pods with the given names
     */
    ListPodsCmd withNameFilter(List<String> names);

    interface Exec extends DockerCmdSyncExec<ListPodsCmd, List<V1Pod>> {
    }
}
