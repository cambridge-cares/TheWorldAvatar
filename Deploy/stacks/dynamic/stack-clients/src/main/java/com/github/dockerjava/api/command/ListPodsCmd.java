package com.github.dockerjava.api.command;

import java.util.List;
import java.util.Map;

import javax.annotation.CheckForNull;

import com.cmclinnovations.swagger.podman.model.ListPodsReport;

public interface ListPodsCmd extends SyncDockerCmd<List<ListPodsReport>> {

    @CheckForNull
    Map<String, List<String>> getFilters();

    /**
     * @param names - Show only pods with the given names
     */
    ListPodsCmd withNameFilter(List<String> names);

    interface Exec extends DockerCmdSyncExec<ListPodsCmd, List<ListPodsReport>> {
    }
}
