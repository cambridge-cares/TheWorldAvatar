package com.github.dockerjava.core.exec;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.swagger.podman.model.ListPodsReport;
import com.fasterxml.jackson.core.type.TypeReference;
import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.MediaType;
import com.github.dockerjava.core.WebTarget;
import com.github.dockerjava.core.util.FiltersEncoder;

public class ListPodsCmdExec extends AbstrSyncDockerCmdExec<ListPodsCmd, List<ListPodsReport>> implements
        ListPodsCmd.Exec {

    private static final Logger LOGGER = LoggerFactory.getLogger(ListPodsCmdExec.class);

    public ListPodsCmdExec(WebTarget baseResource, DockerClientConfig dockerClientConfig) {
        super(baseResource, dockerClientConfig);
    }

    @Override
    protected List<ListPodsReport> execute(ListPodsCmd command) {
        WebTarget webTarget = getBaseResource().path("pods", "json");

        Map<String, List<String>> filters = command.getFilters();
        if (filters != null && !filters.isEmpty()) {
            webTarget = webTarget.queryParam("filters", FiltersEncoder.jsonEncode(filters));
        }

        LOGGER.trace("GET: {}", webTarget);

        List<ListPodsReport> pods = webTarget.request().accept(MediaType.APPLICATION_JSON)
                .get(new TypeReference<List<ListPodsReport>>() {
                });

        LOGGER.trace("Response: {}", pods);

        return pods;
    }

}
