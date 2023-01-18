package com.github.dockerjava.core.exec;

import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.MediaType;
import com.github.dockerjava.core.WebTarget;
import com.github.dockerjava.core.util.FiltersEncoder;
import com.google.gson.Gson;

import io.kubernetes.client.openapi.models.V1Pod;
import jakarta.json.JsonObject;

public class ListPodsCmdExec extends AbstrSyncDockerCmdExec<ListPodsCmd, List<V1Pod>> implements
        ListPodsCmd.Exec {

    private Gson gson = new Gson();

    private static final Logger LOGGER = LoggerFactory.getLogger(ListPodsCmdExec.class);

    public ListPodsCmdExec(WebTarget baseResource, DockerClientConfig dockerClientConfig) {
        super(baseResource, dockerClientConfig);
    }

    @Override
    protected List<V1Pod> execute(ListPodsCmd command) {
        WebTarget webTarget = getBaseResource().path("/libpod/pods/json");

        if (command.getFilters() != null && !command.getFilters().isEmpty()) {
            webTarget = webTarget
                    .queryParam("filters", FiltersEncoder.jsonEncode(command.getFilters()));
        }

        LOGGER.trace("GET: {}", webTarget);

        List<JsonObject> podsJson = webTarget.request().accept(MediaType.APPLICATION_JSON)
                .get(new TypeReference<List<JsonObject>>() {
                });

        List<V1Pod> pods = podsJson.stream()
                .map(object -> gson.fromJson(object.toString(), V1Pod.class))
                .collect(Collectors.toList());

        LOGGER.trace("Response: {}", pods);

        return pods;
    }

}
