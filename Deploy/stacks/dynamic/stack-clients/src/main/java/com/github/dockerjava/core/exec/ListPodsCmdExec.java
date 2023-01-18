package com.github.dockerjava.core.exec;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.swagger.podman.model.ListPodsReport;
import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.InvocationBuilder;
import com.github.dockerjava.core.MediaType;
import com.github.dockerjava.core.WebTarget;
import com.github.dockerjava.core.util.FiltersEncoder;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;


public class ListPodsCmdExec extends AbstrSyncDockerCmdExec<ListPodsCmd, List<ListPodsReport>> implements
        ListPodsCmd.Exec {

    private Gson gson = new Gson();

    private static final Logger LOGGER = LoggerFactory.getLogger(ListPodsCmdExec.class);

    public ListPodsCmdExec(WebTarget baseResource, DockerClientConfig dockerClientConfig) {
        super(baseResource, dockerClientConfig);
    }

    @Override
    protected List<ListPodsReport> execute(ListPodsCmd command) {
        WebTarget webTarget = getBaseResource().path("/v4.0.0/libpod/pods/json");

        if (command.getFilters() != null && !command.getFilters().isEmpty()) {
            webTarget = webTarget
                    .queryParam("filters", FiltersEncoder.jsonEncode(command.getFilters()));
        }

        LOGGER.trace("GET: {}", webTarget);

        InvocationBuilder invocationBuilder = webTarget.request().accept(MediaType.APPLICATION_JSON);
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(invocationBuilder.get()))) {
            List<ListPodsReport> pods = gson.fromJson(reader, new TypeToken<List<ListPodsReport>>() {
            }.getType());

            LOGGER.trace("Response: {}", pods);

            return pods;
        } catch (IOException ex) {
            throw new RuntimeException(null, ex);
        }
    }

}
