package com.github.dockerjava.core.exec;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.swagger.podman.model.ListPodsReport;
import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.core.DefaultPodmanCmdExecFactory;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.InvocationBuilder;
import com.github.dockerjava.core.MediaType;
import com.github.dockerjava.core.WebTarget;
import com.github.dockerjava.core.util.FiltersEncoder;
import com.google.gson.reflect.TypeToken;

public class ListPodsCmdExec extends AbstrSyncDockerCmdExec<ListPodsCmd, List<ListPodsReport>> implements
        ListPodsCmd.Exec {

    private static final Logger LOGGER = LoggerFactory.getLogger(ListPodsCmdExec.class);

    public ListPodsCmdExec(WebTarget baseResource, DockerClientConfig dockerClientConfig) {
        super(baseResource, dockerClientConfig);
    }

    @Override
    protected List<ListPodsReport> execute(ListPodsCmd command) {
        WebTarget webTarget = getBaseResource().path("/v4.0.0/libpod/pods/json");

        Map<String, List<String>> filters = command.getFilters();
        if (filters != null && !filters.isEmpty()) {
            webTarget = webTarget.queryParam("filters", FiltersEncoder.jsonEncode(filters));
        }

        LOGGER.trace("GET: {}", webTarget);

        InvocationBuilder invocationBuilder = webTarget.request().accept(MediaType.APPLICATION_JSON);
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(invocationBuilder.get()))) {
            List<ListPodsReport> pods = DefaultPodmanCmdExecFactory.getSwaggerObjectMapper()
                    .getGson().fromJson(reader, new TypeToken<List<ListPodsReport>>() {
                    }.getType());

            LOGGER.trace("Response: {}", pods);

            return pods;
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read Podman API method response.", ex);
        }
    }

}
