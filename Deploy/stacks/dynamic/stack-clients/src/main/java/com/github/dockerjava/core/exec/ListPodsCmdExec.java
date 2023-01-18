package com.github.dockerjava.core.exec;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.dockerjava.api.command.ListPodsCmd;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.MediaType;
import com.github.dockerjava.core.WebTarget;
import com.github.dockerjava.core.util.FiltersEncoder;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import io.kubernetes.client.openapi.models.V1Pod;

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

        try (InputStream inputStream = webTarget.request().accept(MediaType.APPLICATION_JSON).get();
                Reader reader = new InputStreamReader(inputStream)) {
            List<V1Pod> pods = gson.fromJson(reader, new TypeToken<List<V1Pod>>() {
            }.getType());

            LOGGER.trace("Response: {}", pods);

            return pods;
        } catch (IOException ex) {
            throw new RuntimeException(null, ex);
        }
    }

}
