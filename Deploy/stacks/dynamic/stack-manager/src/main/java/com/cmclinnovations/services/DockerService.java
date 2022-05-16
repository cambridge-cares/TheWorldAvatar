package com.cmclinnovations.services;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.CreateContainerCmd;
import com.github.dockerjava.api.command.ListContainersCmd;
import com.github.dockerjava.api.command.PullImageCmd;
import com.github.dockerjava.api.command.PullImageResultCallback;
import com.github.dockerjava.api.command.StartContainerCmd;
import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.Container;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DefaultDockerClientConfig.Builder;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.httpclient5.ApacheDockerHttpClient;
import com.github.dockerjava.transport.DockerHttpClient;

public class DockerService extends AbstractService {

    private final DockerClient dockerClient;

    public DockerService(ServiceConfig config) throws URISyntaxException {
        super(config);

        Builder dockerConfigBuilder = DefaultDockerClientConfig.createDefaultConfigBuilder();

        String host = config.getHost();
        if (null != host && !"localhost".equals(host)) {
            int port = config.getPorts().get("api").getInternalPort();
            String path = config.getPath();
            URI hostURI = new URI("tcp", null, host, port, path, null, null);

            dockerConfigBuilder.withDockerHost(hostURI.toString());
        }

        DockerClientConfig dockerConfig = dockerConfigBuilder.build();

        DockerHttpClient httpClient = new ApacheDockerHttpClient.Builder()
                .dockerHost(dockerConfig.getDockerHost())
                .sslConfig(dockerConfig.getSSLConfig())
                .build();

        dockerClient = DockerClientBuilder.getInstance(dockerConfig).withDockerHttpClient(httpClient).build();
    }

    private Optional<Container> getContainer(ServiceConfig serviceConfig) {
        try (ListContainersCmd listContainersCmd = dockerClient.listContainersCmd()) {
            // Setting "showAll" to "true" ensures non-running containers are also returned
            return listContainersCmd.withNameFilter(List.of(serviceConfig.getContainerName())).withShowAll(true).exec()
                    .stream().findAny();
        }
    }

    public boolean isContainerUp(ServiceConfig serviceConfig) {
        try (ListContainersCmd listContainersCmd = dockerClient.listContainersCmd()) {
            // Don't need to filter for "running" state as this is the default setting
            return !listContainersCmd.withNameFilter(List.of(serviceConfig.getContainerName())).exec().isEmpty();
        }
    }

    public String getContainerID(ServiceConfig serviceConfig) {
        return getContainer(serviceConfig).map(Container::getId).orElseThrow();
    }

    public void startContainer(ServiceConfig serviceConfig) {
        startContainer(serviceConfig, Collections.emptyList());
    }

    public void startContainer(ServiceConfig serviceConfig, List<Bind> volumes) {
        startContainer(serviceConfig, volumes, getPortMappings(serviceConfig));
    }

    private static List<PortBinding> getPortMappings(ServiceConfig serviceConfig) {
        return serviceConfig.getPorts().entrySet().stream()
                .filter(portMapping -> null != portMapping.getValue().getExternalPort())
                .map(portMapping -> portMapping.getValue().getPortBinding())
                .collect(Collectors.toList());
    }

    public void startContainer(ServiceConfig serviceConfig, List<Bind> volumes, List<PortBinding> ports) {
        final String containerId;
        final String containerState;
        Optional<Container> container = getContainer(serviceConfig);
        if (container.isEmpty()) {
            // No container matching that config
            if (dockerClient.listImagesCmd().withImageNameFilter(serviceConfig.getImage()).exec().isEmpty()) {
                // No image with the requested image ID, so try to pull image
                try (PullImageCmd pullImageCmd = dockerClient.pullImageCmd(serviceConfig.getImage())) {
                    pullImageCmd
                            .exec(new PullImageResultCallback())
                            .awaitCompletion();
                } catch (InterruptedException e) {
                    throw new RuntimeException("Docker image pull command interupted", e);
                }
            }

            // Create container
            try (CreateContainerCmd createContainerCmd = dockerClient
                    .createContainerCmd(serviceConfig.getImage())) {
                HostConfig hostConfig = HostConfig.newHostConfig()
                        .withPortBindings(ports)
                        .withBinds(volumes)
                        .withAutoRemove(true);
                containerId = createContainerCmd
                        .withName(serviceConfig.getContainerName())
                        .withHostName(serviceConfig.getContainerName())
                        .withHostConfig(hostConfig)
                        .exec().getId();
                containerState = "created";
            }
        } else {
            // Get required details of the existing container
            containerId = container.get().getId();
            containerState = container.get().getState();
        }
        switch (containerState) {
            case "running":
                // The container is already running, all is fine.
                break;
            case "created":
                // The container is not running, start it.
                try (StartContainerCmd startContainerCmd = dockerClient.startContainerCmd(containerId)) {
                    startContainerCmd.exec();
                }
                break;
            default:
                // TODO Need to consider actions for other states
        }
    }

}
