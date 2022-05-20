package com.cmclinnovations.services;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Optional;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.ConnectToNetworkCmd;
import com.github.dockerjava.api.command.CreateContainerCmd;
import com.github.dockerjava.api.command.CreateNetworkCmd;
import com.github.dockerjava.api.command.InspectNetworkCmd;
import com.github.dockerjava.api.command.ListContainersCmd;
import com.github.dockerjava.api.command.ListNetworksCmd;
import com.github.dockerjava.api.command.PullImageCmd;
import com.github.dockerjava.api.command.PullImageResultCallback;
import com.github.dockerjava.api.command.StartContainerCmd;
import com.github.dockerjava.api.model.Container;
import com.github.dockerjava.api.model.Network;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DefaultDockerClientConfig.Builder;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.httpclient5.ApacheDockerHttpClient;
import com.github.dockerjava.transport.DockerHttpClient;

public class DockerService extends AbstractService {

    static final String TYPE = "docker";

    private final DockerClient dockerClient;
    private Network network;

    public DockerService(ServiceManager serviceManager, ServiceConfig config) throws URISyntaxException {
        super(serviceManager, config);

        Builder dockerConfigBuilder = DefaultDockerClientConfig.createDefaultConfigBuilder();

        URI endpoint = getEndpoint("dockerHost");
        if (null != endpoint) {
            dockerConfigBuilder.withDockerHost(endpoint.toString());
            // TODO need to set up TLS so that the unsecured Docker port "2375" doesn't need
            // to be opened.
            // dockerConfigBuilder.withDockerTlsVerify(true);
        }

        DockerClientConfig dockerConfig = dockerConfigBuilder.build();

        DockerHttpClient httpClient = new ApacheDockerHttpClient.Builder()
                .dockerHost(dockerConfig.getDockerHost())
                .sslConfig(dockerConfig.getSSLConfig())
                .build();

        dockerClient = DockerClientBuilder.getInstance(dockerConfig).withDockerHttpClient(httpClient).build();
    }

    public void createNetwork(String name) {
        Optional<Network> potentialNetwork;
        try (ListNetworksCmd listNetworksCmd = dockerClient.listNetworksCmd()) {
            potentialNetwork = listNetworksCmd.withNameFilter(name).exec().stream().findAny();
        }
        if (potentialNetwork.isEmpty()) {
            try (CreateNetworkCmd createNetworkCmd = dockerClient.createNetworkCmd()) {
                createNetworkCmd.withName(name).withAttachable(true).withCheckDuplicate(true).exec();
                try (ListNetworksCmd listNetworksCmd = dockerClient.listNetworksCmd()) {
                    potentialNetwork = listNetworksCmd.withNameFilter(name).exec().stream().findAny();
                }
            }
        }
        potentialNetwork.ifPresent(nw -> this.network = nw);
    }

    private Optional<Container> getContainer(ContainerService service) {
        try (ListContainersCmd listContainersCmd = dockerClient.listContainersCmd()) {
            // Setting "showAll" to "true" ensures non-running containers are also returned
            return listContainersCmd.withNameFilter(List.of(service.getContainerName()))
                    .withShowAll(true).exec()
                    .stream().findAny();
        }
    }

    public boolean isContainerUp(ContainerService service) {
        try (ListContainersCmd listContainersCmd = dockerClient.listContainersCmd()) {
            // Don't need to filter for "running" state as this is the default setting
            return !listContainersCmd.withNameFilter(List.of(service.getContainerName())).exec().isEmpty();
        }
    }

    public String getContainerID(ContainerService service) {
        return getContainer(service).map(Container::getId).orElseThrow();
    }

    public void startContainer(ContainerService service) {
        final String containerId;
        final String containerState;
        Optional<Container> container = getContainer(service);
        if (container.isEmpty()) {
            // No container matching that config

            String image = service.getImage();
            if (dockerClient.listImagesCmd().withImageNameFilter(image).exec().isEmpty()) {
                // No image with the requested image ID, so try to pull image
                try (PullImageCmd pullImageCmd = dockerClient.pullImageCmd(image)) {
                    pullImageCmd
                            .exec(new PullImageResultCallback())
                            .awaitCompletion();
                } catch (InterruptedException e) {
                    throw new RuntimeException("Docker image pull command interupted", e);
                }
            }

            // Create container
            try (CreateContainerCmd createContainerCmd = dockerClient
                    .createContainerCmd(image)) {
                containerId = createContainerCmd
                        .withName(service.getContainerName())
                        .withHostName(service.getName())
                        .withHostConfig(service.getHostConfig())
                        .withEnv(service.getEnvironment())
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
                throw new IllegalStateException("Container '" + containerId + "' in a state (" + containerState
                        + ") that is currently unsupported in the DockerService::startContainer method.");

        }

        // Add container to the stack's network, if not already added
        try (InspectNetworkCmd inspectNetworkCmd = dockerClient.inspectNetworkCmd()) {
            if (null == inspectNetworkCmd.withNetworkId(network.getId()).exec().getContainers().get(containerId)) {
                try (ConnectToNetworkCmd connectToNetworkCmd = dockerClient.connectToNetworkCmd()) {
                    connectToNetworkCmd.withContainerId(containerId).withNetworkId(network.getId()).exec();
                }
            }
        }

        service.setContainerId(containerId);
    }

}
