package com.cmclinnovations.services;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.github.dockerjava.api.model.HostConfig;

public class ContainerService extends AbstractService {

    public static final String TYPE = "container";

    private final String stackName;
    private String containerId;

    public ContainerService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(serviceManager, config);
        Objects.requireNonNull(stackName, "A 'stackName' must be provided for all container-based services.");
        this.stackName = stackName;
    }

    String getContainerName() {
        return stackName + "_" + getName();
    }

    String getImage() {
        return getConfig().getImage();
    }

    HostConfig getHostConfig() {
        return getConfig().getDockerHostConfig();
    }

    List<String> getEnvironment() {
        return getConfig().getEnvironment().entrySet().stream()
                .map(entry -> entry.getKey() + "=" + entry.getValue())
                .collect(Collectors.toList());
    }

    String getContainerId() {
        return containerId;
    }

    void setContainerId(String containerId) {
        this.containerId = containerId;
    }

}
