package com.cmclinnovations.services;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.github.dockerjava.api.model.Container;
import com.github.dockerjava.api.model.HostConfig;

public class ContainerService extends AbstractService {

    private final String stackName;
    private Container container;

    public ContainerService(String stackName, ServiceConfig config) {
        super(config);
        Objects.requireNonNull(stackName, "A 'stackName' must be provided for all container-based services.");
        this.stackName = stackName;
    }

    public String getContainerName() {
        return stackName + "_" + getName();
    }

    public String getImage() {
        return getConfig().getImage();
    }

    public Container getContainer() {
        return container;
    }

    void setContainer(Container container) {
        this.container = container;
    }

    public HostConfig getHostConfig() {
        return getConfig().getDockerHostConfig();
    }

    public List<String> getEnvironment() {
        return getConfig().getEnvironment().entrySet().stream()
                .map(entry -> entry.getKey() + "=" + entry.getValue())
                .collect(Collectors.toList());
    }

}
