package com.cmclinnovations.services.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.ServiceSpec;
import com.github.dockerjava.api.model.TaskSpec;

public class ServiceConfig {

    private final String type;

    private final Map<String, Connection> endpoints;

    // Docker specific settings
    @JsonProperty("ServiceSpec")
    private ServiceSpec dockerServiceSpec;

    @JsonIgnore
    private final Map<String, String> environment = new HashMap<>();

    public ServiceConfig() {
        type = "container";
        endpoints = new HashMap<>();

        dockerServiceSpec = new ServiceSpec();
    }

    public String getName() {
        return dockerServiceSpec.getName();
    }

    public Map<String, Connection> getEndpoints() {
        return endpoints;
    }

    public String getImage() {
        return getContainerSpec().getImage();
    }

    private Map<String, String> getEnvironment() {
        List<String> env = getContainerSpec().getEnv();
        if (null == env) {
            env = new ArrayList<>();
            getContainerSpec().withEnv(env);
        } else {
            if (environment.isEmpty()) {
                environment.putAll(env.stream()
                        .collect(Collectors.toMap(
                                entry -> entry.split("=", 2)[0],
                                entry -> entry.split("=", 2)[1])));
            }
        }
        return environment;
    }

    public boolean hasEnvironmentVariable(String key) {
        return getEnvironment().containsKey(key);
    }

    public String getEnvironmentVariable(String key) {
        return getEnvironment().get(key);
    }

    @SuppressWarnings("java:S2259")
    public void setEnvironmentVariable(String key, String value) {
        getEnvironment().put(key, value);
        getContainerSpec().getEnv().add(key + "=" + value);
    }

    public ServiceSpec getDockerServiceSpec() {
        return dockerServiceSpec;
    }

    public TaskSpec getTaskTemplate() {
        return getDockerServiceSpec().getTaskTemplate();
    }

    public ContainerSpec getContainerSpec() {
        return getTaskTemplate().getContainerSpec();
    }

    public String getType() {
        return type;
    }

}
