package com.cmclinnovations.services;

import java.net.URL;
import java.util.Map;
import java.util.Objects;

public abstract class AbstractService {

    private final ServiceConfig config;

    AbstractService(ServiceConfig config) {
        Objects.requireNonNull(config, "Services require a 'ServiceConfig' to be specified.");
        this.config = config;
    }

    protected ServiceConfig getConfig() {
        return config;
    }

    public String getName() {
        return config.getName();
    }

    public Map<String, URL> getEndpoints() {
        return config.getEndpoints();
    }

    public URL getEndpoint(String endpointName) {
        return config.getEndpoints().get(endpointName);
    }

    public String getEnvironmentVariable(String key) {
        return config.getEnvironment().get(key);
    }

    public void setEnvironmentVariable(String key, String value) {
        config.getEnvironment().put(key, value);
    }
}
