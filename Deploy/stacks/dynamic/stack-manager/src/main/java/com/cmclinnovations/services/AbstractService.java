package com.cmclinnovations.services;

import java.net.URI;
import java.util.Map;
import java.util.Objects;

public abstract class AbstractService implements Service {

    private final ServiceConfig config;
    private final ServiceManager serviceManager;

    AbstractService(ServiceManager serviceManager, ServiceConfig config) {
        Objects.requireNonNull(config, "Services require a 'ServiceConfig' to be specified.");
        this.serviceManager = serviceManager;
        this.config = config;
    }

    protected ServiceConfig getConfig() {
        return config;
    }

    public String getName() {
        return config.getName();
    }

    public Map<String, URI> getEndpoints() {
        return config.getEndpoints();
    }

    public URI getEndpoint(String endpointName) {
        return config.getEndpoints().get(endpointName);
    }

    public String getEnvironmentVariable(String key) {
        return config.getEnvironment().get(key);
    }

    public void setEnvironmentVariable(String key, String value) {
        config.getEnvironment().put(key, value);
    }

    <S extends Service> S getService(String otherServiceName) {
        return serviceManager.getService(otherServiceName);
    }
}
