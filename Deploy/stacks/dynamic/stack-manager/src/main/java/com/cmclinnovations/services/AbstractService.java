package com.cmclinnovations.services;

public abstract class AbstractService implements Service {

    private final ServiceConfig config;

    AbstractService(ServiceConfig config) {
        this.config = config;
    }

    public ServiceConfig getConfig() {
        return config;
    }

}
