package com.cmclinnovations.services;

public abstract class Service {

    private final ServiceConfig config;

    Service(ServiceConfig config) {
        this.config = config;
    }

    protected ServiceConfig getConfig() {
        return config;
    }

}
