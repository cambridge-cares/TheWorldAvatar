package com.cmclinnovations.clients.core;

public abstract class AbstractEndpointConfig {

    private final String name;

    protected AbstractEndpointConfig(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}