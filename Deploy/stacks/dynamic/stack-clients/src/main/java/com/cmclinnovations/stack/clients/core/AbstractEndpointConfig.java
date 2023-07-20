package com.cmclinnovations.stack.clients.core;

public abstract class AbstractEndpointConfig implements EndpointConfig {

    private final String name;

    protected AbstractEndpointConfig(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }
}