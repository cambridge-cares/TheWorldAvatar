package com.cmclinnovations.apis;

public abstract class AbstractEndpointConfig {

    private final String name;

    protected AbstractEndpointConfig(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}