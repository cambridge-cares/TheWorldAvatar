package com.cmclinnovations.stack.clients.core;

public class BasicEndpointConfig extends AbstractEndpointConfig {

    private final String url;

    public BasicEndpointConfig() {
        this(null, null);
    }

    public BasicEndpointConfig(String name, String url) {
        super(name);
        this.url = url;
    }

    public String getUrl() {
        return url;
    }

}