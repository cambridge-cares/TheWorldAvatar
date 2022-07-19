package com.cmclinnovations.stack.clients.core;

import java.net.URL;

import com.cmclinnovations.stack.jackson.deserialisers.URLDeserialiser;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

public class RESTEndpointConfig extends PasswordEndpointConfig {

    @JsonDeserialize(using = URLDeserialiser.class)
    private final URL url;

    private final String userName;

    public RESTEndpointConfig() {
        this(null, null, null, null);
    }

    public RESTEndpointConfig(String name, URL url, String userName, String passwordFile) {
        super(name, passwordFile);
        this.url = url;
        this.userName = userName;
    }

    public URL getUrl() {
        return url;
    }

    public String getUserName() {
        return userName;
    }

}