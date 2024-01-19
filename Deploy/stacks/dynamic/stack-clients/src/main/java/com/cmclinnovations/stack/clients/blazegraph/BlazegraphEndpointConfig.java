package com.cmclinnovations.stack.clients.blazegraph;

import java.util.Objects;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class BlazegraphEndpointConfig extends PasswordEndpointConfig {

    private final String hostName;
    private final String port;
    private final String username;

    private final String url;

    protected BlazegraphEndpointConfig() {
        this(null, null, null, null, null);
    }

    public BlazegraphEndpointConfig(String name, String hostName, String port, String username, String passwordFile) {
        super(name, passwordFile);
        this.hostName = hostName;
        this.port = port;
        this.username = username;

        this.url = null;
    }

    public String getHostName() {
        return hostName;
    }

    public String getPort() {
        return port;
    }

    public String getUsername() {
        return username;
    }

    @JsonIgnore
    public String getServiceUrl() {
        return "http://" + hostName + ":" + port + "/blazegraph";
    }

    public String getUrl(String namespace) {
        if (null == url) {
            Objects.requireNonNull(namespace,
                    "If the 'url' is not explicitly specified then a namespace must be specified in the code.");
            return getServiceUrl() + "/namespace/" + namespace + "/sparql/";
        } else {
            return url;
        }
    }
}