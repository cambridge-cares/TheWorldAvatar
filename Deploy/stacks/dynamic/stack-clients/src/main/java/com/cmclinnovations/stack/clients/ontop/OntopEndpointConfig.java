package com.cmclinnovations.stack.clients.ontop;

import com.cmclinnovations.stack.clients.core.AbstractEndpointConfig;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
// Ignore these properties that were removed
@JsonIgnoreProperties({ "username", "passwordFile" })
public class OntopEndpointConfig extends AbstractEndpointConfig {

    private final String hostName;
    private final String port;

    @JsonProperty
    private final String url;

    protected OntopEndpointConfig() {
        this(null, null, null);
    }

    public OntopEndpointConfig(String name, String hostName, String port) {
        this(name, hostName, port, null);
    }

    public OntopEndpointConfig(String name, String hostName, String port, String url) {
        super(name);
        this.hostName = hostName;
        this.port = port;
        this.url = url;
    }

    public String getHostName() {
        return hostName;
    }

    public String getPort() {
        return port;
    }

    @JsonIgnore
    public String getUrl() {
        if (null == url) {
            return "http://" + hostName + ":" + port + "/sparql";
        } else {
            return url;
        }
    }
}