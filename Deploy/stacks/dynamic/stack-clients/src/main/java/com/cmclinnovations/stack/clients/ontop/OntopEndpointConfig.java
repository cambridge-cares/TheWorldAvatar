package com.cmclinnovations.stack.clients.ontop;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class OntopEndpointConfig extends PasswordEndpointConfig {

    private final String hostName;
    private final String port;
    private final String username;

    private final String url;

    protected OntopEndpointConfig() {
        this(null, null, null, null, null);
    }

    public OntopEndpointConfig(String name, String hostName, String port, String username, String passwordFile) {
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

    public String getUrl() {
        if (null == url) {
            return "http://" + hostName + ":" + port + "/sparql/";
        } else {
            return url;
        }
    }
}