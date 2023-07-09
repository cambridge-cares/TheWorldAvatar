package com.cmclinnovations.jurongisland;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;

public class EndpointConfig {

    private String kgurl;
    private String kguser;
    private String kgpassword;

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();

        BlazegraphEndpointConfig blazegraphEndpointConfig = containerClient.readEndpointConfig("blazegraph",
                BlazegraphEndpointConfig.class);
        this.kgurl = blazegraphEndpointConfig.getUrl("kb");
        this.kguser = blazegraphEndpointConfig.getUsername();
        this.kgpassword = blazegraphEndpointConfig.getPassword();
    }

    public String getKgurl() {
        return this.kgurl;
    }

    public String getKguser() {
        return this.kguser;
    }

    public String getKgpassword() {
        return this.kgpassword;
    }
}
