package com.cmclinnovations.virtualsensor;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class EndpointConfig {
    private String dburl;
    private String dbuser;
    private String dbpassword;

    private String kgurl;
    private String kguser;
    private String kgpassword;
    private String ontopurl;

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        PostGISEndpointConfig postGISEndpointConfig = containerClient.readEndpointConfig("postgis",
                PostGISEndpointConfig.class);
        this.dburl = postGISEndpointConfig.getJdbcURL(EnvConfig.DATABASE);
        this.dbuser = postGISEndpointConfig.getUsername();
        this.dbpassword = postGISEndpointConfig.getPassword();

        BlazegraphEndpointConfig blazegraphEndpointConfig = containerClient.readEndpointConfig("blazegraph",
                BlazegraphEndpointConfig.class);
        this.kgurl = blazegraphEndpointConfig.getUrl("kb");
        this.kguser = blazegraphEndpointConfig.getUsername();
        this.kgpassword = blazegraphEndpointConfig.getPassword();

        OntopEndpointConfig ontopEndpointConfig = OntopClient.getInstance().getEndpoint();
        this.ontopurl = ontopEndpointConfig.getUrl();
    }

    public String getDburl() {
        return this.dburl;
    }

    public String getDbuser() {
        return this.dbuser;
    }

    public String getDbpassword() {
        return this.dbpassword;
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

    public String getOntopUrl() {
        return this.ontopurl;
    }
}
