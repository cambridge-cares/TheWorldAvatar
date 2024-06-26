package com.cmclinnovations.aermod;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class EndpointConfig {
    private String kgurl;
    private String kguser;
    private String kgpassword;

    private String ontopurl;

    private String dburl;
    private String pythonDBURL;
    private String dbuser;
    private String dbpassword;
    private String dbDriver;

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        BlazegraphEndpointConfig blazegraphEndpointConfig = containerClient.readEndpointConfig("blazegraph",
                BlazegraphEndpointConfig.class);
        this.kgurl = blazegraphEndpointConfig.getUrl("kb");
        this.kguser = blazegraphEndpointConfig.getUsername();
        this.kgpassword = blazegraphEndpointConfig.getPassword();

        PostGISEndpointConfig postGISEndpointConfig = containerClient.readEndpointConfig("postgis",
                PostGISEndpointConfig.class);
        this.dburl = postGISEndpointConfig.getJdbcURL(EnvConfig.DATABASE);
        this.dbuser = postGISEndpointConfig.getUsername();
        this.dbpassword = postGISEndpointConfig.getPassword();
        this.pythonDBURL = postGISEndpointConfig.getSQLALchemyURL(EnvConfig.DATABASE);
        this.dbDriver = postGISEndpointConfig.getJdbcDriver();

        OntopEndpointConfig ontopEndpointConfig = containerClient.readEndpointConfig("ontop",
                OntopEndpointConfig.class);
        this.ontopurl = ontopEndpointConfig.getUrl();
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

    public String getOntopurl() {
        return this.ontopurl;
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

    public String getpythonDBURL() {
        return this.pythonDBURL;
    }

    public String getDBDriver() {
        return this.dbDriver;
    }
}
