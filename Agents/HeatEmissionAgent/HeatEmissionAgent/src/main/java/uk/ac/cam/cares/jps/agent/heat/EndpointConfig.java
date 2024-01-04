package uk.ac.cam.cares.jps.agent.heat;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class EndpointConfig {
    private PostGISEndpointConfig postGISEndpointConfig;
    private String dbUser;
    private String dbPassword;

    private String kgurl;
    private String kguser;
    private String kgpassword;

    public EndpointConfig(String namespace) {
        ContainerClient containerClient = new ContainerClient();
        BlazegraphEndpointConfig blazegraphEndpointConfig = containerClient.readEndpointConfig("blazegraph",
                BlazegraphEndpointConfig.class);
        this.kgurl = blazegraphEndpointConfig.getUrl(namespace);
        this.kguser = blazegraphEndpointConfig.getUsername();
        this.kgpassword = blazegraphEndpointConfig.getPassword();
        postGISEndpointConfig = containerClient.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        this.dbUser = postGISEndpointConfig.getUsername();
        this.dbPassword = postGISEndpointConfig.getPassword();
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

    public String getDbUrl(String dbName) {
        return this.postGISEndpointConfig.getJdbcURL(dbName);
    }

    public String getDbUser() {
        return this.dbUser;
    }

    public String getDbPassword() {
        return this.dbPassword;
    }

}
