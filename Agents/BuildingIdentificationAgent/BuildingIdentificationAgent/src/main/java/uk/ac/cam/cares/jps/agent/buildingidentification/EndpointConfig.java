package uk.ac.cam.cares.jps.agent.buildingidentification;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class EndpointConfig {
    private PostGISEndpointConfig postGISEndpointConfig;
    private BlazegraphEndpointConfig blazegraphEndpointConfig;
    private String dbUser;
    private String dbPassword;

    private String kguser;
    private String kgpassword;

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        blazegraphEndpointConfig = containerClient.readEndpointConfig("blazegraph",
                BlazegraphEndpointConfig.class);
        this.kguser = blazegraphEndpointConfig.getUsername();
        this.kgpassword = blazegraphEndpointConfig.getPassword();
        postGISEndpointConfig = containerClient.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        this.dbUser = postGISEndpointConfig.getUsername();
        this.dbPassword = postGISEndpointConfig.getPassword();
    }

    public String getKgurl(String namespace) {
        return blazegraphEndpointConfig.getUrl(namespace);
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
