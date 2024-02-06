package uk.ac.cam.cares.jps.agent.gfaagent;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;

public class EndpointConfig {
    private PostGISEndpointConfig postGISEndpointConfig;
    private String dbUser;
    private String dbPassword;
    private BlazegraphEndpointConfig blazegraphEndpointConfig;
    private String kgurl;
    private String kguser;
    private String kgpassword;

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        postGISEndpointConfig = containerClient.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        this.dbUser = postGISEndpointConfig.getUsername();
        this.dbPassword = postGISEndpointConfig.getPassword();

        blazegraphEndpointConfig = containerClient.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class);
        this.kgurl = blazegraphEndpointConfig.getUrl("ontology");
        this.kguser = blazegraphEndpointConfig.getUsername();
        this.kgpassword = blazegraphEndpointConfig.getPassword();
   
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

    public String getKGUrl() {
        return this.kgurl;
    }

    public String getKGUser() {
        return this.kguser;
    }

    public String getKGPassword() {
        return this.kgpassword;
    }
}
