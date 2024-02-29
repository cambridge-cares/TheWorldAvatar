package uk.ac.cam.cares.jps.agent.gfaagent;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;

public class EndpointConfig {
    private PostGISEndpointConfig postGISEndpointConfig;
    private String dbUser;
    private String dbPassword;
    private String filePath = System.getenv("floors_csv");
    public String dbName = System.getenv("DATABASE");

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        postGISEndpointConfig = containerClient.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        this.dbUser = postGISEndpointConfig.getUsername();
        this.dbPassword = postGISEndpointConfig.getPassword();
   
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

    public String getDbName() {
        return this.dbName;
    }

    public String getFilepath() {
        return this.filePath;
    }
}
