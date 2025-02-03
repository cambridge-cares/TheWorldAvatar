package uk.ac.cam.cares.jps.agent.ceavisualisation;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class EndpointConfig {
    private PostGISEndpointConfig postGISEndpointConfig;
    private String dbUser;
    private String dbPassword;

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        postGISEndpointConfig = containerClient.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        this.dbUser = postGISEndpointConfig.getUsername();
        this.dbPassword = postGISEndpointConfig.getPassword();
    }

    /**
     * Returns URL to dbName
     * @param dbName name of PostgreSQL database
     * @return URL to dbName
     */
    public String getDbUrl(String dbName) {
        return this.postGISEndpointConfig.getJdbcURL(dbName);
    }

    /**
     * Returns PostgreSQL username
     * @return PostgreSQL username
     */
    public String getDbUser() {
        return this.dbUser;
    }

    /**
     * Returns PostgreSQL password
     * @return PostgreSQL password
     */
    public String getDbPassword() {
        return this.dbPassword;
    }
}
