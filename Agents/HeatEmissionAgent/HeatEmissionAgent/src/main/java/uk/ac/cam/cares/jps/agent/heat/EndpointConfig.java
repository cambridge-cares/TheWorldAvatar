package uk.ac.cam.cares.jps.agent.heat;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class EndpointConfig {
    private PostGISEndpointConfig postGISEndpointConfig;
    private BlazegraphEndpointConfig blazegraphEndpointConfig;

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        this.blazegraphEndpointConfig = containerClient.readEndpointConfig("blazegraph",
                BlazegraphEndpointConfig.class);
        this.postGISEndpointConfig = containerClient.readEndpointConfig("postgis", PostGISEndpointConfig.class);
    }

    public String getKgUrl(String namespace) {
        return blazegraphEndpointConfig.getUrl(namespace);
    }

    public String getKgUser() {
        return blazegraphEndpointConfig.getUsername();
    }

    public String getKgPassword() {
        return blazegraphEndpointConfig.getPassword();
    }

    public String getDbUrl(String dbName) {
        return this.postGISEndpointConfig.getJdbcURL(dbName);
    }

    public String getDbUser() {
        return this.postGISEndpointConfig.getUsername();
    }

    public String getDbPassword() {
        return this.postGISEndpointConfig.getPassword();
    }
}