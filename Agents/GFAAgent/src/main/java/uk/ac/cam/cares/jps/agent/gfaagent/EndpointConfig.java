package uk.ac.cam.cares.jps.agent.gfaagent;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;

public class EndpointConfig {
    private PostGISEndpointConfig postGISEndpointConfig;
    private String dbUser;
    private String dbPassword;
    private String ontopUrl;

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        postGISEndpointConfig = containerClient.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        this.dbUser = postGISEndpointConfig.getUsername();
        this.dbPassword = postGISEndpointConfig.getPassword();
        OntopEndpointConfig ontopEndpointConfig = OntopClient.getInstance().getEndpoint();
        this.ontopUrl = ontopEndpointConfig.getUrl();
    }

    public String getDbUrl() {
        return this.postGISEndpointConfig.getJdbcURL(System.getenv("DATABASE"));
    }

    public String getDbUser() {
        return this.dbUser;
    }

    public String getDbPassword() {
        return this.dbPassword;
    }

    public String getOntopUrl() {
        return this.ontopUrl;
    }
}
