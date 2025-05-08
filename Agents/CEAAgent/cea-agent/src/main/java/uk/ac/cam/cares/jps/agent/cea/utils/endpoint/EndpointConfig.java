package uk.ac.cam.cares.jps.agent.cea.utils.endpoint;

import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

public class EndpointConfig {
    private PostGISEndpointConfig postGISEndpointConfig;
    private String dbUser;
    private String dbPassword;
    private String ontopUrl;

    public EndpointConfig() {
        postGISEndpointConfig = PostGISClient.getInstance().readEndpointConfig();
        this.dbUser = postGISEndpointConfig.getUsername();
        this.dbPassword = postGISEndpointConfig.getPassword();
        OntopEndpointConfig ontopEndpointConfig = OntopClient.getInstance().readEndpointConfig();
        this.ontopUrl = ontopEndpointConfig.getUrl();
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

    /**
     * Returns Ontop endpoint URL
     * @return Ontop endpoint URL
     */
    public String getOntopUrl() {return this.ontopUrl;}
}
