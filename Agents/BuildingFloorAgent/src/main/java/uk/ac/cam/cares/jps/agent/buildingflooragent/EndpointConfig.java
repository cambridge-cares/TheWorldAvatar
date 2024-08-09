package uk.ac.cam.cares.jps.agent.buildingflooragent;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;

public class EndpointConfig {
    private PostGISEndpointConfig postGISEndpointConfig;
    private String dbUser;
    private String dbPassword;
    private String filePath = System.getenv("floors_csv");
    private String dbName = System.getenv("DATABASE");
    private String osmSchema = System.getenv("osmSchema");
    private String osmPoints = System.getenv("osmPoints");
    private String osmPolygons = System.getenv("osmPolygons");
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

    public String getOSMSchema() {
        return this.osmSchema;
    }

    public String getOSMPoints() {
        return this.osmPoints;
    }

    public String getOSMPolygons() {
        return this.osmPolygons;
    }

    public String getOntopUrl() {
        return this.ontopUrl;
    }
}
