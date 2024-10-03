package uk.ac.cam.cares.jps.agent.trafficincident;

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
    private String dbuser;
    private String dbpassword;
    
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

        OntopEndpointConfig ontopEndpointConfig = containerClient.readEndpointConfig("ontop", OntopEndpointConfig.class);
        this.ontopurl = ontopEndpointConfig.getUrl();

    }
    public String getKgUrl() {
        return this.kgurl;
    }
    public String getKgUser() {
        return this.kguser;
    }
    public String getKgPassword() {
        return this.kgpassword;
    }
    public String getOntopUrl() {
        return this.ontopurl;
    }
    public String getDbUrl() {
        return this.dburl;
    }
    public String getDbUser() {
        return this.dbuser;
    }
    public String getDbPassword() {
        return this.dbpassword;
    } 
}