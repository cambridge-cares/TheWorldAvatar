package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

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
        this.dburl = postGISEndpointConfig.getJdbcURL("postgres");
        this.dbuser = postGISEndpointConfig.getUsername();
        this.dbpassword = postGISEndpointConfig.getPassword();
    }
    public String getKgurl() {
        return this.kgurl;
    }
    public String getDburl() {
        return this.dburl;
    }
    public String getDbuser() {
        return this.dbuser;
    }
    public String getDbpassword() {
        return this.dbpassword;
    } 
}
