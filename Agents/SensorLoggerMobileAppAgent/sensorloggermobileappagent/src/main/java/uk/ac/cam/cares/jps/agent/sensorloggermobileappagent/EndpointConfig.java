package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EndpointConfig {
    private String kgurl;

    private String dburl;
    private String dbuser;
    private String dbpassword;

    private String ontopUrl;

    private static final Logger LOGGER = LogManager.getLogger(EndpointConfig.class);

    public EndpointConfig() {
        BlazegraphEndpointConfig blazegraphEndpointConfig = BlazegraphClient.getInstance().getEndpoint();
        this.kgurl = blazegraphEndpointConfig.getUrl("kb");

        PostGISEndpointConfig postGISEndpointConfig = PostGISClient.getInstance().getEndpoint();
        this.dburl = postGISEndpointConfig.getJdbcURL("postgres");
        this.dbuser = postGISEndpointConfig.getUsername();
        this.dbpassword = postGISEndpointConfig.getPassword();

        ontopUrl = OntopClient.getInstance().getEndpoint().getUrl();
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

    public String getOntopUrl() {
        return ontopUrl;
    }
}
