package uk.ac.cam.cares.jps.agent.bmsquery;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.core.AbstractEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.IOException;
import java.io.StringReader;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;

/**
 * This class acts as endpoint configuration. It fetches the blazegraph configuration from docker stack.
 * This class takes reference from <a href="https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_VIRTUALSENSOR/AermodAgent/src/main/java/com/cmclinnovations/aermod/EndpointConfig.java">...</a>
 * and <a href="https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/FeatureInfoAgent/code/src/main/java/com/cmclinnovations/featureinfo/config/NamespaceGetter.java">...</a>
 * It supports query from different namespaces.
 *
 * @author sandradeng20
 */
public class EndpointConfig {
    private List<String> kgurls;
    private String kguser;
    private String kgpassword;
    private String kgurl;

    private static final Logger LOGGER = LogManager.getLogger(EndpointConfig.class);

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        BlazegraphEndpointConfig blazegraphEndpointConfig = containerClient.readEndpointConfig("blazegraph",
                BlazegraphEndpointConfig.class);

        this.kguser = blazegraphEndpointConfig.getUsername();
        this.kgpassword = blazegraphEndpointConfig.getPassword();
        this.kgurl = blazegraphEndpointConfig.getUrl("lab");
    }

    /**
     * Getter for kguser
     * @return kguser
     */
    public String getKguser() {
        return this.kguser;
    }

    /**
     * Getter for kgpassword
     * @return kgpassword
     */
    public String getKgpassword() {
        return this.kgpassword;
    }

    public String getKgurl() {
        return kgurl;
    }
}
