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

public class EndpointConfig {
    private List<String> kgurls;
    private String kguser;
    private String kgpassword;

    private String ontopurl;

    private String dburl;
    private String dbuser;
    private String dbpassword;

    private static final Logger LOGGER = LogManager.getLogger(EndpointConfig.class);

    public EndpointConfig() {
        ContainerClient containerClient = new ContainerClient();
        BlazegraphEndpointConfig blazegraphEndpointConfig = containerClient.readEndpointConfig("blazegraph",
                BlazegraphEndpointConfig.class);
        try {
            this.kgurls = getKgUrls(blazegraphEndpointConfig);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to get blazegraph namespaces.");
        }
        this.kguser = blazegraphEndpointConfig.getUsername();
        this.kgpassword = blazegraphEndpointConfig.getPassword();

        PostGISEndpointConfig postGISEndpointConfig = containerClient.readEndpointConfig("postgis",
                PostGISEndpointConfig.class);
        this.dburl = postGISEndpointConfig.getJdbcURL("bms");
        this.dbuser = postGISEndpointConfig.getUsername();
        this.dbpassword = postGISEndpointConfig.getPassword();

        OntopEndpointConfig ontopEndpointConfig = containerClient.readEndpointConfig("ontop", OntopEndpointConfig.class);
        this.ontopurl = ontopEndpointConfig.getUrl();
    }

    private String getBasicAuthHeader(String username, String password) {
        String valueToEncode = username + ":" + password;
        return "Basic " + Base64.getEncoder().encodeToString(valueToEncode.getBytes());
    }

    private Document loadXMLFromString(String xml) throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        InputSource is = new InputSource(new StringReader(xml));
        return builder.parse(is);
    }

    private List<String> parseResponse(String response, BlazegraphEndpointConfig blazegraphEndpointConfig) throws Exception {
        List<String> namespaces = new ArrayList<>();

        Document xmlDoc = loadXMLFromString(response);
        NodeList descriptions = xmlDoc.getElementsByTagName("rdf:Description");

        for(int i = 0; i < descriptions.getLength(); i++) {
            Element description = (Element) descriptions.item(i);

            Node nameNode = description.getElementsByTagName("Namespace").item(0);
            String namespace = nameNode.getTextContent();

            // Do not get the URL from the node here, it's unreliable. Build it instead
            String endpoint = blazegraphEndpointConfig.getUrl(namespace);

            namespaces.add(endpoint);
        }

        LOGGER.info("The following endpoints are found: " + String.join(",", namespaces));

        return namespaces;
    }

    private List<String> getKgUrls(BlazegraphEndpointConfig blazegraphEndpointConfig) throws Exception {

        // Build the request URL
        String requestURL = "http://" + blazegraphEndpointConfig.getHostName() + ":" + blazegraphEndpointConfig.getPort() + "/blazegraph/namespace?describe-each-named-graph=false";

        // Create the client
        HttpClient client = HttpClient.newHttpClient();

        // Create the request
        HttpRequest request = null;
        String username = blazegraphEndpointConfig.getUsername();
        String password = blazegraphEndpointConfig.getPassword();

        if(username != null && password != null) {
            request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(requestURL))
                    .header("Authorization", getBasicAuthHeader(username, password))
                    .build();
        } else {
            request = HttpRequest.newBuilder()
                    .GET()
                    .uri(URI.create(requestURL))
                    .build();
        }

        // Send the request
        HttpResponse<String> response = client.send(
                request,
                HttpResponse.BodyHandlers.ofString()
        );

        // Parse the response
        if(response.statusCode() != 200 || response.body() == null) {
            throw new IOException("Invalid response from Blazegraph service.");
        }

        return parseResponse(response.body(), blazegraphEndpointConfig);
    }

    public List<String> getKgurls() {
        return this.kgurls;
    }

    public String getKguser() {
        return this.kguser;
    }

    public String getKgpassword() {
        return this.kgpassword;
    }

    public String getOntopurl() {
        return this.ontopurl;
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
