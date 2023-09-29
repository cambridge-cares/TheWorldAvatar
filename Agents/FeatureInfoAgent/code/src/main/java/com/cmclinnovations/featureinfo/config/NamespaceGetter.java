package com.cmclinnovations.featureinfo.config;

import java.io.IOException;
import java.io.StringReader;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Attempts to use the Blazegraph REST API to get a list of available
 * namespaces. If successful these are then stored with their endpoints
 * within the ConfigStore instance.
 */
public class NamespaceGetter {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(NamespaceGetter.class);

    /**
     * Blazegraph root URL.
     */
    private String url;

    /**
     * Blazegraph username (may be null).
     */
    private String username;

    /**
     * Blazegraph password (may be null).
     */
    private String password;
    
    /**
     * Initialise a new instance.
     * 
     * @param url enforced root URL for Blazegraph
     * @param username Blazegraph username
     * @param password Blazegraph password.
     */
    public NamespaceGetter(String url, String username, String password) {
        this.url = url;
        this.username = username;
        this.password = password;
    }

    /**
     * Use the REST API to ask for a list of available namespaces.
     * 
     * @returns List of discovered endpoints.
     * 
     * @throws IllegalStateException if credentials are missing.
     * @throws IOException if cannot contact Blazegraph.
     */
    public List<StackEndpoint> discoverEndpoints() throws
        IllegalStateException,
        IOException,
        InterruptedException,
        ParserConfigurationException,
        SAXException {

        if(this.url == null) {
            throw new IllegalArgumentException("Root URL for Blazegraph is required!");
        }
        if(this.username != null && this.password == null) {
            throw new IllegalArgumentException("Must supply Blazegraph password if username is not null!");
        } else if (this.username == null && this.password != null) {
            throw new IllegalArgumentException("Must supply Blazegraph username if password is not null!");
        }

        // Endpoint collection
        List<StackEndpoint> endpoints = new ArrayList<>();

        // Build the request URL
        String requestURL = this.url;
        if(requestURL.endsWith("/")) {
            requestURL += "namespace?describe-each-named-graph=false";
        } else {
            requestURL += "/namespace?describe-each-named-graph=false";
        }
        
        // Create the client
        HttpClient client = HttpClient.newHttpClient();

        // Create the request
        HttpRequest request = null;
        if(username != null && password != null) {
            request = HttpRequest.newBuilder()
                .GET()
                .uri(URI.create(requestURL))
                .header("Authorization", getBasicAuthHeader())
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
            throw new IOException("Invalid HTTP response from Blazegraph service!");
        }

        // Parse the XML response
        parseResponse(endpoints, response.body());
        return endpoints;
    }

    /**
     * Parse the XML response from Blazegraph.
     * 
     * @param endpoints list of endpoints to add to.
     * @param response raw response string.
     * 
     * @throws IOException if XML string cannot be loaded.
     * @throws SAXExeption if XML is invalid
     * @throws ParserConfigurationException if parser is invalid.
     */
    protected void parseResponse(List<StackEndpoint> endpoints, String response) throws
        IOException,
        ParserConfigurationException,
        SAXException {

        Document xmlDoc = loadXMLFromString(response);
        NodeList descriptions = xmlDoc.getElementsByTagName("rdf:Description");
 
        // Iterate through XML to get namespace details
         for(int i = 0; i < descriptions.getLength(); i++) {
            Element description = (Element) descriptions.item(i);
         
            Node nameNode = description.getElementsByTagName("Namespace").item(0);
            String namespace = nameNode.getTextContent();
             
             // Do not get the URL from the node here, it's unreliable; build it instead.
            String namespaceURL = "";
            if(this.url.endsWith("/")) {
                namespaceURL = this.url + "namespace/" + namespace + "/sparql";
            } else {
                namespaceURL = this.url + "/namespace/" + namespace + "/sparql";
            }

            LOGGER.info("Have discovered a local Blazegraph endpoint: {}", namespaceURL);

            // Build endpoint object
            endpoints.add(new StackEndpoint(
                namespaceURL,
                this.username,
                this.password,
                StackEndpointType.BLAZEGRAPH
            ));
         }
    }

    /**
     * Encode username and password into base 64.
     * 
     * @return encoded string.
     */
    private String getBasicAuthHeader() {
        String valueToEncode = this.username + ":" + this.password;
        return "Basic " + Base64.getEncoder().encodeToString(valueToEncode.getBytes());
    }

    /**
     * Parse input XML string into a W3C DOM document.
     * 
     * @param xml raw content
     * 
     * @return W3C document
     * 
     * @throws IOException if XML string cannot be loaded.
     * @throws SAXExeption if XML is invalid
     * @throws ParserConfigurationException if parser is invalid.
     */
    private Document loadXMLFromString(String xml) throws
        IOException, 
        SAXException,
        ParserConfigurationException {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        InputSource is = new InputSource(new StringReader(xml));
        return builder.parse(is);
    }

}
// End of class.