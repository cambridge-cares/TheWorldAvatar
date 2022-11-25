package com.cmclinnovations.featureinfo.config;

import java.io.IOException;
import java.io.StringReader;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;

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
     */
    public NamespaceGetter(String url) {
        this(url, null, null);
    }

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
     * Return determined Blazegraph username.
     * 
     * @return username
     */
    public String getUsername() {
        return this.username;
    }

    /**
     * Return determined Blazegraph password.
     * 
     * @return password
     */
    public String getPassword() {
        return this.password;
    }

    /**
     * Use the stack client library to get the root URL of Blazegraph.
     * 
     * @throws IllegalStateException if root URL cannot be determined.
     */
    private void getRoot() throws Exception {
        ContainerClient client = FeatureInfoAgent.CONFIG;
        BlazegraphEndpointConfig blazeConfig = client.readEndpointConfig(
            "blazegraph", 
            BlazegraphEndpointConfig.class
        );

        this.username = blazeConfig.getUsername();
        this.password = blazeConfig.getPassword();
        this.url = blazeConfig.getServiceUrl();

        LOGGER.info("Determined base URL of Blazegraph as: {}", this.url);
        if(this.url == null || this.url.isEmpty()) {
            throw new IllegalStateException("Cannot determine root URL of Blazegraph.");
        }
    }

    /**
     * Use the REST API to ask for a list of available namespaces.
     */
    public Map<String, String> listNamespaces() throws Exception {
        // If not already provided, then get the Blazegraph root from the stack
        if(this.url == null) getRoot();

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

        return parseResponse(response.body());
    }

    /**
     * Parse the response from Blazegraph
     * @param response
     * @return
     */
    protected Map<String, String> parseResponse(String response) throws Exception {
         // Iterate through XML to get namespace details
         Map<String, String> namespaces = new HashMap<>();

         Document xmlDoc = loadXMLFromString(response);
         NodeList descriptions = xmlDoc.getElementsByTagName("rdf:Description");
 
         for(int i = 0; i < descriptions.getLength(); i++) {
             Element description = (Element) descriptions.item(i);
         
             Node nameNode = description.getElementsByTagName("Namespace").item(0);
             String namespace = nameNode.getTextContent();
             
             // Do not get the URL from the node here, it's unreliable. Build it instead
             String endpoint = "";
             if(this.url.endsWith("/")) {
                endpoint = this.url + "namespace/" + namespace + "/sparql";
             } else {
                endpoint = this.url + "/namespace/" + namespace + "/sparql";
             }
     
            namespaces.put(namespace, endpoint);
         }
         return namespaces;
    }

    /**
     * Encode username and password into base 64.
     * 
     * @param username username 
     * @param password password
     * 
     * @return encoded string
     */
    private String getBasicAuthHeader(String username, String password) {
        String valueToEncode = username + ":" + password;
        return "Basic " + Base64.getEncoder().encodeToString(valueToEncode.getBytes());
    }

    /**
     * Parse input XML string into a W3C DOM document.
     * 
     * @param xml raw content
     * 
     * @return W3C document
     * 
     * @throws Exception if string is not valid XML
     */
    private Document loadXMLFromString(String xml) throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        InputSource is = new InputSource(new StringReader(xml));
        return builder.parse(is);
    }

}
// End of class.