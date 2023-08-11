package uk.ac.cam.cares.jps.agent.dashboard.stack;

import java.io.IOException;
import java.io.StringReader;
import java.net.http.HttpResponse;

import org.apache.jena.query.QueryParseException;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.utils.SparqlAction;
import uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.utils.SparqlQuery;
import uk.ac.cam.cares.jps.agent.dashboard.utils.AgentCommunicationClient;
import uk.ac.cam.cares.jps.agent.dashboard.utils.datamodel.Facility;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import org.w3c.dom.Document;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.util.*;

/**
 * A client that provides methods to interact and store information from the SPARQL endpoint within a stack.
 *
 * @author qhouyee
 */
public class SparqlClient {
    private final List<String> SPATIAL_ZONE_SPARQL_ENDPOINTS = new ArrayList<>();
    private final List<String> REMAINING_SPARQL_ENDPOINTS = new ArrayList<>();
    private final Map<String, Facility> SPATIAL_ZONES = new HashMap<>();
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);

    /**
     * Standard Constructor to initialise this client.
     *
     * @param url The SPARQL url within the stack without the namespace.
     */
    public SparqlClient(String url, String username, String password) {
        // Retrieve all namespaces as a queue of endpoints
        Queue<String> sparqlEndpoints = getAllEndpoints(url, username, password);
        // While the queue is not empty, classify the endpoint as a spatial zone endpoint or not
        LOGGER.debug("Classifying all SPARQL endpoints...");
        while (!sparqlEndpoints.isEmpty()) {
            String endpoint = sparqlEndpoints.poll();
            executeSparqlAction(endpoint, this::classifyEndpointType);
        }
        LOGGER.debug("Retrieving all metadata from SPARQL endpoints...");
        // For each of the spatial zone endpoint, retrieve the metadata associated with them
        for (String endpoint : this.SPATIAL_ZONE_SPARQL_ENDPOINTS) {
            executeSparqlAction(endpoint, this::retrieveMetaData);
        }
    }

    /**
     * Get all spatial zones within the knowledge graph. This method is accessible for the stack client's usage.
     *
     * @return An array of all available spatial zones to monitor.
     */
    protected String[] getAllSpatialZones() {
        Set<String> spatialZones = this.SPATIAL_ZONES.keySet();
        return spatialZones.toArray(new String[spatialZones.size()]);
    }

    /**
     * Get all assets and their time series information from a specific spatial zone in the knowledge graph.
     *
     * @param spatialZone The spatial zone to retrieve all assets.
     * @return A map linking all assets to their measures.
     */
    protected Map<String, Queue<String[]>> getAllAssetMetaData(String spatialZone) {
        return this.SPATIAL_ZONES.get(spatialZone).getAllAssets();
    }

    /**
     * Retrieve all available endpoints in the stack's SPARQL knowledge graph.
     *
     * @param url      The stack's SPARQL blazegraph endpoint without namespaces.
     * @param username The username credentials for the stack's SPARQL blazegraph endpoint. Defaults to null if the endpoint is unauthenticated.
     * @param password The password credentials for the stack's SPARQL blazegraph endpoint. Defaults to "" if the endpoint is unauthenticated.
     * @return A queue containing all SPARQL endpoints.
     */
    private Queue<String> getAllEndpoints(String url, String username, String password) {
        LOGGER.debug("Retrieving all SPARQL endpoints...");
        // Send a GET request to this specific url
        String requestUrl = url + "namespace?describe-each-named-graph=false";
        // If there is no password, send the GET request without an authentication header.
        HttpResponse response = password.isEmpty() ? AgentCommunicationClient.sendGetRequest(requestUrl) :
                // Else, the authentication header must be included
                AgentCommunicationClient.sendGetRequest(requestUrl, username, password);
        return parseXmlNamespaces(response.body().toString(), url);
    }

    /**
     * Parses the response into valid xml and extract only the namespace information required into endpoint format.
     *
     * @param responseBody The response's body content.
     * @param url          The SPARQL url within the stack without the namespace.
     * @return A queue containing all SPARQL endpoints.
     */
    private Queue<String> parseXmlNamespaces(String responseBody, String url) {
        Queue<String> sparqlEndpoints = new ArrayDeque<>();
        // Parse the response into an XML document format
        Document xmlDoc;
        try {
            DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            InputSource is = new InputSource(new StringReader(responseBody));
            xmlDoc = builder.parse(is);
        } catch (ParserConfigurationException e) {
            LOGGER.fatal("Unable to create a DocumentBuilder which satisfies the configuration requested! Please see error for more details: " + e.getMessage());
            throw new JPSRuntimeException("Unable to create a DocumentBuilder which satisfies the configuration requested! Please see error for more details: " + e.getMessage());
        } catch (SAXException e) {
            LOGGER.fatal("Unable to parse the response into valid XML! Please see error for more details: " + e.getMessage());
            throw new JPSRuntimeException("Unable to parse the response into valid XML! Please see error for more details: " + e.getMessage());
        } catch (IOException e) {
            LOGGER.fatal("Unable to access the response string! Please see error for more details: " + e.getMessage());
            throw new JPSRuntimeException("Unable to access the response string! Please see error for more details: " + e.getMessage());
        }
        // Iterate through XML format to get each namespace details
        NodeList descriptions = xmlDoc.getElementsByTagName("rdf:Description");
        for (int i = 0; i < descriptions.getLength(); i++) {
            Element description = (Element) descriptions.item(i);
            // Do not get the URL from the node here, it's unreliable. Build it instead
            Node nameNode = description.getElementsByTagName("Namespace").item(0);
            String namespace = nameNode.getTextContent();
            // Construct a valid SPARQL endpoint, and store it
            sparqlEndpoints.offer(url + "namespace/" + namespace + "/sparql");
        }
        return sparqlEndpoints;
    }

    /**
     * Executes a SPARQL action on the provided SPARQL endpoint.
     *
     * @param endpoint The URL of the SPARQL endpoint.
     * @param action   The SparqlAction to be executed.
     * @throws JPSRuntimeException If an error occurs while executing the SPARQL action or connecting to the endpoint.
     */
    private void executeSparqlAction(String endpoint, SparqlAction action) {
        try (RDFConnection conn = RDFConnection.connect(endpoint)) {
            action.execute(conn, endpoint);
        } catch (QueryParseException e) {
            LOGGER.fatal("Invalid query: " + e.getMessage());
            throw new JPSRuntimeException("Invalid query: " + e.getMessage());
        } catch (Exception e) {
            LOGGER.fatal("Error connecting to SPARQL endpoint: " + e);
            throw new JPSRuntimeException("Error connecting to SPARQL endpoint: " + e);
        }
    }

    /**
     * Classifies the SPARQL endpoint depending on if they contain spatial zone information or not.
     * These endpoints will be stored in the corresponding internal list.
     *
     * @param conn     Connection object to the SPARQL endpoint.
     * @param endpoint The current SPARQL endpoint.
     */
    private void classifyEndpointType(RDFConnection conn, String endpoint) {
        // Executes a simple facility query to see if there are any facilities with assets.
        conn.queryResultSet(SparqlQuery.genSimpleFacilityQuery(), (resultSet) -> {
            // If there is at least one result, the current endpoint holds spatial zone information
            if (resultSet.hasNext()) {
                this.SPATIAL_ZONE_SPARQL_ENDPOINTS.add(endpoint); // Store different endpoint types in different lists
            } else {
                // If no results are available, this endpoint holds various information that are not on spatial zones
                this.REMAINING_SPARQL_ENDPOINTS.add(endpoint);
            }
        });
    }

    /**
     * Retrieves metadata required for generating the dashboard syntax.
     *
     * @param conn     Connection object to the SPARQL endpoint.
     * @param endpoint The current SPARQL endpoint.
     */
    private void retrieveMetaData(RDFConnection conn, String endpoint) {
        // Execute a SELECT query on the current spatial zone endpoint
        // As the time series triples are stored on the remaining endpoints, these have to be executed one by one using the SERVICE keyword
        // Effectively, we repeatedly perform the following steps for all remaining endpoints
        for (String serviceEndpoint : this.REMAINING_SPARQL_ENDPOINTS) {
            // Execute SELECT query and upon execution, run the following lines for each result row
            conn.querySelect(SparqlQuery.genFacilityMeasureQuery(serviceEndpoint), (qs) -> {
                // Retrieve relevant information
                String facilityName = qs.getLiteral("facilityname").toString();
                String assetName = qs.getLiteral("elementname").toString();
                String assetType = qs.getResource("elementtype").getLocalName();
                String measureIri = qs.getResource("measure").toString();
                // Measure name might contain UUID
                String measureName = qs.getLiteral("measurename").toString();
                String timeSeriesIri = qs.getResource("timeseries").toString();
                // Check if the facility already exists in the map
                if (this.SPATIAL_ZONES.containsKey(facilityName)) {
                    // If it does exist, add the asset to the existing facility object
                    Facility facility = this.SPATIAL_ZONES.get(facilityName);
                    facility.addAsset(assetName, assetType, measureName, measureIri, timeSeriesIri);
                } else {
                    // If it does not exist, initialise a new facility object and add it in
                    Facility facility = new Facility(assetName, assetType, measureName, measureIri, timeSeriesIri);
                    this.SPATIAL_ZONES.put(facilityName, facility);
                }
            });
        }
    }
}
