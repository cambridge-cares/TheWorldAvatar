package uk.ac.cam.cares.jps.agent.dashboard.stack;

import java.io.IOException;
import java.io.StringReader;
import java.net.http.HttpResponse;

import org.apache.jena.query.QueryParseException;
import org.apache.jena.rdf.model.Literal;
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
import uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel.Organisation;
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
    private final Map<String, Organisation> ORGANISATIONS = new HashMap<>();
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
            executeSparqlAction(endpoint, this::retrieveRoomMetaData);
            executeSparqlAction(endpoint, this::retrieveAssetMetaData);
        }
    }

    /**
     * Get all spatial zones within the knowledge graph. This method is accessible for the stack client's usage.
     *
     * @return An array of all available spatial zones to monitor.
     */
    protected String[] getAllSpatialZones() {
        Set<String> spatialZones = this.ORGANISATIONS.keySet();
        return spatialZones.toArray(new String[spatialZones.size()]);
    }

    /**
     * Get all assets and rooms alongside their time series information from a specific spatial zone in the knowledge graph.
     *
     * @param spatialZone The spatial zone to retrieve all their associated time series.
     * @return A map linking all rooms and assets to their measures.
     */
    protected Map<String, Queue<String[]>> getAllSpatialZoneMetaData(String spatialZone) {
        return this.ORGANISATIONS.get(spatialZone).getAllMeasures();
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
     * Retrieves room-related metadata required for generating the dashboard syntax.
     *
     * @param conn     Connection object to the SPARQL endpoint.
     * @param endpoint The current SPARQL endpoint.
     */
    private void retrieveRoomMetaData(RDFConnection conn, String endpoint) {
        // Execute SELECT query and upon execution, run the following lines for each result row
        conn.querySelect(SparqlQuery.genFacilityRoomMeasureQuery(), (qs) -> {
            // Retrieve relevant information
            String orgName = qs.getLiteral(SparqlQuery.ORGANISATION_NAME).toString();
            String facilityName = qs.getLiteral(SparqlQuery.FACILITY_NAME).toString();
            String roomName = qs.getLiteral(SparqlQuery.ROOM_NAME).toString();
            String measureIri = qs.getResource(SparqlQuery.MEASURE).toString();
            String measureName = qs.getLiteral(SparqlQuery.MEASURE_NAME).toString();
            // If there is no unit to retrieve, this will throw a null pointer exception
            String unit;
            try {
                unit = qs.getLiteral(SparqlQuery.UNIT).toString();
            } catch (NullPointerException ne) {
                // But as this is an optional variable, we can ignore the error and explicitly treat it as null
                unit = null;
            }
            String timeSeriesIri = qs.getResource(SparqlQuery.TIME_SERIES).toString();
            // Default values for thresholds if unavailable
            String minThreshold = "";
            String maxThreshold = "";
            // Ensure that there are literals to process
            if (qs.getLiteral(SparqlQuery.MIN_THRESHOLD) != null && qs.getLiteral(SparqlQuery.MAX_THRESHOLD) != null) {
                Literal minLiteral = qs.getLiteral(SparqlQuery.MIN_THRESHOLD);
                Literal maxLiteral = qs.getLiteral(SparqlQuery.MAX_THRESHOLD);
                // Retrieves their value (typically an integer) and convert it into String
                minThreshold = String.valueOf(minLiteral.getValue());
                maxThreshold = String.valueOf(maxLiteral.getValue());
            }
            // Check if the organisation already exists in the map
            if (this.ORGANISATIONS.containsKey(facilityName)) {
                // If it does exist, add the room to the existing organisation object
                Organisation organisation = this.ORGANISATIONS.get(facilityName);
                organisation.addRoom(roomName, measureName, unit, measureIri, timeSeriesIri);
                if (!minThreshold.isEmpty() && !maxThreshold.isEmpty())
                    organisation.addThresholds(measureName, minThreshold, maxThreshold); // Add thresholds
            } else {
                // If it does not exist, initialise a new organisation object and add it in
                Organisation organisation = new Organisation(roomName, measureName, unit, measureIri, timeSeriesIri);
                if (!minThreshold.isEmpty() && !maxThreshold.isEmpty())
                    organisation.addThresholds(measureName, minThreshold, maxThreshold); // Add thresholds
                this.ORGANISATIONS.put(facilityName, organisation);
            }
        });
    }

    /**
     * Retrieves asset-related metadata required for generating the dashboard syntax.
     *
     * @param conn     Connection object to the SPARQL endpoint.
     * @param endpoint The current SPARQL endpoint.
     */
    private void retrieveAssetMetaData(RDFConnection conn, String endpoint) {
        // Execute a SELECT query on the current spatial zone endpoint
        // As the time series triples are stored on the remaining endpoints, these have to be executed one by one using the SERVICE keyword
        // Effectively, we repeatedly perform the following steps for all remaining endpoints
        for (String serviceEndpoint : this.REMAINING_SPARQL_ENDPOINTS) {
            // Execute SELECT query and upon execution, run the following lines for each result row
            conn.querySelect(SparqlQuery.genFacilityAssetMeasureQuery(serviceEndpoint), (qs) -> {
                // Retrieve relevant information
                String orgName = qs.getLiteral(SparqlQuery.ORGANISATION_NAME).toString();
                String facilityName = qs.getLiteral(SparqlQuery.FACILITY_NAME).toString();
                String assetName = qs.getLiteral(SparqlQuery.ELEMENT_NAME).toString();
                String assetType = qs.getResource(SparqlQuery.ELEMENT_TYPE).getLocalName();
                String measureIri = qs.getResource(SparqlQuery.MEASURE).toString();
                String measureName = qs.getLiteral(SparqlQuery.MEASURE_NAME).toString();
                // If there is no unit to retrieve, this will throw a null pointer exception
                String unit;
                try {
                    unit = qs.getLiteral(SparqlQuery.UNIT).toString();
                } catch (NullPointerException ne) {
                    // But as this is an optional variable, we can ignore the error and explicitly treat it as null
                    unit = null;
                }
                String timeSeriesIri = qs.getResource(SparqlQuery.TIME_SERIES).toString();
                // Check if the organisation already exists in the map
                if (this.ORGANISATIONS.containsKey(facilityName)) {
                    // If it does exist, add the asset to the existing organisation object
                    Organisation organisation = this.ORGANISATIONS.get(facilityName);
                    organisation.addAsset(assetName, assetType, measureName, unit, measureIri, timeSeriesIri);
                } else {
                    // If it does not exist, initialise a new organisation object and add it in
                    Organisation organisation = new Organisation(assetName, assetType, measureName, unit, measureIri, timeSeriesIri);
                    this.ORGANISATIONS.put(facilityName, organisation);
                }
            });
        }
    }
}
