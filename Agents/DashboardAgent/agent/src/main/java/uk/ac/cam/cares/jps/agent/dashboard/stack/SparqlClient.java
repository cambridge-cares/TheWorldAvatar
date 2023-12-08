package uk.ac.cam.cares.jps.agent.dashboard.stack;

import java.io.IOException;
import java.io.StringReader;
import java.net.http.HttpResponse;

import org.apache.jena.query.QueryParseException;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
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
import java.util.function.BiConsumer;
import java.util.function.Supplier;

/**
 * A client that provides methods to interact and store information from the SPARQL endpoint within a stack.
 *
 * @author qhouyee
 */
public class SparqlClient {
    private final List<String> spatialZoneSparqlEndpoints = new ArrayList<>();
    private final List<String> remainingSparqlEndpoints = new ArrayList<>();
    private final Map<String, Organisation> organisations = new HashMap<>();
    private static final Logger LOGGER = LogManager.getLogger(SparqlClient.class);

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
        for (String endpoint : this.spatialZoneSparqlEndpoints) {
            executeSparqlAction(endpoint, this::retrieveRoomMetaData);
            executeSparqlAction(endpoint, this::retrieveSystemMetaData);
            executeSparqlAction(endpoint, this::retrieveAssetMetaData);
        }
    }

    /**
     * Get all organisations within the knowledge graph that has facilities with measures for analytical use. This method is accessible for the stack client's usage.
     *
     * @return An array of all available organisations to monitor.
     */
    protected String[] getAllOrganisations() {
        Set<String> organisationsKey = this.organisations.keySet();
        return organisationsKey.toArray(new String[organisationsKey.size()]);
    }

    /**
     * Get all assets and rooms alongside their time series information associated with the spatial zones managed by the specified organisation in the knowledge graph.
     *
     * @param organisation The organisation with time series measures available in their facilities.
     * @return A map linking all rooms, systems, and assets to their measures within the specified organisation's facilities.
     * Format: {asset1: [measure1, dataIRI, timeseriesIRI, unit, assetType], [measure2, dataIRI, timeseriesIRI, null(if no unit), assetType]],
     * room1: [[measureName, dataIRI, timeseriesIRI, unit, rooms], [measureName, dataIRI, timeseriesIRI, unit, rooms]], ...],
     * system1: [[measureName, dataIRI, timeseriesIRI, unit, systems], [measureName, dataIRI, timeseriesIRI, unit, systems]], ...],
     * facilities: [[facility1, asset1InFacility1,system1InFacility1,...],[facility2, room1InFacility2,...]]
     * thresholds: [[measureName, min, max],...]}
     */
    protected Map<String, Queue<String[]>> getAllSpatialZoneMetaData(String organisation) {
        return this.organisations.get(organisation).getAllMeasures();
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
        HttpResponse<String> response = password.isEmpty() ? AgentCommunicationClient.sendGetRequest(requestUrl) :
                // Else, the authentication header must be included
                AgentCommunicationClient.sendGetRequest(requestUrl, username, password);
        return parseXmlNamespaces(response.body(), url);
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
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            InputSource is = new InputSource(new StringReader(responseBody));
            xmlDoc = builder.parse(is);
        } catch (ParserConfigurationException e) {
            LOGGER.fatal("Unable to create a DocumentBuilder which satisfies the configuration requested! Please see error for more details: ", e);
            throw new JPSRuntimeException("Unable to create a DocumentBuilder which satisfies the configuration requested! Please see error for more details: ", e);
        } catch (SAXException e) {
            LOGGER.fatal("Unable to parse the response into valid XML! Please see error for more details: ", e);
            throw new JPSRuntimeException("Unable to parse the response into valid XML! Please see error for more details: ", e);
        } catch (IOException e) {
            LOGGER.fatal("Unable to access the response string! Please see error for more details: ", e);
            throw new JPSRuntimeException("Unable to access the response string! Please see error for more details: ", e);
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
            LOGGER.fatal("Invalid query: ", e);
            throw new JPSRuntimeException("Invalid query: ", e);
        } catch (Exception e) {
            LOGGER.fatal("Error connecting to SPARQL endpoint: ", e);
            throw new JPSRuntimeException("Error connecting to SPARQL endpoint: ", e);
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
        conn.queryResultSet(SparqlQuery.genSimpleFacilityQuery(), resultSet -> {
            // If there is at least one result, the current endpoint holds spatial zone information
            if (resultSet.hasNext()) {
                this.spatialZoneSparqlEndpoints.add(endpoint); // Store different endpoint types in different lists
            } else {
                // If no results are available, this endpoint holds various information that are not on spatial zones
                this.remainingSparqlEndpoints.add(endpoint);
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
        retrieveMetaData(conn, SparqlQuery.ROOM_NAME, SparqlQuery::genFacilityRoomMeasureQuery, (qs, dataArray) -> {
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
            // Retrieve the organisation
            Organisation organisation = this.organisations.get(dataArray[0]);
            // Add the item
            organisation.addRoom(dataArray[1], dataArray[2], dataArray[3], dataArray[4], dataArray[5], dataArray[6]);
            if (!minThreshold.isEmpty() && !maxThreshold.isEmpty()) {
                organisation.addThresholds(dataArray[3], minThreshold, maxThreshold); // Add thresholds
            }
        });
    }

    /**
     * Retrieves system-related metadata required for generating the dashboard syntax.
     *
     * @param conn     Connection object to the SPARQL endpoint.
     * @param endpoint The current SPARQL endpoint.
     */
    private void retrieveSystemMetaData(RDFConnection conn, String endpoint) {
        retrieveMetaData(conn, SparqlQuery.SYSTEM_NAME, SparqlQuery::genFacilitySystemMeasureQuery, (qs, dataArray) -> {
            Organisation organisation = this.organisations.get(dataArray[0]);
            organisation.addSystem(dataArray[1], dataArray[2], dataArray[3], dataArray[4], dataArray[5], dataArray[6]);
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
        for (String serviceEndpoint : this.remainingSparqlEndpoints) {
            retrieveMetaData(conn, SparqlQuery.ELEMENT_NAME, () -> SparqlQuery.genFacilityAssetMeasureQuery(serviceEndpoint), (qs, dataArray) -> {
                // Retrieve asset type
                String assetType = qs.getResource(SparqlQuery.ELEMENT_TYPE).getLocalName();
                // Retrieve organisation
                Organisation organisation = this.organisations.get(dataArray[0]);
                // Add asset
                organisation.addAsset(dataArray[1], dataArray[2], assetType, dataArray[3], dataArray[4], dataArray[5], dataArray[6]);
            });
        }
    }

    /**
     * Retrieves metadata related to a specific item type required for generating the dashboard syntax.
     * <p>
     * This method executes a SELECT query on the provided SPARQL endpoint and processes the results
     * for metadata related to a specific item type. The retrieved metadata includes information such
     * as organisation name, facility name, object name, measure details, unit, and time series.
     *
     * @param conn          Connection object to the SPARQL endpoint.
     * @param objectNameVar The variable representing the specific item name in the SPARQL query.
     * @param querySupplier A supplier providing the SPARQL query for metadata retrieval.
     * @param dataConsumer  A bi-consumer to process the metadata for each result row.
     *                      The first parameter is the QuerySolution object, and the second parameter
     *                      is a String array containing metadata values in the order:
     *                      [orgName, facilityName, objectName, measureName, unit, measureIri, timeSeriesIri].
     */
    private void retrieveMetaData(RDFConnection conn, String objectNameVar, Supplier<String> querySupplier, BiConsumer<QuerySolution, String[]> dataConsumer) {
        // Execute the query in the supplier as a SELECT query
        conn.querySelect(querySupplier.get(), qs -> {
            // Retrieve the common variables
            String orgName = qs.getLiteral(SparqlQuery.ORGANISATION_NAME).toString();
            String facilityName = qs.getLiteral(SparqlQuery.FACILITY_NAME).toString();
            String objectName = qs.getLiteral(objectNameVar).toString();
            String measureIri = qs.getResource(SparqlQuery.MEASURE).toString();
            String measureName = qs.getLiteral(SparqlQuery.MEASURE_NAME).toString();
            // If there is no unit to retrieve, this will throw a null pointer exception
            String unit;
            try {
                unit = qs.getLiteral(SparqlQuery.UNIT).toString();
            } catch (NullPointerException ne) {
                unit = null; // But as this is an optional variable, we can ignore the error and explicitly treat it as null
            }
            String timeSeriesIri = qs.getResource(SparqlQuery.TIME_SERIES).toString();
            // Initialise the organisation if it hasn't yet
            if (!this.organisations.containsKey(orgName)) this.organisations.put(orgName, new Organisation());
            // Stores the values into a bi-consumer that can be retrieved in other functions
            dataConsumer.accept(qs, new String[]{orgName, facilityName, objectName, measureName, unit, measureIri, timeSeriesIri});
        });
    }
}
