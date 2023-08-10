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
import uk.ac.cam.cares.jps.agent.dashboard.utils.AgentCommunicationClient;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;
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
    private final Queue<String> STACK_SPARQL_ENDPOINTS = new ArrayDeque<>();
    private final Map<String, Facility> SPATIAL_ZONES = new HashMap<>();
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);

    /**
     * Standard Constructor to initialise this client.
     *
     * @param url The SPARQL url within the stack without the namespace.
     */
    public SparqlClient(String url, String username, String password) {
        // Retrieve all namespaces and generate their endpoints based on url
        getAllEndpoints(url, username, password);
        // Retrieve all metadata required for each endpoint
        for (String endpoint : this.STACK_SPARQL_ENDPOINTS) {
            try (RDFConnection conn = RDFConnection.connect(endpoint)) {
                this.retrieveMetaData(conn);
            } catch (QueryParseException e) {
                LOGGER.fatal("Invalid query: " + e.getMessage());
                throw new JPSRuntimeException("Invalid query: " + e.getMessage());
            } catch (Exception e) {
                LOGGER.fatal("Error connecting to SPARQL endpoint: " + e);
                throw new JPSRuntimeException("Error connecting to SPARQL endpoint: " + e);
            }
        }
    }

    /**
     * Retrieve all available endpoints in the stack's SPARQL knowledge graph.
     *
     * @param url      The stack's SPARQL blazegraph endpoint without namespaces.
     * @param username The username credentials for the stack's SPARQL blazegraph endpoint. Defaults to null if the endpoint is unauthenticated.
     * @param password The password credentials for the stack's SPARQL blazegraph endpoint. Defaults to "" if the endpoint is unauthenticated.
     */
    protected void getAllEndpoints(String url, String username, String password) {
        LOGGER.debug("Retrieving all SPARQL endpoints...");
        // Send a GET request to this specific url
        String requestUrl = url + "namespace?describe-each-named-graph=false";
        // If there is no password, send the GET request without an authentication header.
        HttpResponse response = password.isEmpty() ? AgentCommunicationClient.sendGetRequest(requestUrl) :
                // Else, the authentication header must be included
                AgentCommunicationClient.sendGetRequest(requestUrl, username, password);
        parseXmlNamespaces(response.body().toString(), url);
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
     * Parses the response into valid xml and extract only the namespace information required into endpoint format.
     *
     * @param responseBody The response's body content.
     * @param url          The SPARQL url within the stack without the namespace.
     */
    protected void parseXmlNamespaces(String responseBody, String url) {
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
            this.STACK_SPARQL_ENDPOINTS.offer(url + "namespace/" + namespace + "/sparql");
        }
    }

    /**
     * Retrieves metadata required for generating the dashboard syntax.
     *
     * @param conn Connection object to the SPARQL endpoint.
     */
    private void retrieveMetaData(RDFConnection conn) {
        StringBuilder query = new StringBuilder();
        query.append("PREFIX bot: <https://w3id.org/bot#>")
                .append("PREFIX ontobim: <https://www.theworldavatar.com/kg/ontobim/>")
                .append("PREFIX ontodevice: <https://www.theworldavatar.com/kg/ontodevice/>")
                .append("PREFIX ontotimeseries: <https://www.theworldavatar.com/kg/ontotimeseries/>")
                .append("PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>")
                .append("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
                .append("PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>")
                .append("PREFIX saref:<https://saref.etsi.org/core/>")
                .append("SELECT DISTINCT ?facilityname ?elementname ?elementtype ?measure ?measurename ?timeseries ")
                .append("WHERE {")
                // Query to get assets within a facility
                .append("?building rdf:type bot:Building;")
                .append("   ontobim:hasFacility ?facility.")
                .append("?facility rdfs:label ?facilityname;")
                .append("   ontobim:hasRoom ?room.")
                .append("?room rdf:type ontobim:Room;")
                .append("   bot:containsElement ?element.")
                .append("?element rdfs:label ?elementname;")
                .append("   rdf:type ?elementtype;")
                // Query to retrieve the time series associated with devices
                // The below line performs a recursive query to retrieve all sub devices in the possible permutations of:
                // Device sendsSignalTo subDevice; sendsSignalTo/consistsOf subDevice; consistsOf subDevice;
                // consistsOf/sendsSignalTo subDevice; consistsOf/sendsSignalTo/consistsOf subDevice
                .append("   ontodevice:sendsSignalTo*/saref:consistsOf*/ontodevice:sendsSignalTo*/saref:consistsOf* ?subdevices.")
                // Retrieve the measure and its name associated with either the element or their subdevices
                .append("{?element ontodevice:measures ?measurename.            ?measurename om:hasValue ?measure.}")
                .append("UNION {?element ontodevice:observes ?measure.          ?measure rdf:type ?measurename.}")
                .append("UNION {?subdevices ontodevice:measures ?measurename.   ?measurename om:hasValue ?measure.}")
                .append("UNION {?subdevices ontodevice:observes ?measure.       ?measure rdf:type ?measurename.}")
                // Once retrieved, all measures has a time series
                .append("?measure ontotimeseries:hasTimeSeries ?timeseries.")
                .append("}");
        // Execute SELECT query and upon execution, run the following lines for each result row
        conn.querySelect(query.toString(), (qs) -> {
            // Retrieve relevant information
            String facilityName = qs.getLiteral("facilityname").toString();
            String assetName = qs.getLiteral("elementname").toString();
            String assetType = qs.getResource("elementtype").getLocalName();
            String measureIri = qs.getResource("measure").toString();
            // Measure name might contain UUID
            String measureName = qs.getResource("measurename").getLocalName();
            measureName = StringHelper.removeUUID(measureName);
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
