package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.apache.jena.query.QueryParseException;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.agent.dashboard.utils.ResponseHelper;
import uk.ac.cam.cares.jps.agent.dashboard.utils.datamodel.Facility;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.*;

/**
 * A client that provides methods to interact and store information from the SPARQL endpoint within a stack.
 *
 * @author qhouyee
 */
public class SparqlClient {
    private final String STACK_SPARQL_ENDPOINT;
    private final Map<String, Facility> SPATIAL_ZONES = new HashMap<>();
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);

    /**
     * Standard Constructor to initialise this client.
     *
     * @param endpoint The SPARQL endpoint within the stack.
     */
    public SparqlClient(String endpoint) {
        // Set up the required information for further interactions
        this.STACK_SPARQL_ENDPOINT = endpoint;
        // Retrieve all metadata required
        try (RDFConnection conn = RDFConnection.connect(this.STACK_SPARQL_ENDPOINT)) {
            this.retrieveMetaData(conn);
        } catch (QueryParseException e) {
            LOGGER.fatal("Invalid query: " + e.getMessage());
            throw new JPSRuntimeException("Invalid query: " + e.getMessage());
        } catch (Exception e) {
            LOGGER.fatal("Error connecting to SPARQL endpoint: " + e);
            throw new JPSRuntimeException("Error connecting to SPARQL endpoint: " + e);
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
     * Get all assets from a specific spatial zone in the knowledge graph.
     *
     * @param spatialZone The spatial zone to retrieve all assets.
     * @return An array of all available assets within a specific spatial zone in the knowledge graph.
     */
    protected Map<String, List<String>> getAllAssets(String spatialZone) {
        return this.SPATIAL_ZONES.get(spatialZone).getAllAssets();
    }

    /**
     * Get all measures from a specific spatial zone in the knowledge graph.
     *
     * @param spatialZone The spatial zone to retrieve all assets.
     * @return A map linking all assets to their measures.
     */
    protected Map<String, Queue<String[]>> getAllMeasures(String spatialZone) {
        return this.SPATIAL_ZONES.get(spatialZone).getAllMeasures();
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
            measureName = ResponseHelper.removeUUID(measureName);
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
