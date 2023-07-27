package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.apache.jena.query.QueryParseException;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.agent.dashboard.utils.datamodel.Facility;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A client that provides methods to interact and store information from the SPARQL endpoint within a stack.
 *
 * @author qhouyee
 */
public class SparqlClient {
    private final String STACK_SPARQL_ENDPOINT;
    private final Map<String, Facility> INFRASTRUCTURES = new HashMap<>();
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
     * Get all infrastructure within the knowledge graph. This method is accessible for the stack client's usage.
     *
     * @return An array of all available infrastructure to monitor.
     */
    protected String[] getAllInfrastructure() {
        Set<String> infrastructures = this.INFRASTRUCTURES.keySet();
        return infrastructures.toArray(new String[infrastructures.size()]);
    }

    /**
     * Get all assets from a specific infrastructure in the knowledge graph.
     *
     * @param infrastructure The infrastructure to retrieve all assets.
     * @return An array of all available assets within a specific infrastructure in the knowledge graph.
     */
    protected String[] getAllAssets(String infrastructure) {
        return this.INFRASTRUCTURES.get(infrastructure).getAllAssets();
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
                .append("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
                .append("PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>")
                .append("SELECT ?facilityname ?elementname ?elementtype ")
                .append("WHERE {")
                .append("?building rdf:type bot:Building;")
                .append("   ontobim:hasFacility ?facility.")
                .append("?facility rdfs:label ?facilityname;")
                .append("   ontobim:hasRoom ?room.")
                .append("?room rdf:type ontobim:Room;")
                .append("   bot:containsElement ?element.")
                .append("?element rdfs:label ?elementname;")
                .append("   rdf:type ?elementtype.")
                .append("}");
        // Execute SELECT query and upon execution, run the following lines for each result row
        conn.querySelect(query.toString(), (qs) -> {
            // Retrieve relevant information
            String facilityName = qs.getLiteral("facilityname").toString();
            String assetName = qs.getLiteral("elementname").toString();
            String assetType = qs.getResource("elementtype").getLocalName();
            // Check if the facility already exists in the map
            if (this.INFRASTRUCTURES.containsKey(facilityName)) {
                // If it does exist, add the asset to the existing facility object
                Facility facility = this.INFRASTRUCTURES.get(facilityName);
                facility.addAsset(assetName, assetType);
            } else {
                // If it does not exist, initialise a new facility object and add it in
                Facility facility = new Facility(assetName, assetType);
                this.INFRASTRUCTURES.put(facilityName, facility);
            }
        });
    }
}
