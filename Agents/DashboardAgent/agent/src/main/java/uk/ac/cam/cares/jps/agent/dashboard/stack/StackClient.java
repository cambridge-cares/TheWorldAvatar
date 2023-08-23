package uk.ac.cam.cares.jps.agent.dashboard.stack;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.List;
import java.util.Map;
import java.util.Queue;

/**
 * The public client for other classes to interface and interact with the knowledge graph and the stack to retrieve the necessary information.
 *
 * @author qhouyee
 */
public class StackClient {
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);
    private final String STACK_RDB_DOMAIN;
    private final String DASHBOARD_URL;
    private final PostGisClient POSTGIS_CLIENT;
    private final SparqlClient SPARQL_CLIENT;

    /**
     * Standard Constructor.
     */
    public StackClient() {
        LOGGER.debug("Attempting to retrieve services from the stack...");
        ContainerClient client = new ContainerClient();
        BlazegraphEndpointConfig blazeConfig = client.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class);
        PostGISEndpointConfig postConfig = client.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        LOGGER.debug("Retrieving PostGIS services...");
        this.STACK_RDB_DOMAIN = postConfig.getHostName() + ":" + postConfig.getPort();
        String stackJdbcUrl = "jdbc:postgresql://" + this.STACK_RDB_DOMAIN + "/";
        this.POSTGIS_CLIENT = new PostGisClient(stackJdbcUrl, postConfig.getUsername(), postConfig.getPassword());
        LOGGER.debug("Retrieving SPARQL services...");
        // Generate the generic stack SPARQL endpoint url based on their authentication enabled
        String stackSparqlEndpoint = blazeConfig.getPassword().isEmpty() ?
                // Non-authenticated endpoint
                "http://" + blazeConfig.getHostName() + ":" + blazeConfig.getPort() + "/blazegraph/" :
                // Authenticated endpoint
                "http://" + blazeConfig.getUsername() + ":" + blazeConfig.getPassword() + "@" + blazeConfig.getHostName() + ":" + blazeConfig.getPort() + "/blazegraph/";
        // Initialise a new Sparql client
        this.SPARQL_CLIENT = new SparqlClient(stackSparqlEndpoint, blazeConfig.getUsername(), blazeConfig.getPassword());
        // Note that the container name and port number is dependent on the custom setup - This may change when we have a built-in container for grafana
        this.DASHBOARD_URL = "http://" + getStackNameFromHost(blazeConfig.getHostName()) + "-grafana:3000";
        LOGGER.debug("Services have been successfully retrieved from the stack...");
    }

    /**
     * Get the stack name from the container's host name in the stack.
     *
     * @param hostName The container's host name.
     */
    private static String getStackNameFromHost(String hostName) {
        // Host names for stack's SPARQL endpoint are usually in the format: stackName-blazegraph
        // Code here retrieves stackName
        int stackIndex = hostName.lastIndexOf("-blazegraph");
        // If the host name does not exit, it probably is invalid
        if (stackIndex != -1) {
            return hostName.substring(0, stackIndex);
        }
        LOGGER.fatal("Invalid host name! Please ensure the stack container name is correct in: " + hostName);
        throw new JPSRuntimeException("Invalid host name! Please ensure the container name is correct in: " + hostName);
    }

    /**
     * Get all spatial zones within the knowledge graph.
     *
     * @return An array of all available spatial zones to monitor.
     */
    public String[] getAllSpatialZones() {
        return this.SPARQL_CLIENT.getAllSpatialZones();
    }

    /**
     * Get the list of database names that is available in this stack.
     */
    public List<String> getDatabaseNames() {
        return this.POSTGIS_CLIENT.getDatabaseNames();
    }

    /**
     * Get the PostGIS credentials for the dashboard.
     *
     * @return An array containing the credentials in sequence of domain name, username, and password.
     */
    public String[] getPostGisCredentials() {
        String[] credentials = new String[3];
        credentials[0] = this.STACK_RDB_DOMAIN;
        credentials[1] = this.POSTGIS_CLIENT.getUsername();
        credentials[2] = this.POSTGIS_CLIENT.getPassword();
        return credentials;
    }

    /**
     * Get the dashboard service within this stack.
     */
    public String getDashboardUrl() {
        return this.DASHBOARD_URL;
    }

    /**
     * Get all time series associated with assets from a specific spatial zone and rooms in the knowledge graph with groups of measures tied to group of asset types.
     *
     * @param spatialZone The spatial zone of interest.
     * @return A map: {assetType: {assets:[asset name list], measure[[measureDetails],[measureDetails]]}, room : {measure: [[measureDetails],[measureDetails]]}}.
     */
    public Map<String, Map<String, List<String[]>>> getAllTimeSeries(String spatialZone) {
        LOGGER.debug("Retrieving the time series metadata from PostGIS...");
        Map<String, Queue<String[]>> measures = this.SPARQL_CLIENT.getAllSpatialZoneMetaData(spatialZone);
        return this.POSTGIS_CLIENT.getMeasureColAndTableName(measures);
    }
}
