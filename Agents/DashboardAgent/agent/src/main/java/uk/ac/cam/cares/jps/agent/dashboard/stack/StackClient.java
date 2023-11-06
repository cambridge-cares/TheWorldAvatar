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
     * Get all organisations who are managing spatial zones within the knowledge graph.
     *
     * @return An array of all available organisations and their associated spatial zones to monitor.
     */
    public String[] getAllOrganisations() {
        return this.SPARQL_CLIENT.getAllOrganisations();
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
     * Get all time series associated with the spatial zones managed by an organisation, namely their assets and rooms' measures in the knowledge graph.
     * The measure groups are tied to group of asset types. The final format is as follows:
     * { facilities:{
     * facility1: [[RoomName1], [AssetName1], [AssetName2], [AssetName6]],
     * facility2: [[RoomName2], [AssetName3], [AssetName4], [AssetName5], [AssetName7]],
     * }, assetType1: {
     * assets: [AssetName1, AssetName2, AssetName3],
     * measure1: [[AssetName1, ColName1, TableName1, Database, unit],[AssetName2, ColName2, TableName1, Database, unit],[AssetName3, ColName3, TableName1, Database, unit]],
     * measure2: [[AssetName1, ColName5, TableName1, Database, unit],[AssetName2, ColName6, TableName1, Database, unit],[AssetName3, ColName7, TableName1, Database, unit]],
     * },
     * assetType2: {
     * assets: [AssetName5, AssetName6, AssetName7],
     * measure1: [[AssetName5, ColName1, TableName1, Database, unit],[AssetName6, ColName2, TableName1, Database, unit],[AssetName7, ColName3, TableName1, Database, unit]],
     * measure2: [[AssetName5, ColName5, TableName1, Database, unit],[AssetName6, ColName6, TableName1, Database, unit],[AssetName7, ColName7, TableName1, Database, unit]],
     * },
     * Rooms:{
     * thresholds: [[measure1, minThreshold, maxThreshold],[measure2, minThreshold, maxThreshold]],
     * Rooms: [RoomName1, RoomName2],
     * measure1: [[RoomName1, ColName1, TableName2, Database, unit],[RoomName2, ColName3, TableName2, Database, unit]],
     * measure2: [[RoomName1, ColName2, TableName2, Database, unit],[RoomName2, ColName4, TableName2, Database, unit]]
     * }
     * }
     *
     * @param organisation The organisation of interest.
     * @return A map: {assetType: {assets:[asset name list], measure[[measureDetails],[measureDetails]]}, room : {measure: [[measureDetails],[measureDetails]]}}.
     */
    public Map<String, Map<String, List<String[]>>> getAllTimeSeries(String organisation) {
        LOGGER.debug("Retrieving the spatial zone metadata for organisation: " + organisation + "...");
        Map<String, Queue<String[]>> measures = this.SPARQL_CLIENT.getAllSpatialZoneMetaData(organisation);
        LOGGER.debug("Retrieving the time series metadata from PostGIS...");
        return this.POSTGIS_CLIENT.getMeasureColAndTableName(measures);
    }
}
