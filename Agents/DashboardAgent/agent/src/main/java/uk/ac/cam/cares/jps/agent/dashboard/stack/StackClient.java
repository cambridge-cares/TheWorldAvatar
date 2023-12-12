package uk.ac.cam.cares.jps.agent.dashboard.stack;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.grafana.GrafanaEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.Map;
import java.util.Queue;

/**
 * The public client for other classes to interface and interact with the knowledge graph and the stack to retrieve the necessary information.
 *
 * @author qhouyee
 */
public class StackClient {
    private final String stackRdbDomain;
    private final GrafanaEndpointConfig dashboardConfig;
    private final PostGisClient postgisClient;
    private final SparqlClient sparqlClient;
    private static final Logger LOGGER = LogManager.getLogger(StackClient.class);

    /**
     * Standard Constructor.
     */
    public StackClient() {
        LOGGER.debug("Attempting to retrieve services from the stack...");
        ContainerClient client = new ContainerClient();
        BlazegraphEndpointConfig blazeConfig = client.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class);
        PostGISEndpointConfig postConfig = client.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        this.dashboardConfig = client.readEndpointConfig("grafana", GrafanaEndpointConfig.class);
        LOGGER.debug("Retrieving PostGIS services...");
        this.stackRdbDomain = postConfig.getHostName() + ":" + postConfig.getPort();
        String stackJdbcUrl = "jdbc:postgresql://" + this.stackRdbDomain + "/";
        this.postgisClient = new PostGisClient(stackJdbcUrl, postConfig.getUsername(), postConfig.getPassword());
        LOGGER.debug("Retrieving SPARQL services...");
        // Generate the generic stack SPARQL endpoint url based on their authentication enabled
        String stackSparqlEndpoint = blazeConfig.getPassword().isEmpty() ?
                // Non-authenticated endpoint
                "http://" + blazeConfig.getHostName() + ":" + blazeConfig.getPort() + "/blazegraph/" :
                // Authenticated endpoint
                "http://" + blazeConfig.getUsername() + ":" + blazeConfig.getPassword() + "@" + blazeConfig.getHostName() + ":" + blazeConfig.getPort() + "/blazegraph/";
        // Initialise a new Sparql client
        this.sparqlClient = new SparqlClient(stackSparqlEndpoint, blazeConfig.getUsername(), blazeConfig.getPassword());
        LOGGER.debug("Services have been successfully retrieved from the stack...");
    }

    /**
     * Get all organisations who are managing spatial zones within the knowledge graph.
     *
     * @return An array of all available organisations and their associated spatial zones to monitor.
     */
    public String[] getAllOrganisations() {
        return this.sparqlClient.getAllOrganisations();
    }

    /**
     * Get the list of database names that is available in this stack.
     */
    public List<String> getDatabaseNames() {
        return this.postgisClient.getDatabaseNames();
    }

    /**
     * Get the RDB domain url within the stack.
     */
    public String getRdbDomain() {
        return this.stackRdbDomain;
    }

    /**
     * Get the username to access the RDB within the stack.
     */
    public String getRdbUser() {
        return this.postgisClient.getUsername();
    }

    /**
     * Get the username to access the RDB within the stack.
     */
    public String getRdbPassword() {
        return this.postgisClient.getPassword();
    }

    /**
     * Get the dashboard service within this stack.
     */
    public String getDashboardUrl() {
        return this.dashboardConfig.getServiceUrl();
    }

    /**
     * Get the dashboard username credentials within this stack.
     */
    public String getDashboardUser() { return this.dashboardConfig.getUsername(); }

    /**
     * Get the dashboard password credential within this stack.
     */
    public String getDashboardPassword() { return this.dashboardConfig.getPassword(); }

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
     * },
     * systems:{
     * systems: [System1, Subsystem2],
     * measure1: [[System1, ColName1, TableName2, Database, unit],[Subsystem2, ColName3, TableName2, Database, unit]],
     * measure2: [[System1, ColName2, TableName2, Database, unit],[Subsystem2, ColName4, TableName2, Database, unit]]
     * }
     * }
     *
     * @param organisation The organisation of interest.
     * @return A map: {assetType: {assets:[asset name list], measure[[measureDetails],[measureDetails]]}, room : {measure: [[measureDetails],[measureDetails]]}}.
     */
    public Map<String, Map<String, List<String[]>>> getAllTimeSeries(String organisation) {
        LOGGER.debug("Retrieving the spatial zone metadata for organisation: {}...", organisation);
        Map<String, Queue<String[]>> measures = this.sparqlClient.getAllSpatialZoneMetaData(organisation);
        LOGGER.debug("Retrieving the time series metadata from PostGIS...");
        return this.postgisClient.getMeasureColAndTableName(measures);
    }
}
