package uk.ac.cam.cares.jps.agent.dashboard.stack;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.grafana.GrafanaEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;

import java.util.List;

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
     * Get all organisations who are managing facilities within the knowledge graph.
     *
     * @return An array of all available time series data model for each organisation.
     */
    public List<Organisation> getAllOrganisations() {
        List<Organisation> orgList = this.sparqlClient.getAllOrganisations();
        for (Organisation organisation : orgList) {
            this.postgisClient.retrieveMeasureRDBLocation(organisation);
        }
        return orgList;
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
}
