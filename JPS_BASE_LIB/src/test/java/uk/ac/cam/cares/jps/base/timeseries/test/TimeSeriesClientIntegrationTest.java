package uk.ac.cam.cares.jps.base.timeseries.test;

import java.io.IOException;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.*;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import org.testcontainers.containers.PostgreSQLContainer;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)")
@Testcontainers
public class TimeSeriesClientIntegrationTest {

	private static TimeSeriesClient<Instant> tsClient;

	// Will create two Docker containers for Blazegraph and postgreSQL
	// NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
	@Container
	// Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
	// For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker_Registry
	private static final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
			.withExposedPorts(9999);
	@Container
	// Create Docker container with latest postgres image from Docker Hub
	private static final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres");
	
	@Before
	public void initialiseTimeSeriesClient() {
		// Start Blazegraph container
		blazegraph.start();
		// Start postgreSQL container
		postgres.start();
		
		// Set endpoint to the triple store. The host and port are read from the container
		String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
		// Default namespace in blazegraph is "kb"
		endpoint = endpoint + "/blazegraph/namespace/kb/sparql";
		
		// Set up a kb client that points to the location of the triple store
		RemoteStoreClient kbClient = new RemoteStoreClient();		
		kbClient.setUpdateEndpoint(endpoint);
		kbClient.setQueryEndpoint(endpoint);
		
		// Initialise TimeSeriesClient client with pre-configured kb client
	    try {
	    	tsClient = new TimeSeriesClient(kbClient, Instant.class, "src/test/resources/timeseries.properties");
	    } catch (IOException e) {
	    	// Simply suppress exception for potential IO issues, as properties will be overwritten anyway
	    }
	    
	    // Configure database access
	    tsClient.setRDBClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
		
	}
	
	
}

