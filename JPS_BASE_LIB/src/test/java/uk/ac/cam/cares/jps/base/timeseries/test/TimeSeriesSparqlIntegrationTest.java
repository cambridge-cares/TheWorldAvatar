package uk.ac.cam.cares.jps.base.timeseries.test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.*;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

@Ignore("Requires triple store endpoint set up and running (using testcontainers)\n" + 
		"Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution")
@Testcontainers
public class TimeSeriesSparqlIntegrationTest {

	private static TimeSeriesSparql sparqlClient;

	// Will create a container that is shared between tests.
	// NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
	// For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker_Registry
	@Container
	private static final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
			.withExposedPorts(9999);

	@BeforeClass
	public static void initialiseSparqlClient() {
		// Start the container manually
		blazegraph.start();
		// Set up a kb client that points to the location of the triple store
		// This can be a RemoteStoreClient or the FileBasedStoreClient
		RemoteStoreClient kbClient = new RemoteStoreClient();
		// Set endpoint to the triple store. The host and port are read from the container
		String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
		// Default namespace in blazegraph is "kb", but in production a specific one should be created
		endpoint = endpoint + "/blazegraph/namespace/kb/sparql";
		kbClient.setUpdateEndpoint(endpoint);
		kbClient.setQueryEndpoint(endpoint);
		// Initialise TimeSeriesSparql client with kb client
		sparqlClient = new TimeSeriesSparql(kbClient);
	}
	
	@After
	public void clearKb() {
		// Clear entire knowledge base
		sparqlClient.removeAllTimeSeries();
	}
	
	@Test
	public void runIntegrationTest() {

		// IRIs for 2 times series: 1 with 3 associated data series and 1 with only 1 associated data series
		String tsIRI1 = "http://tsIRI1";
		List<String> dataIRI1 = Arrays.asList("http://data1", "http://data2", "http://data3");
		String tsIRI2 = "http://tsIRI2";
		List<String> dataIRI2 = Collections.singletonList("http://data4");
		String dbURL = "jdbc:postgresql:timeseries";
		String timeUnit = "s";

		// Initialise time series in kb
		sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);

		// Test number of initialised time series in kb
		Assert.assertEquals(1,  sparqlClient.countTS());
		// Test whether all data IRI have the time series attached
		for (String iri: dataIRI1) {
			Assert.assertEquals(tsIRI1,  sparqlClient.getTimeSeries(iri));
		}
		// Retrieve all data IRIs for the time series
		List<String> dataIRIs = sparqlClient.getAssociatedData(tsIRI1);
		for (String iri : dataIRI1) {
			Assert.assertTrue(dataIRIs.contains(iri));
		}

		// Retrieve time series properties
		Assert.assertEquals(dbURL,  sparqlClient.getDbUrl(tsIRI1));
		Assert.assertEquals(timeUnit,  sparqlClient.getTimeUnit(tsIRI1));

		// Initialise another time series without time unit
		sparqlClient.initTS(tsIRI2, dataIRI2, dbURL, null);
		Assert.assertNull(sparqlClient.getTimeUnit(tsIRI2));

		// Remove the attachment of one data IRI to a time series
		sparqlClient.removeTimeSeriesAssociation(dataIRI1.get(0));
		// Try to retrieve data IRI association
		Assert.assertNull(sparqlClient.getTimeSeries(dataIRI1.get(0)));

		// Remove entire time series
		sparqlClient.removeTimeSeries(sparqlClient.getTimeSeries(dataIRI1.get(1)));
		// Retrieve all time series remaining
		List<String> timeSeries = sparqlClient.getAllTimeSeries();
		Assert.assertEquals(1, timeSeries.size());
		Assert.assertTrue(timeSeries.contains(tsIRI2));

	}
}

