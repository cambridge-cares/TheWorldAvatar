package uk.ac.cam.cares.jps.base.timeseries.test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.*;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

@Ignore("Requires triple store endpoint set up and running")
public class TimeSeriesSparqlIntegrationTest {

	private static TimeSeriesSparql sparqlClient;

	@BeforeClass
	public static void initialiseSparqlClient() {
		// Set up a kb client that points to the location of the triple store
		// This can be a RemoteStoreClient or the FileBasedStoreClient
		RemoteStoreClient kbClient = new RemoteStoreClient();
		// Set path to local Triple Store instance - when working with Blazegraph:
		// Port needs to be adjusted depending on local environment settings
		// Namespace "timeseries" needs to be created beforehand
		String endpoint = "http://localhost:9999/blazegraph/namespace/timeseries/sparql";
		kbClient.setUpdateEndpoint(endpoint);
		kbClient.setQueryEndpoint(endpoint);
		// Initialise TimeSeriesSparql client with kb client
		sparqlClient = new TimeSeriesSparql(kbClient);
	}
	
	@AfterClass
	public static void clearKb() {
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

