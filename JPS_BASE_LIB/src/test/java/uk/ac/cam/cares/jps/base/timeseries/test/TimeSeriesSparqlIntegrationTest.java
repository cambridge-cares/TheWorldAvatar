package uk.ac.cam.cares.jps.base.timeseries.test;

import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.Ignore;
import org.junit.Before;
import org.junit.After;

import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

@Ignore("Requires Triple store end point set up")
public class TimeSeriesSparqlIntegrationTest {
	
	// Set path to local Triple Store instance - when working with Blazegraph:
	// Port needs to be adjusted depending on local environment settings
	// Namespace "timeseries" needs to be created beforehand
	private static String endpoint = "http://localhost:9999/blazegraph/namespace/timeseries/sparql";
	
	// Initialise 2 times series: 1 with 3 associated data series and 1 with only 1 associated data series
	private static String tsIRI1 = "http://tsIRI1";
	private static List<String> dataIRI1 = Arrays.asList("http://data1", "http://data2", "http://data3");
	private static String tsIRI2 = "http://tsIRI2";
	private static List<String> dataIRI2 = Arrays.asList("http://data4");
	private static String dbURL = "jdbc:postgresql:timeseries"; 
	private static String timeUnit = "http://s";
	
	public static TimeSeriesSparql initialiseSparqlClient() {
		// Set up a kb client that points to the location of your instance
		// This can be a RemoteKnowledgeBaseClient or the FileBasedKnowledgeBaseClient
		RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
		kbClient.setUpdateEndpoint(endpoint);
		kbClient.setQueryEndpoint(endpoint);
		// Initialise TimeSeriesSparql client with kb client
		TimeSeriesSparql SparqlClient = new TimeSeriesSparql(kbClient);
			
		return SparqlClient;
	}
	
	@After
	public void clearKb() {
		TimeSeriesSparql SparqlClient = initialiseSparqlClient();
		// Clear entire knowledge base
		SparqlClient.removeAllTimeSeries();
	}
	
	@Test
	public void testInitTS() {
		TimeSeriesSparql SparqlClient = initialiseSparqlClient();
		// Initialise time series in kb
		SparqlClient.initTS(tsIRI1, null, dataIRI1, dbURL, timeUnit);
		// Test number of initialised time series in kb
		Assert.assertEquals(1,  SparqlClient.countTS());
		// Test for time series IRI
		Assert.assertEquals("http://tsIRI1",  SparqlClient.getTimeSeries("http://data1"));
		Assert.assertEquals("http://tsIRI1",  SparqlClient.getAllTimeSeries().get(0));
		// Test for correct dataIRIs
		List<String> data = SparqlClient.getAssociatedData("http://tsIRI1");
		for (String s : data) {
			Assert.assertTrue(dataIRI1.contains(s));
		}
		// Test for RDB URL
		Assert.assertEquals("jdbc:postgresql:timeseries",  SparqlClient.getDbUrl("http://tsIRI1"));
		Assert.assertNull(SparqlClient.getDbUrl("http://tsIRI3"));
		// Test for time unit
		Assert.assertEquals("http://s",  SparqlClient.getTimeUnit("http://tsIRI1"));
		Assert.assertNull(SparqlClient.getTimeUnit("http://tsIRI3"));
	}
	
	@Test
	public void testCheckTimeSeriesExists() {
		TimeSeriesSparql SparqlClient = initialiseSparqlClient();
		// Initialise time series in kb
		SparqlClient.initTS(tsIRI1, null, dataIRI1, dbURL, timeUnit);
		Assert.assertTrue(SparqlClient.checkTimeSeriesExists(tsIRI1));
		Assert.assertFalse(SparqlClient.checkTimeSeriesExists(tsIRI2));
	}
	
	@Test
	public void testCheckDataExists() {
		TimeSeriesSparql SparqlClient = initialiseSparqlClient();
		// Initialise time series in kb
		SparqlClient.initTS(tsIRI1, null, dataIRI1, dbURL, timeUnit);
		Assert.assertTrue(SparqlClient.checkDataExists("http://data1"));
		Assert.assertFalse(SparqlClient.checkDataExists("http://data5"));
	}
	
	@Test
	public void testcheckTimeUnitExists() {
		TimeSeriesSparql SparqlClient = initialiseSparqlClient();
		// Initialise time series in kb
		SparqlClient.initTS(tsIRI1, null, dataIRI1, dbURL, null);
		Assert.assertFalse(SparqlClient.checkTimeUnitExists(tsIRI1));
		SparqlClient.initTS(tsIRI1, null, dataIRI1, dbURL, timeUnit);
		Assert.assertTrue(SparqlClient.checkTimeUnitExists(tsIRI1));
	}
	
	@Test
	public void testRemoveTimeSeries() {
		TimeSeriesSparql SparqlClient = initialiseSparqlClient();
		// Initialise time series in kb
		SparqlClient.initTS(tsIRI1, null, dataIRI1, dbURL, timeUnit);
		Assert.assertEquals(1, SparqlClient.countTS());
		SparqlClient.initTS(tsIRI2, null, dataIRI2, dbURL, timeUnit);
		Assert.assertEquals(2, SparqlClient.countTS());
		SparqlClient.removeTimeSeries(tsIRI2);
		Assert.assertEquals(1, SparqlClient.countTS());
		// Test whether deletion of non-instantiated tsIRIs is handled
		SparqlClient.removeTimeSeries(tsIRI2);
		Assert.assertEquals(1, SparqlClient.countTS());
		
		SparqlClient.removeTimeSeries(tsIRI1);
		Assert.assertEquals(0, SparqlClient.countTS());
	}
	
	@Test
	public void testRemoveAllTimeseries() {
		TimeSeriesSparql SparqlClient = initialiseSparqlClient();
		// Initialise time series in kb
		SparqlClient.initTS(tsIRI1, null, dataIRI1, dbURL, timeUnit);
		SparqlClient.initTS(tsIRI2, null, dataIRI2, dbURL, timeUnit);
		Assert.assertEquals(2, SparqlClient.countTS());
		SparqlClient.removeAllTimeSeries();
		Assert.assertEquals(0, SparqlClient.countTS());
	}
	
	@Test
	public void testRemoveTimeSeriesAssociation() {
		TimeSeriesSparql SparqlClient = initialiseSparqlClient();
		// Initialise time series in kb
		SparqlClient.initTS(tsIRI1, null, dataIRI1, dbURL, timeUnit);		
		Assert.assertEquals(3, SparqlClient.getAssociatedData(tsIRI1).size());
		SparqlClient.removeTimeSeriesAssociation("http://data1");		
		Assert.assertEquals(2, SparqlClient.getAssociatedData(tsIRI1).size());
		SparqlClient.removeTimeSeriesAssociation("http://data2");		
		Assert.assertEquals(1, SparqlClient.getAssociatedData(tsIRI1).size());
		SparqlClient.removeTimeSeriesAssociation("http://data3");		
		Assert.assertEquals(0, SparqlClient.getAssociatedData(tsIRI1).size());
		Assert.assertEquals(0, SparqlClient.countTS());
	}
	
	
}

