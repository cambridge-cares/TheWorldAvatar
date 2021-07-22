package uk.ac.cam.cares.jps.base.timeseries.test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.junit.Assert;
import org.junit.Test;
import org.junit.Ignore;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;


public class TimeSeriesSparqlTest {
	
	// Create mocks
    private ModifyQuery modify = Mockito.mock(ModifyQuery.class);
	private RemoteStoreClient mockClient = Mockito.mock(RemoteStoreClient.class);

    @Test
    public void testConstructor() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteStoreClient kbClient = new RemoteStoreClient();    
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient);
    	// Retrieve the value of the private field 'kbClient' of the client
        Field kbc = client.getClass().getDeclaredField("kbClient");
        kbc.setAccessible(true);
        RemoteStoreClient kbcl = (RemoteStoreClient) kbc.get(client);
        // Test whether kbClients are the same 
    	Assert.assertSame(kbcl, kbClient);
    }
    
    @Test
    public void testSetKbClient() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteStoreClient kbClient1 = new RemoteStoreClient();
    	RemoteStoreClient kbClient2 = new RemoteStoreClient();
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient1);
    	// Retrieve the value of the private field 'kbClient' of the client
        Field kbc = client.getClass().getDeclaredField("kbClient");
        kbc.setAccessible(true);
        // Test whether kbClients are the same 
    	Assert.assertSame(kbClient1, (RemoteStoreClient) kbc.get(client));
    	client.setKBClient(kbClient2);
    	Assert.assertNotSame(kbClient1, (RemoteStoreClient) kbc.get(client));
    	Assert.assertSame(kbClient2, (RemoteStoreClient) kbc.get(client));
    }
    
    @Test
    public void testNamespaces() {
        // Test the value of the public namespaces for the ontology and the knowledge base
        Assert.assertEquals("http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#", TimeSeriesSparql.ns_ontology);
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontotimeseries/", TimeSeriesSparql.ns_kb);        
    }
    
    @Test
    public void testPrefixes() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	// Retrieve the value of the private static field 'prefix_ontology' of the client
        Field p_onto = TimeSeriesSparql.class.getDeclaredField("prefix_ontology");
        p_onto.setAccessible(true);
        Prefix onto = (Prefix) p_onto.get(null);
        Assert.assertEquals("PREFIX ts: <http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#>", 
        					onto.getQueryString());
    	// Retrieve the value of the private static field 'prefix_kb' of the client
        Field p_kb = TimeSeriesSparql.class.getDeclaredField("prefix_kb");
        p_kb.setAccessible(true);
        Prefix kb = (Prefix) p_kb.get(null);
        Assert.assertEquals("PREFIX kb: <http://www.theworldavatar.com/kb/ontotimeseries/>", 
        					kb.getQueryString());      
    }
    
    @Test
    public void testIRIs() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	// Retrieve the value of the private static field 'TimeSeries' of the client
        Field timeseries = TimeSeriesSparql.class.getDeclaredField("TimeSeries");
        timeseries.setAccessible(true);
        Iri ts = (Iri) timeseries.get(null);
        Assert.assertEquals("ts:TimeSeries", ts.getQueryString());
    	// Retrieve the value of the private static field 'hasTimeSeries' of the client
        Field hasTimeSeries = TimeSeriesSparql.class.getDeclaredField("hasTimeSeries");
        hasTimeSeries.setAccessible(true);
        Iri has_ts = (Iri) hasTimeSeries.get(null);
        Assert.assertEquals("ts:hasTimeSeries", has_ts.getQueryString());
    	// Retrieve the value of the private static field 'hasRDB' of the client
        Field hasRDB = TimeSeriesSparql.class.getDeclaredField("hasRDB");
        hasRDB.setAccessible(true);
        Iri rdb = (Iri) hasRDB.get(null);
        Assert.assertEquals("ts:hasRDB", rdb.getQueryString());
    	// Retrieve the value of the private static field 'TimeSeries' of the client
        Field hasTimeUnit = TimeSeriesSparql.class.getDeclaredField("hasTimeUnit");
        hasTimeUnit.setAccessible(true);
        Iri unit = (Iri) hasTimeUnit.get(null);
        Assert.assertEquals("ts:hasTimeUnit", unit.getQueryString());
    }
    
    @Test
    public void testInitTSExceptions() {
    	RemoteStoreClient kbClient = new RemoteStoreClient();
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient);
    	// Test exception for incorrect tsIRI format
    	String[] iris = {"tsIRI", ":tsIRI", "/:tsIRI", "ns:#", "ns:/", "ns: "};
    	for (String iri : iris) {
        	try {
        		client.initTS(iri, null, null, null);
        	} catch (JPSRuntimeException e) {
            	Assert.assertEquals("TimeSeriesSparql: Time series IRI does not have valid IRI format", 
            						e.getMessage());
        	}    		
    	}
    	// Test that no exception is thrown for correct tsIRI
    	try (MockedStatic<Queries> query = Mockito.mockStatic(Queries.class)) {
    		// Mocks the behaviour of the Queries when new time series get initialised
        	query.when(() -> Queries.MODIFY()).thenThrow(new JPSRuntimeException("No exceptions occured"));
        	try {
        		client.initTS("http://tsIRI", null, null, null);
        	} catch (JPSRuntimeException e) {
            	Assert.assertEquals("No exceptions occured", e.getMessage());
        	}
    	}
    }
    
    @Test
    public void testInitTS() {
    	// Initialise test data
    	String tsIRI = "http://tsIRI1";
    	List<String> dataIRI = new ArrayList<>();
    	dataIRI.add("http://data1"); dataIRI.add("http://data2"); dataIRI.add("http://data3"); 
    	String dburl = "postgres";
    	String timeunit = "s";
    	
    	TimeSeriesSparql client = new TimeSeriesSparql(mockClient);
    	// Test whether modify query is called correct number of times (6 inserts for given sample data)
    	try (MockedStatic<Queries> query = Mockito.mockStatic(Queries.class)) {
    		// Mocks the behaviour of the Queries when new time series get initialised
    		query.when(() -> Queries.MODIFY()).thenReturn(modify);
    		Mockito.when(mockClient.executeUpdate(Mockito.anyString())).thenReturn(1);
    		client.initTS(tsIRI, dataIRI, dburl, timeunit);
    		//System.out.println(Mockito.mockingDetails(modify).printInvocations());
    		Mockito.verify(modify, Mockito.times(6)).insert(Mockito.any());
    	}
    }
}
