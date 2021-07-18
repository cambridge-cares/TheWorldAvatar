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
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;


public class TimeSeriesSparqlTest {
	
	// Create mocks
    private ModifyQuery modify = Mockito.mock(ModifyQuery.class);
	private RemoteKnowledgeBaseClient mockClient = Mockito.mock(RemoteKnowledgeBaseClient.class);

    @Test
    public void testConstructor() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();    
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient);
    	// Retrieve the value of the private field 'kbClient' of the client
        Field kbc = client.getClass().getDeclaredField("kbClient");
        kbc.setAccessible(true);
        RemoteKnowledgeBaseClient kbcl = (RemoteKnowledgeBaseClient) kbc.get(client);
        // Test whether kbClients are the same 
    	Assert.assertSame(kbcl, kbClient);
    }
    
    @Test
    public void testSetKbClient() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteKnowledgeBaseClient kbClient1 = new RemoteKnowledgeBaseClient();
    	RemoteKnowledgeBaseClient kbClient2 = new RemoteKnowledgeBaseClient();
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient1);
    	// Retrieve the value of the private field 'kbClient' of the client
        Field kbc = client.getClass().getDeclaredField("kbClient");
        kbc.setAccessible(true);
        // Test whether kbClients are the same 
    	Assert.assertSame(kbClient1, (RemoteKnowledgeBaseClient) kbc.get(client));
    	client.setKBClient(kbClient2);
    	Assert.assertNotSame(kbClient1, (RemoteKnowledgeBaseClient) kbc.get(client));
    	Assert.assertSame(kbClient2, (RemoteKnowledgeBaseClient) kbc.get(client));
    }
    
    @Test
    public void testNamespaces() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient);
    	// Retrieve the value of the private field 'ns_ontology' of the client
        Field ns_onto = client.getClass().getDeclaredField("ns_ontology");
        ns_onto.setAccessible(true);
        String onto = (String) ns_onto.get(client);
        Assert.assertEquals("http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#", onto);
    	// Retrieve the value of the private field 'ns_kb' of the client
        Field ns_kb = client.getClass().getDeclaredField("ns_kb");
        ns_kb.setAccessible(true);
        String kb = (String) ns_kb.get(client);
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontotimeseries/", kb);        
    }
    
    @Test
    public void testPrefixes() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient);
    	// Retrieve the value of the private field 'prefix_ontology' of the client
        Field p_onto = client.getClass().getDeclaredField("prefix_ontology");
        p_onto.setAccessible(true);
        Prefix onto = (Prefix) p_onto.get(client);
        Assert.assertEquals("PREFIX ts: <http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#>", 
        					onto.getQueryString());
    	// Retrieve the value of the private field 'prefix_kb' of the client
        Field p_kb = client.getClass().getDeclaredField("prefix_kb");
        p_kb.setAccessible(true);
        Prefix kb = (Prefix) p_kb.get(client);
        Assert.assertEquals("PREFIX kb: <http://www.theworldavatar.com/kb/ontotimeseries/>", 
        					kb.getQueryString());      
    }
    
    @Test
    public void testIRIs() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient);
    	// Retrieve the value of the private field 'TimeSeries' of the client
        Field timeseries = client.getClass().getDeclaredField("TimeSeries");
        timeseries.setAccessible(true);
        Iri ts = (Iri) timeseries.get(client);
        Assert.assertEquals("ts:TimeSeries", ts.getQueryString());
    	// Retrieve the value of the private field 'hasTimeSeries' of the client
        Field hasTimeSeries = client.getClass().getDeclaredField("hasTimeSeries");
        hasTimeSeries.setAccessible(true);
        Iri has_ts = (Iri) hasTimeSeries.get(client);
        Assert.assertEquals("ts:hasTimeSeries", has_ts.getQueryString());
    	// Retrieve the value of the private field 'hasRDB' of the client
        Field hasRDB = client.getClass().getDeclaredField("hasRDB");
        hasRDB.setAccessible(true);
        Iri rdb = (Iri) hasRDB.get(client);
        Assert.assertEquals("ts:hasRDB", rdb.getQueryString());
    	// Retrieve the value of the private field 'TimeSeries' of the client
        Field hasTimeUnit = client.getClass().getDeclaredField("hasTimeUnit");
        hasTimeUnit.setAccessible(true);
        Iri unit = (Iri) hasTimeUnit.get(client);
        Assert.assertEquals("ts:hasTimeUnit", unit.getQueryString());
    }
    
    @Test
    public void testInitTSExceptions() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient);
    	// Test exception when neither tsIRI nor uuid is provided
    	try {
    		client.initTS(null, null, null, null, null);
    	} catch (JPSRuntimeException e) {
        	Assert.assertEquals("TimeSeriesSparql: Either the timeseries IRI OR the uuid shall be provided", 
        						e.getMessage());
    	}
    	// Test exception for incorrect tsIRI format
    	try {
    		client.initTS("tsIRI", null, null, null, null);
    	} catch (JPSRuntimeException e) {
        	Assert.assertEquals("TimeSeriesSparql: Time series IRI needs to start with http://", 
        						e.getMessage());
    	}
    	// Test that no exception is thrown for correct tsIRI
    	try (MockedStatic<Queries> query = Mockito.mockStatic(Queries.class)) {
    		// Mocks the behaviour of the Queries when new time series get initialised
        	query.when(() -> Queries.MODIFY()).thenThrow(new JPSRuntimeException("No exceptions occured"));
        	try {
        		client.initTS("http://tsIRI", null, null, null, null);
        	} catch (JPSRuntimeException e) {
            	Assert.assertEquals("No exceptions occured", e.getMessage());
        	}
    	}
    }
    
    @Test
    public void testInitTS() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
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
    		client.initTS(tsIRI, null, dataIRI, dburl, timeunit);
    		//System.out.println(Mockito.mockingDetails(modify).printInvocations());
    		Mockito.verify(modify, Mockito.times(6)).insert(Mockito.any());
    	}
    }
}
