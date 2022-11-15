package uk.ac.cam.cares.jps.base.timeseries;

import java.io.*;
import java.nio.file.Paths;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.*;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateRequest;
import org.apache.jena.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.junit.*;
import org.junit.rules.TemporaryFolder;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This class provides unit tests for the TimeSeriesSparql class
 */

public class TimeSeriesSparqlTest {
	
    @ClassRule
    // Temporary folder to place a properties file (same file for all potential tests)
    public static TemporaryFolder folder = new TemporaryFolder();

    private MockKnowledgeBaseClient mockClient;
    private TimeSeriesSparql sparqlClient;
    
	// Initialise correct namespaces to use for ontology and knowledge base
	private final String ns_ontology = "https://www.theworldavatar.com/kg/ontotimeseries/";
	private final String ns_kb = "https://www.theworldavatar.com/kg/ontotimeseries/";

    // Initialise IRIs for 2 times series: 1 with 3 associated data series and 1 with only 1 associated data series
    private final String tsIRI1 = "http://tsIRI1";
    private final List<String> dataIRI1 = Arrays.asList("http://data1", "http://data2", "http://data3");
    private final String tsIRI2 = "http://tsIRI2";
    private final List<String> dataIRI2 = Collections.singletonList("http://data4");
    private final String dbURL = "jdbc:postgresql:timeseries";
    private final String timeUnit = "http://s";

	// Class that is used as the knowledge base client in the tests.
    // It will use a jena model to execute queries on.
	private static class MockKnowledgeBaseClient extends RemoteStoreClient {

	    private final OntModel kb;
        private String query;

        MockKnowledgeBaseClient(OntModel kb) { this.kb = kb;}

        public OntModel getKnowledgeBase() {
            return kb;
        }

        public void closeKnowledgeBase() {
            if (!(kb==null)) {
                kb.close();
            }
        }

        @Override
        public String get(String graphName, String accept) {
            return null;
        }

        @Override
        public void insert(String graphName, String content, String contentType) {
        }

        @Override
        public JSONArray executeQuery(String query) {
            return new JSONArray(execute(query));
        }

        @Override
        public JSONArray executeQuery() {
            return executeQuery(query);
        }

        @Override
        public String execute() {
            return execute(query);
        }

        @Override
        public String execute(String sparql) {
            Query query = QueryFactory.create(sparql);
            QueryExecution queryExec = QueryExecutionFactory.create(query, kb);
            if (sparql.toLowerCase().contains("ask")) {
                boolean askResult = queryExec.execAsk();
                return new JSONArray("[{'ASK': "+ askResult + "}]").toString();
            }
            else {
                ResultSet rs = queryExec.execSelect();
                return JenaResultSetFormatter.convertToSimplifiedList(rs).getJSONArray("results").toString();
            }

        }

        @Override
        public Model executeConstruct(Query sparql) {
            return null;
        }

        @Override
        public Model executeConstruct(String sparql) {
            return null;
        }

        @Override
        public int executeUpdate() {
            return executeUpdate(query);
        }

        @Override
        public int executeUpdate(String update) {
            UpdateAction.parseExecute(update, kb);
            return 0;
        }

        @Override
        public int executeUpdate(UpdateRequest update) {
            return executeUpdate(update.toString());
        }

        @Override
        public String setQuery(String query) {
            this.query = query;
            return this.query;
        }

        @Override
        public String getQuery() {
            return query;
        }

        @Override
        public String getQueryEndpoint() {
            return null;
        }

        @Override
        public String setQueryEndpoint(String queryEndpoint) {
            return null;
        }

        @Override
        public String getUpdateEndpoint() {
            return null;
        }

        @Override
        public String setUpdateEndpoint(String updateEndpoint) {
            return null;
        }

        @Override
        public String getUser() {
            return null;
        }

        @Override
        public void setUser(String userName) {
        }

        @Override
        public String getPassword() {
            return null;
        }

        @Override
        public void setPassword(String password) {
        }
    }

    @Before
    public void initialiseSparqlClient() {
        OntModel kb = ModelFactory.createOntologyModel();
        mockClient = new MockKnowledgeBaseClient(kb);
        sparqlClient = new TimeSeriesSparql(mockClient);
    }

    @After
    public void closeKnowledgeBase() {
	    mockClient.closeKnowledgeBase();
    }

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
    	Assert.assertSame(kbClient1, kbc.get(client));
    	client.setKBClient(kbClient2);
    	Assert.assertNotSame(kbClient1, kbc.get(client));
    	Assert.assertSame(kbClient2, kbc.get(client));
    }
    
    @Test
    public void testNamespaces() {
        // Test the value of the public namespaces for the ontology and the knowledge base
        Assert.assertEquals(ns_ontology, TimeSeriesSparql.ns_ontology);
        Assert.assertEquals(ns_kb, TimeSeriesSparql.ns_kb);        
    }
    
    @Test
    public void testPrefixes() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	// Retrieve the value of the private static field 'prefix_ontology' of the client
        Field p_onto = TimeSeriesSparql.class.getDeclaredField("prefix_ontology");
        p_onto.setAccessible(true);
        Prefix onto = (Prefix) p_onto.get(null);
        Assert.assertEquals("PREFIX ts: <" + ns_ontology + ">", 
        					onto.getQueryString());
    	// Retrieve the value of the private static field 'prefix_kb' of the client
        Field p_kb = TimeSeriesSparql.class.getDeclaredField("prefix_kb");
        p_kb.setAccessible(true);
        Prefix kb = (Prefix) p_kb.get(null);
        Assert.assertEquals("PREFIX kb: <" + ns_kb + ">", 
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
    public void testInitTS() {
        TimeSeriesSparql sparqlClient = new TimeSeriesSparql(mockClient);
        // Initialise time series in knowledge base
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);
        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Test that the IRIs are correctly set as individuals
        Assert.assertNotNull(testKnowledgeBase.getIndividual(tsIRI1));
        for (String iri: dataIRI1) {
            Assert.assertNotNull(testKnowledgeBase.getIndividual(iri));
        }
        // Test timeseries instance
        Assert.assertEquals(TimeSeriesSparql.ns_ontology + "TimeSeries",
                testKnowledgeBase.getIndividual(tsIRI1).getRDFType().getURI());
        RDFNode object = testKnowledgeBase.getIndividual(tsIRI1)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasRDB"))
                .getObject();
        Assert.assertTrue(object.isLiteral());
        Assert.assertEquals(dbURL, object.asLiteral().getString());
        object = testKnowledgeBase.getIndividual(tsIRI1)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeUnit"))
                .getObject();
        Assert.assertTrue(object.isLiteral());
        Assert.assertEquals(timeUnit, object.asLiteral().getString());

        // Test that data IRIs are attached to time series instance
        for (String iri: dataIRI1) {
            object = testKnowledgeBase.getIndividual(iri)
                    .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI1, object.asResource().getURI());
        }

        // Trying to init same time series should result in an exception
        Exception exception = Assert.assertThrows(JPSRuntimeException.class, () ->
                sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit));
        String errorMessage = exception.getMessage();
        Assert.assertTrue(errorMessage.contains(tsIRI1));
        Assert.assertTrue(errorMessage.contains("already in the Knowledge Graph"));
        Assert.assertTrue(errorMessage.contains(TimeSeriesSparql.class.getSimpleName()));
        // Trying to init different time series but same data IRI should result in an exception
        exception = Assert.assertThrows(JPSRuntimeException.class, () ->
                sparqlClient.initTS(tsIRI2, dataIRI1, dbURL, timeUnit));
        errorMessage = exception.getMessage();
        Assert.assertTrue(errorMessage.contains(tsIRI1));
        Assert.assertTrue(errorMessage.contains(dataIRI1.get(0)));
        Assert.assertTrue(errorMessage.contains("is already attached to time series"));
        Assert.assertTrue(errorMessage.contains(TimeSeriesSparql.class.getSimpleName()));
	}

    @Test
    public void testCheckTimeSeriesExists() {
        Assert.assertFalse(sparqlClient.checkTimeSeriesExists(tsIRI1));

        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);

        Assert.assertTrue(sparqlClient.checkTimeSeriesExists(tsIRI1));
        Assert.assertFalse(sparqlClient.checkTimeSeriesExists(tsIRI2));
    }

    @Test
    public void testCheckDataExists() {
        Assert.assertFalse(sparqlClient.checkDataHasTimeSeries("http://data1"));

        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);

        Assert.assertTrue(sparqlClient.checkDataHasTimeSeries("http://data1"));
        Assert.assertFalse(sparqlClient.checkDataHasTimeSeries("http://data5"));

        // Retrieve test knowledge base
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();
        // Add a data IRI with a property that is not attached to a time series
        testKnowledgeBase.add(testKnowledgeBase.createResource(dataIRI2.get(0)), RDFS.comment, "Data IRI without timeseries");
        Assert.assertFalse(sparqlClient.checkDataHasTimeSeries(dataIRI2.get(0)));

    }

    @Test
    public void testCountTS() {
        Assert.assertEquals(0, sparqlClient.countTS());

        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, null);
        Assert.assertEquals(1, sparqlClient.countTS());
        // Initialise different time series in kb
        sparqlClient.initTS(tsIRI2, dataIRI2, dbURL, timeUnit);
        Assert.assertEquals(2, sparqlClient.countTS());
    }

    @Test
    public void testGetTimeSeries() {
        Assert.assertNull(sparqlClient.getTimeSeries(dataIRI1.get(0)));
        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, null);
        Assert.assertNull(sparqlClient.getTimeSeries(dataIRI2.get(0)));
        for(String iri: dataIRI1) {
            Assert.assertEquals(tsIRI1, sparqlClient.getTimeSeries(iri));
        }

        // Retrieve test knowledge base
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();
        // Add a data IRI with a property
        testKnowledgeBase.add(testKnowledgeBase.createResource(dataIRI2.get(0)), RDFS.comment, "Data IRI without timeseries");
        Assert.assertNull(sparqlClient.getTimeSeries(dataIRI2.get(0)));
    }

    @Test
    public void testGetDbUrl() {
        Assert.assertNull(sparqlClient.getDbUrl(tsIRI1));
        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, null);
        Assert.assertNull(sparqlClient.getDbUrl(tsIRI2));
        Assert.assertEquals(dbURL, sparqlClient.getDbUrl(tsIRI1));
        // Initialise different time series in kb
        sparqlClient.initTS(tsIRI2, dataIRI2, dbURL + "_2", null);
        Assert.assertEquals(dbURL, sparqlClient.getDbUrl(tsIRI1));
        Assert.assertEquals(dbURL + "_2", sparqlClient.getDbUrl(tsIRI2));
    }

    @Test
    public void testGetTimeUnit() {
        Assert.assertNull(sparqlClient.getTimeUnit(tsIRI1));
        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);
        Assert.assertNull(sparqlClient.getTimeUnit(tsIRI2));
        Assert.assertEquals(timeUnit, sparqlClient.getTimeUnit(tsIRI1));
        // Initialise different time series in kb without time series
        sparqlClient.initTS(tsIRI2, dataIRI2, dbURL, null);
        Assert.assertEquals(timeUnit, sparqlClient.getTimeUnit(tsIRI1));
        Assert.assertNull(sparqlClient.getTimeUnit(tsIRI2));
    }

    @Test
    public void testGetAssociatedData() {
        Assert.assertEquals(0, sparqlClient.getAssociatedData(tsIRI1).size());
	    // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);

        Assert.assertEquals(0, sparqlClient.getAssociatedData(tsIRI2).size());
        List<String> retrievedDataIRI = sparqlClient.getAssociatedData(tsIRI1);
        Assert.assertEquals(3, sparqlClient.getAssociatedData(tsIRI1).size());
        // Check that data IRIs were retrieved correctly
        for (String iri: dataIRI1) {
            Assert.assertTrue(retrievedDataIRI.contains(iri));
        }
	}

    @Test
    public void testGetAllTimeSeries() {
        Assert.assertEquals(0, sparqlClient.getAllTimeSeries().size());
        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);
        Assert.assertEquals(1, sparqlClient.getAllTimeSeries().size());
        // Initialise different time series in kb
        sparqlClient.initTS(tsIRI2, dataIRI2, dbURL, timeUnit);
        List<String> retrievedTimeSeries = sparqlClient.getAllTimeSeries();
        Assert.assertEquals(2, retrievedTimeSeries.size());
        for (String iri: Arrays.asList(tsIRI1, tsIRI2)) {
            Assert.assertTrue(retrievedTimeSeries.contains(iri));
        }
    }

    @Test
    public void testInsertTimeSeriesAssociation() {

	    String dataIRIThatGetsAdded = "http://data4";

        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);
        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Test that data IRIs are attached to time series instance
        for (String iri: dataIRI1) {
            RDFNode object = testKnowledgeBase.getIndividual(iri)
                    .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI1, object.asResource().getURI());
        }
        sparqlClient.insertTimeSeriesAssociation(dataIRIThatGetsAdded, tsIRI1);
        // Test that new attached data IRI is really attached to time series instance
        RDFNode object = testKnowledgeBase.getIndividual(dataIRIThatGetsAdded)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries"))
                .getObject();
        Assert.assertTrue(object.isResource());
        Assert.assertEquals(tsIRI1, object.asResource().getURI());
        
        // Check exception for already existing time series association (with any time series)
        try {
        	sparqlClient.insertTimeSeriesAssociation(dataIRIThatGetsAdded, tsIRI1);
        	Assert.fail();
        } catch (Exception e) {
        	Assert.assertTrue(e.getMessage().contains("is already attached to time series"));
        }
        
        // Check exception for non-existing time series IRI
        try {
        	sparqlClient.insertTimeSeriesAssociation("http://data5", "http://tsIRI3");
        	Assert.fail();
        } catch (Exception e) {
        	Assert.assertTrue(e.getMessage().contains("does not exists in the Knowledge Graph"));
            // Test that new attached data IRI is still attached to time series instance
            object = testKnowledgeBase.getIndividual(dataIRIThatGetsAdded)
                    .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI1, object.asResource().getURI());
        }
    }
    
    @Test
    public void testRemoveTimeSeriesAssociation() {

	    String dataIRIThatGetsRemoved = "http://data1";

        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);
        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Add different property to data IRI for which the connection should be removed to ensure it stays in the KG
        testKnowledgeBase.getIndividual(dataIRIThatGetsRemoved).
                addComment(testKnowledgeBase.createLiteral("This data IRI should not be connected to a time series"));

        // Remove association to one of the data IRIs
        sparqlClient.removeTimeSeriesAssociation(dataIRIThatGetsRemoved);
        // Try to remove association of data IRI without connection (should not have any effect)
        sparqlClient.removeTimeSeriesAssociation(dataIRI2.get(0));

        // Check that time series is still in the Knowledge Graph
        Assert.assertNotNull(testKnowledgeBase.getIndividual(tsIRI1));
        // Check that the remaining data IRIs are still attached to the time series
        for (String iri: dataIRI1) {
            if (!iri.equals(dataIRIThatGetsRemoved)) {
                RDFNode object = testKnowledgeBase.getIndividual(iri)
                        .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries"))
                        .getObject();
                Assert.assertTrue(object.isResource());
                Assert.assertEquals(tsIRI1, object.asResource().getURI());
            }
        }
        // Check that the data for which the connection was removed does exist but the connection was removed
        Assert.assertNotNull(testKnowledgeBase.getIndividual(dataIRIThatGetsRemoved));
        Assert.assertNull(testKnowledgeBase.getIndividual(dataIRIThatGetsRemoved)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries")));

        // Initialise other time series with only one data IRI in kb
        sparqlClient.initTS(tsIRI2, dataIRI2, dbURL, timeUnit);
        // Remove association to the only attached data IRI, which will remove the whole time series instance
        sparqlClient.removeTimeSeriesAssociation(dataIRI2.get(0));
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI2));
    }

    @Test
    public void testRemoveTimeSeries() {
        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);
        // Initialise other time series with only one data IRI in kb
        sparqlClient.initTS(tsIRI2, dataIRI2, dbURL, timeUnit);

        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Add different property to one of the data IRI for which the time series will be removed to ensure it stays in the KG
        testKnowledgeBase.getIndividual(dataIRI1.get(0)).
                addComment(testKnowledgeBase.createLiteral("This data IRI should not be connected to a time series"));

        // Remove the first time series
        sparqlClient.removeTimeSeries(tsIRI1);

        // Check that time series does not exist anymore
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI1));
        // Check that the one data IRI still exists and is not attached to the time series
        Assert.assertNotNull(testKnowledgeBase.getIndividual(dataIRI1.get(0)));
        Assert.assertNull(testKnowledgeBase.getIndividual(dataIRI1.get(0))
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries")));
        // Check that other series does still exist and the data is still attached
        Assert.assertNotNull(testKnowledgeBase.getIndividual(tsIRI2));
        for (String iri: dataIRI2) {
            RDFNode object = testKnowledgeBase.getIndividual(iri)
                    .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI2, object.asResource().getURI());
        }
    }

    @Test
    public void testRemoveAllTimeSeries() {
        // Initialise time series in kb
        sparqlClient.initTS(tsIRI1, dataIRI1, dbURL, timeUnit);
        // Initialise other time series with only one data IRI in kb
        sparqlClient.initTS(tsIRI2, dataIRI2, dbURL, timeUnit);

        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Add different property to one of the data IRIs to ensure it stays in the KG
        testKnowledgeBase.getIndividual(dataIRI1.get(0)).
                addComment(testKnowledgeBase.createLiteral("This data IRI should not be connected to a time series"));

        // Remove all time series
        sparqlClient.removeAllTimeSeries();

        // Check that time series does not exist anymore
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI1));
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI2));
        // Check that the one data IRI still exists and is not attached to the time series
        Assert.assertNotNull(testKnowledgeBase.getIndividual(dataIRI1.get(0)));
        Assert.assertNull(testKnowledgeBase.getIndividual(dataIRI1.get(0))
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries")));
    }
    
    @Test
    public void loadSparqlConfigs () throws IOException, NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
    	// Initialise TimeSeriesClient
    	RemoteStoreClient kbClient = new RemoteStoreClient();    
    	TimeSeriesSparql client = new TimeSeriesSparql(kbClient);
    	// Filepath to not yet created file in temporary test folder
    	String filepath = Paths.get(folder.getRoot().toString(), "timeseries.properties").toString();
    	// JPSRuntime error messages
    	String m1 = "TimeSeriesSparql: No properties file found at specified filepath: " + filepath;
    	String m2 = "TimeSeriesSparql: Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\" ";
    	String m3 = "TimeSeriesSparql: Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\" ";
    	
    	// Test for non-existing properties file
    	try {
    		client.loadSparqlConfigs(filepath);
    		Assert.fail();
    	} catch (JPSRuntimeException e) {
    		Assert.assertEquals(m1, e.getMessage());
    	}
    	
    	// Test for missing query endpoint by creating a file only containing update endpoint
    	writePropertyFile(filepath, Collections.singletonList("sparql.update.endpoint=test_update"));
	    // Try loading SPARQL configs
    	try {
    		client.loadSparqlConfigs(filepath);
            Assert.fail();
    	} catch (JPSRuntimeException e) {
    		Assert.assertEquals(m2, e.getMessage());
    	}

    	// Test for missing update endpoint by creating a file only containing query endpoint
        writePropertyFile(filepath, Collections.singletonList("sparql.query.endpoint=test_query"));
	    // Try loading SPARQL configs
    	try {
    		client.loadSparqlConfigs(filepath);
            Assert.fail();
    	} catch (JPSRuntimeException e) {
    		Assert.assertEquals(m3, e.getMessage());
    	}
    	
    	// Test for proper query and update endpoint
        writePropertyFile(filepath, Arrays.asList("sparql.query.endpoint=test_query", "sparql.update.endpoint=test_update"));
	    // Try loading SPARQL configs
    	try {
    		client.loadSparqlConfigs(filepath);
    	} catch (Exception e) {
    	    Assert.fail(e.getMessage());
    	}
    	// Retrieve the value of the private field 'kbClient' of the client
        Field kbc = client.getClass().getDeclaredField("kbClient");
        kbc.setAccessible(true);
        RemoteStoreClient kbcl = (RemoteStoreClient) kbc.get(client);
    	Assert.assertEquals("test_query", kbcl.getQueryEndpoint());
    	Assert.assertEquals("test_update", kbcl.getUpdateEndpoint());

    }
    
    @Test
	public void testBulkInitTs() {
		String tsIRI1 = "http://tsIRI1";
		List<String> dataIRI1 = Arrays.asList("http://data1", "http://data2", "http://data3");
		String tsIRI2 = "http://tsIRI2";
		List<String> dataIRI2 = Collections.singletonList("http://data4");
		String dbURL = "jdbc:postgresql:timeseries";
		List<String> timeUnits = Arrays.asList("s", "s");
		
		List<String> tsList = Arrays.asList(tsIRI1, tsIRI2);
		List<List<String>> dataIRIs = new ArrayList<>();
		dataIRIs.add(dataIRI1); dataIRIs.add(dataIRI2);
		
		sparqlClient.bulkInitTS(tsList, dataIRIs, dbURL, timeUnits);
		
		OntModel testKnowledgeBase = mockClient.getKnowledgeBase();
		
		Property hasRDB = ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasRDB");
		Property hasTimeUnit = ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeUnit");
		Property hasTimeSeries = ResourceFactory.createProperty(TimeSeriesSparql.ns_ontology + "hasTimeSeries");
		Resource TimeSeries = ResourceFactory.createResource(TimeSeriesSparql.ns_ontology + "TimeSeries");
		
		for (String dataIRI : dataIRI1) {
			Assert.assertTrue(testKnowledgeBase.contains(ResourceFactory.createResource(dataIRI), hasTimeSeries, ResourceFactory.createResource(tsIRI1)));
		}
		
		for (String dataIRI : dataIRI2) {
			Assert.assertTrue(testKnowledgeBase.contains(ResourceFactory.createResource(dataIRI), hasTimeSeries, ResourceFactory.createResource(tsIRI2)));
		}
		
		for (String tsIRI : tsList) {
			Resource ts = ResourceFactory.createResource(tsIRI);
			Assert.assertEquals(testKnowledgeBase.getIndividual(tsIRI).getRDFType(),TimeSeries);
			Assert.assertTrue(testKnowledgeBase.contains(ts, hasRDB, ResourceFactory.createStringLiteral(dbURL)));
			Assert.assertTrue(testKnowledgeBase.contains(ts, hasTimeUnit, ResourceFactory.createStringLiteral("s")));
		}
	}
    
    private void writePropertyFile(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
    	FileWriter writer = new FileWriter(filepath, false);
    	// Populate file
    	for (String s : properties) {
    		writer.write(s + "\n");
    	}
    	// Close the file and return the file
    	writer.close();
    }

}
