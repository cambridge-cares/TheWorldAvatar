package uk.ac.cam.cares.jps.base.timeseries;

import java.lang.reflect.Field;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.*;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.*;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateRequest;
import org.apache.jena.vocabulary.RDFS;
import org.json.JSONArray;
import org.junit.*;
import org.junit.rules.TemporaryFolder;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;

/**
 * This class provides unit tests for the TimeSeriesSparql class
 */

public class TimeSeriesSparqlTest {

    @ClassRule
    // Temporary folder to place a properties file (same file for all potential
    // tests)
    public static TemporaryFolder folder = new TemporaryFolder();

    private MockKnowledgeBaseClient mockClient;
    private TimeSeriesSparql sparqlClient;

    // Initialise correct namespaces to use for ontology and knowledge base
    private final String TIMESERIES_NAMESPACE = "https://www.theworldavatar.com/kg/ontotimeseries/";
    private final String NS_TIME = "http://www.w3.org/2006/time#";

    // Initialise IRIs for 2 times series: 1 with 3 associated data series and 1
    // with only 1 associated data series

    private final String tsIRI1 = "http://tsIRI1";
    private final List<String> dataIRI1 = Arrays.asList("http://data1", "http://data2", "http://data3");
    private final Duration duration1 = Duration.ofDays(31 * 8);
    private final ChronoUnit chronoUnit1 = ChronoUnit.MONTHS;
    private final String temporalUnit1 = TimeSeriesSparql.NS_TIME + "unitMonth";
    private final Double numericDuration1 = 8.0;

    private final String tsIRI2 = "http://tsIRI2";
    private final List<String> dataIRI2 = Collections.singletonList("http://data4");
    private final String dbURL = "jdbc:postgresql:timeseries";
    private final String schema = "public";
    private final String timeUnit = "http://s";

    private Class<?> timeClass = Double.class;
    private Class<?> rdbClientClass = TimeSeriesRDBClient.class;

    private final double epsilon = 0.000001d;

    // Class that is used as the knowledge base client in the tests.
    // It will use a jena model to execute queries on.
    private static class MockKnowledgeBaseClient extends RemoteStoreClient {

        private final OntModel kb;
        private String query;

        MockKnowledgeBaseClient(OntModel kb) {
            this.kb = kb;
        }

        public OntModel getKnowledgeBase() {
            return kb;
        }

        public void closeKnowledgeBase() {
            if (!(kb == null)) {
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
                return new JSONArray("[{'ASK': " + askResult + "}]").toString();
            } else {
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
        public CloseableHttpResponse executeUpdateByPost(String query) {
            executeUpdate(query);
            return null;
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
    public void testInitTS() {
        TimeSeriesSparql sparqlClient = new TimeSeriesSparql(mockClient);
        // Initialise AVERAGE time series in knowledge base

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata.setDuration(duration1);
        timeSeriesKgMetadata.setDurationUnit(chronoUnit1);

        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Test that the IRIs are correctly set as individuals
        Assert.assertNotNull(testKnowledgeBase.getIndividual(tsIRI1));
        for (String iri : dataIRI1) {
            Assert.assertNotNull(testKnowledgeBase.getIndividual(iri));
        }
        String avgPeriodIRI = sparqlClient.getAveragingPeriod(tsIRI1);
        Assert.assertNotNull(testKnowledgeBase.getIndividual(avgPeriodIRI));

        // Test timeseries instance
        Assert.assertEquals(TimeSeriesSparql.TIMESERIES_NAMESPACE + "AverageTimeSeries",
                testKnowledgeBase.getIndividual(tsIRI1).getRDFType().getURI());
        RDFNode object = testKnowledgeBase.getIndividual(tsIRI1)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasRDB"))
                .getObject();
        Assert.assertTrue(object.isLiteral());
        Assert.assertEquals(dbURL, object.asLiteral().getString());
        object = testKnowledgeBase.getIndividual(tsIRI1)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeUnit"))
                .getObject();
        Assert.assertTrue(object.isLiteral());
        Assert.assertEquals(timeUnit, object.asLiteral().getString());

        // Test that data IRIs are attached to time series instance
        for (String iri : dataIRI1) {
            object = testKnowledgeBase.getIndividual(iri)
                    .getProperty(
                            ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI1, object.asResource().getURI());
        }

        // Averaging Period IRI
        object = testKnowledgeBase.getIndividual(tsIRI1)
                .getProperty(
                        ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasAveragingPeriod"))
                .getObject();
        Assert.assertTrue(object.isResource());
        Assert.assertEquals(avgPeriodIRI, object.asResource().getURI());

        // Temporal unit linked to averaging period IRI
        object = testKnowledgeBase.getIndividual(avgPeriodIRI)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.NS_TIME + "unitType")).getObject();
        Assert.assertTrue(object.isResource());
        Assert.assertEquals(temporalUnit1, object.asResource().getURI());

        // Numerical duration linked to averaging period
        object = testKnowledgeBase.getIndividual(avgPeriodIRI)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.NS_TIME + "numericDuration")).getObject();
        Assert.assertTrue(object.isLiteral());
        Assert.assertEquals(numericDuration1, object.asLiteral().getDouble(), epsilon);

        // Initialise two average time series with same duration and temporal unit
        // Both time series should be attached to the same averaging period
        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setDuration(duration1);
        timeSeriesKgMetadata2.setDurationUnit(chronoUnit1);

        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);
        // check both time series is attched to the same averaging period
        Assert.assertEquals(
                testKnowledgeBase.getIndividual(tsIRI1)
                        .getProperty(ResourceFactory
                                .createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasAveragingPeriod"))
                        .getObject().asResource().getURI(),
                testKnowledgeBase.getIndividual(tsIRI2)
                        .getProperty(ResourceFactory
                                .createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasAveragingPeriod"))
                        .getObject().asResource().getURI());

        // Trying to init different time series but same data IRI should result in an
        // exception
        TimeSeriesKgMetadata timeSeriesKgMetadata3 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata3.setDataIriList(dataIRI1);
        timeSeriesKgMetadata3.setTimeUnit(timeUnit);
        timeSeriesKgMetadata3.setTimeSeriesType(Type.GENERAL);

        Exception exception = Assert.assertThrows(JPSRuntimeException.class,
                () -> sparqlClient.initTS(timeSeriesKgMetadata3, dbURL, schema, timeClass, rdbClientClass));
        String errorMessage = exception.getMessage();
        Assert.assertTrue(errorMessage.contains("One or more of the provided data IRI has an existing time series"));
        Assert.assertTrue(errorMessage.contains(TimeSeriesSparql.class.getSimpleName()));

        // clear existing time series
        sparqlClient.removeTimeSeries(tsIRI1);
        sparqlClient.removeTimeSeries(tsIRI2);

        // Trying to init average time series with a negative duration value should
        // result in an exception
        timeSeriesKgMetadata2.setDuration(Duration.ofDays(-12));
        exception = Assert.assertThrows(JPSRuntimeException.class,
                () -> sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass));
        errorMessage = exception.getMessage();
        Assert.assertTrue(errorMessage.contains("Numeric Duration must be a positive value"));
        Assert.assertTrue(errorMessage.contains(TimeSeriesSparql.class.getSimpleName()));

        // Trying to init average time series with an invalid temporal unit should
        // result in an exception
        timeSeriesKgMetadata2.setDuration(Duration.ofDays(12));
        timeSeriesKgMetadata2.setDurationUnit(ChronoUnit.MICROS);
        exception = Assert.assertThrows(JPSRuntimeException.class,
                () -> sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass));
        errorMessage = exception.getMessage();
        Assert.assertTrue(errorMessage.contains("Temporal Unit: Micros of invalid type"));
        Assert.assertTrue(errorMessage.contains(TimeSeriesSparql.class.getSimpleName()));
    }

    @Test
    public void testCheckTimeSeriesExists() {
        Assert.assertFalse(sparqlClient.checkTimeSeriesExists(tsIRI1));

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.INSTANTANEOUS);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);

        Assert.assertTrue(sparqlClient.checkTimeSeriesExists(tsIRI1));
        Assert.assertFalse(sparqlClient.checkTimeSeriesExists(tsIRI2));
    }

    @Test
    public void testCheckDataExists() {
        Assert.assertFalse(sparqlClient.checkDataHasTimeSeries("http://data1"));

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.INSTANTANEOUS);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);

        Assert.assertTrue(sparqlClient.checkDataHasTimeSeries("http://data1"));
        Assert.assertFalse(sparqlClient.checkDataHasTimeSeries("http://data5"));

        // Retrieve test knowledge base
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();
        // Add a data IRI with a property that is not attached to a time series
        testKnowledgeBase.add(testKnowledgeBase.createResource(dataIRI2.get(0)), RDFS.comment,
                "Data IRI without timeseries");
        Assert.assertFalse(sparqlClient.checkDataHasTimeSeries(dataIRI2.get(0)));

    }

    @Test
    public void testCountTS() {
        Assert.assertEquals(0, sparqlClient.countTS());

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.INSTANTANEOUS);

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.INSTANTANEOUS);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertEquals(1, sparqlClient.countTS());
        // Initialise different time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertEquals(2, sparqlClient.countTS());
    }

    @Test
    public void testGetTimeSeries() {
        Assert.assertNull(sparqlClient.getTimeSeries(dataIRI1.get(0)));

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.INSTANTANEOUS);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertNull(sparqlClient.getTimeSeries(dataIRI2.get(0)));
        for (String iri : dataIRI1) {
            Assert.assertEquals(tsIRI1, sparqlClient.getTimeSeries(iri));
        }

        // Retrieve test knowledge base
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();
        // Add a data IRI with a property
        testKnowledgeBase.add(testKnowledgeBase.createResource(dataIRI2.get(0)), RDFS.comment,
                "Data IRI without timeseries");
        Assert.assertNull(sparqlClient.getTimeSeries(dataIRI2.get(0)));
    }

    @Test
    public void testGetDbUrl() {
        Assert.assertNull(sparqlClient.getDbUrl(tsIRI1));

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.INSTANTANEOUS);

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.INSTANTANEOUS);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertNull(sparqlClient.getDbUrl(tsIRI2));
        Assert.assertEquals(dbURL, sparqlClient.getDbUrl(tsIRI1));
        // Initialise different time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL + "_2", schema, timeClass, rdbClientClass);
        Assert.assertEquals(dbURL, sparqlClient.getDbUrl(tsIRI1));
        Assert.assertEquals(dbURL + "_2", sparqlClient.getDbUrl(tsIRI2));
    }

    @Test
    public void testGetTimeUnit() {
        Assert.assertNull(sparqlClient.getTimeUnit(tsIRI1));

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.INSTANTANEOUS);

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(null);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.INSTANTANEOUS);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertNull(sparqlClient.getTimeUnit(tsIRI2));
        Assert.assertEquals(timeUnit, sparqlClient.getTimeUnit(tsIRI1));
        // Initialise different time series in kb without time series
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertEquals(timeUnit, sparqlClient.getTimeUnit(tsIRI1));
        Assert.assertNull(sparqlClient.getTimeUnit(tsIRI2));
    }

    @Test
    public void testGetAssociatedData() {
        Assert.assertEquals(0, sparqlClient.getAssociatedData(tsIRI1).size());

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.INSTANTANEOUS);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);

        Assert.assertEquals(0, sparqlClient.getAssociatedData(tsIRI2).size());
        List<String> retrievedDataIRI = sparqlClient.getAssociatedData(tsIRI1);
        Assert.assertEquals(3, sparqlClient.getAssociatedData(tsIRI1).size());
        // Check that data IRIs were retrieved correctly
        for (String iri : dataIRI1) {
            Assert.assertTrue(retrievedDataIRI.contains(iri));
        }
    }

    @Test
    public void testGetAllTimeSeries() {
        Assert.assertEquals(0, sparqlClient.getAllTimeSeries().size());

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.INSTANTANEOUS);

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.INSTANTANEOUS);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertEquals(1, sparqlClient.getAllTimeSeries().size());
        // Initialise different time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);
        List<String> retrievedTimeSeries = sparqlClient.getAllTimeSeries();
        Assert.assertEquals(2, retrievedTimeSeries.size());
        for (String iri : Arrays.asList(tsIRI1, tsIRI2)) {
            Assert.assertTrue(retrievedTimeSeries.contains(iri));
        }
    }

    @Test
    public void testInsertTimeSeriesAssociation() {

        String dataIRIThatGetsAdded = "http://data4";

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.GENERAL);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Test that data IRIs are attached to time series instance
        for (String iri : dataIRI1) {
            RDFNode object = testKnowledgeBase.getIndividual(iri)
                    .getProperty(
                            ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI1, object.asResource().getURI());
        }
        sparqlClient.insertTimeSeriesAssociation(dataIRIThatGetsAdded, tsIRI1);
        // Test that new attached data IRI is really attached to time series instance
        RDFNode object = testKnowledgeBase.getIndividual(dataIRIThatGetsAdded)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries"))
                .getObject();
        Assert.assertTrue(object.isResource());
        Assert.assertEquals(tsIRI1, object.asResource().getURI());

        // Check exception for already existing time series association (with any time
        // series)
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
                    .getProperty(
                            ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI1, object.asResource().getURI());
        }
    }

    @Test
    public void testRemoveTimeSeriesAssociation() {

        String dataIRIThatGetsRemoved = "http://data1";

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata.setDuration(duration1);
        timeSeriesKgMetadata.setDurationUnit(chronoUnit1);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Add different property to data IRI for which the connection should be removed
        // to ensure it stays in the KG
        testKnowledgeBase.getIndividual(dataIRIThatGetsRemoved)
                .addComment(testKnowledgeBase.createLiteral("This data IRI should not be connected to a time series"));

        // Remove association to one of the data IRIs
        sparqlClient.removeTimeSeriesAssociation(dataIRIThatGetsRemoved);
        // Try to remove association of data IRI without connection (should not have any
        // effect)
        sparqlClient.removeTimeSeriesAssociation(dataIRI2.get(0));

        // Check that time series is still in the Knowledge Graph
        Assert.assertNotNull(testKnowledgeBase.getIndividual(tsIRI1));
        // Check that the remaining data IRIs are still attached to the time series
        for (String iri : dataIRI1) {
            if (!iri.equals(dataIRIThatGetsRemoved)) {
                RDFNode object = testKnowledgeBase.getIndividual(iri)
                        .getProperty(
                                ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries"))
                        .getObject();
                Assert.assertTrue(object.isResource());
                Assert.assertEquals(tsIRI1, object.asResource().getURI());
            }
        }
        // Check that the data for which the connection was removed does exist but the
        // connection was removed
        Assert.assertNotNull(testKnowledgeBase.getIndividual(dataIRIThatGetsRemoved));
        Assert.assertNull(testKnowledgeBase.getIndividual(dataIRIThatGetsRemoved)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries")));

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.GENERAL);

        // Initialise other time series with only one data IRI in kb
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);
        // Remove association to the only attached data IRI, which will remove the whole
        // time series instance
        sparqlClient.removeTimeSeriesAssociation(dataIRI2.get(0));
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI2));
    }

    @Test
    public void testRemoveTimeSeries() {
        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.GENERAL);

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata2.setDuration(duration1);
        timeSeriesKgMetadata2.setDurationUnit(chronoUnit1);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        // Initialise other time series with only one data IRI in kb
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);

        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Add different property to one of the data IRI for which the time series will
        // be removed to ensure it stays in the KG
        testKnowledgeBase.getIndividual(dataIRI1.get(0))
                .addComment(testKnowledgeBase.createLiteral("This data IRI should not be connected to a time series"));

        // Remove the first time series
        sparqlClient.removeTimeSeries(tsIRI1);

        // Check that time series does not exist anymore
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI1));
        // Check that the one data IRI still exists and is not attached to the time
        // series
        Assert.assertNotNull(testKnowledgeBase.getIndividual(dataIRI1.get(0)));
        Assert.assertNull(testKnowledgeBase.getIndividual(dataIRI1.get(0))
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries")));
        // Check that other series does still exist and the data is still attached
        Assert.assertNotNull(testKnowledgeBase.getIndividual(tsIRI2));
        for (String iri : dataIRI2) {
            RDFNode object = testKnowledgeBase.getIndividual(iri)
                    .getProperty(
                            ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI2, object.asResource().getURI());
        }

        // Initialise time series 1 back in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        String avgPeriod = sparqlClient.getAveragingPeriod(tsIRI2);
        Assert.assertNotNull(testKnowledgeBase.getIndividual(avgPeriod));

        // Remove average time series with averaging period connected to only one time
        // series
        sparqlClient.removeTimeSeries(tsIRI2);
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI2));
        Assert.assertNull(testKnowledgeBase.getIndividual(avgPeriod));

        // Check that other series does still exist and the data is still attached
        Assert.assertNotNull(testKnowledgeBase.getIndividual(tsIRI1));
        for (String iri : dataIRI1) {
            RDFNode object = testKnowledgeBase.getIndividual(iri)
                    .getProperty(
                            ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI1, object.asResource().getURI());
        }
        sparqlClient.removeTimeSeries(tsIRI1);

        // Initialise two average time series with same numeric duration and temporal
        // unit
        // Deleting one time series should still preserve the common averaging period
        timeSeriesKgMetadata.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata.setDuration(duration1);
        timeSeriesKgMetadata.setDurationUnit(chronoUnit1);

        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);
        avgPeriod = sparqlClient.getAveragingPeriod(tsIRI1);
        Assert.assertNotNull(testKnowledgeBase.getIndividual(avgPeriod));

        // check both time series is attached to the same averaging period
        Assert.assertEquals(avgPeriod, testKnowledgeBase.getIndividual(tsIRI2)
                .getProperty(
                        ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasAveragingPeriod"))
                .getObject().asResource().getURI());

        // Remove average time series 1
        sparqlClient.removeTimeSeries(tsIRI1);

        // Check that time series does not exist anymore
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI1));

        // Check that the averaging period IRI still exists and is attached to the 2nd
        // average time series
        Assert.assertNotNull(testKnowledgeBase.getIndividual(avgPeriod));
        Assert.assertEquals(avgPeriod, testKnowledgeBase.getIndividual(tsIRI2)
                .getProperty(
                        ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasAveragingPeriod"))
                .getObject().asResource().getURI());
        Assert.assertEquals(temporalUnit1,
                testKnowledgeBase.getIndividual(avgPeriod)
                        .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.NS_TIME + "unitType")).getObject()
                        .asResource().getURI());
        Assert.assertEquals(numericDuration1,
                testKnowledgeBase.getIndividual(avgPeriod)
                        .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.NS_TIME + "numericDuration"))
                        .getObject().asLiteral().getDouble(),
                epsilon);

        // Check that other series does still exist and the data is still attached
        Assert.assertNotNull(testKnowledgeBase.getIndividual(tsIRI2));
        for (String iri : dataIRI2) {
            RDFNode object = testKnowledgeBase.getIndividual(iri)
                    .getProperty(
                            ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI2, object.asResource().getURI());
        }
    }

    @Test
    public void testRemoveAllTimeSeries() {
        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.GENERAL);

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.GENERAL);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        // Initialise other time series with only one data IRI in kb
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);

        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Add different property to one of the data IRIs to ensure it stays in the KG
        testKnowledgeBase.getIndividual(dataIRI1.get(0))
                .addComment(testKnowledgeBase.createLiteral("This data IRI should not be connected to a time series"));

        // Remove all time series
        sparqlClient.removeAllTimeSeries();

        // Check that time series does not exist anymore
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI1));
        Assert.assertNull(testKnowledgeBase.getIndividual(tsIRI2));
        // Check that the one data IRI still exists and is not attached to the time
        // series
        Assert.assertNotNull(testKnowledgeBase.getIndividual(dataIRI1.get(0)));
        Assert.assertNull(testKnowledgeBase.getIndividual(dataIRI1.get(0))
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries")));
    }

    @Test
    public void testBulkInitTs() {
        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.INSTANTANEOUS);

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata2.setDuration(Duration.ofDays(366 * 5));
        timeSeriesKgMetadata2.setDurationUnit(ChronoUnit.YEARS);

        sparqlClient.bulkInitTS(Arrays.asList(timeSeriesKgMetadata, timeSeriesKgMetadata2), dbURL, schema, timeClass,
                rdbClientClass);

        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        Property hasRDB = ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasRDB");
        Property hasTimeUnit = ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeUnit");
        Property hasTimeSeries = ResourceFactory
                .createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries");
        Property hasAveragingPeriod = ResourceFactory
                .createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasAveragingPeriod");
        Property unitType = ResourceFactory.createProperty(TimeSeriesSparql.NS_TIME + "unitType");
        Property numericDuration = ResourceFactory.createProperty(TimeSeriesSparql.NS_TIME + "numericDuration");
        Resource averageTimeSeries = ResourceFactory
                .createResource(TimeSeriesSparql.TIMESERIES_NAMESPACE + "AverageTimeSeries");
        Resource instantaneousTimeSeries = ResourceFactory
                .createResource(TimeSeriesSparql.TIMESERIES_NAMESPACE + "InstantaneousTimeSeries");
        Resource avgPeriod = ResourceFactory.createResource(sparqlClient.getAveragingPeriod(tsIRI2));

        for (String dataIRI : dataIRI1) {
            Assert.assertTrue(testKnowledgeBase.contains(ResourceFactory.createResource(dataIRI), hasTimeSeries,
                    ResourceFactory.createResource(tsIRI1)));
        }

        for (String dataIRI : dataIRI2) {
            Assert.assertTrue(testKnowledgeBase.contains(ResourceFactory.createResource(dataIRI), hasTimeSeries,
                    ResourceFactory.createResource(tsIRI2)));
        }

        Resource ts;
        for (String tsIRI : Arrays.asList(tsIRI1, tsIRI2)) {
            ts = ResourceFactory.createResource(tsIRI);
            Assert.assertTrue(testKnowledgeBase.contains(ts, hasRDB, ResourceFactory.createStringLiteral(dbURL)));
            Assert.assertTrue(
                    testKnowledgeBase.contains(ts, hasTimeUnit, ResourceFactory.createStringLiteral("http://s")));
        }

        // for tsIRI1 of InstantaneousTimeSeries type
        Assert.assertEquals(testKnowledgeBase.getIndividual(tsIRI1).getRDFType(), instantaneousTimeSeries);

        // for tsIRI2 of AverageTimeSeries type
        ts = ResourceFactory.createResource(tsIRI2);
        Assert.assertEquals(testKnowledgeBase.getIndividual(tsIRI2).getRDFType(), averageTimeSeries);
        Assert.assertTrue(testKnowledgeBase.contains(ts, hasAveragingPeriod, avgPeriod));
        Assert.assertTrue(testKnowledgeBase.contains(avgPeriod, unitType,
                ResourceFactory.createResource(TimeSeriesSparql.NS_TIME + "unitYear")));
        Assert.assertTrue(testKnowledgeBase.contains(avgPeriod, numericDuration,
                ResourceFactory.createTypedLiteral("5.0", XSDDatatype.XSDdecimal)));
    }

    @Test
    public void testGetCustomDuration() {
        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata.setDuration(duration1);
        timeSeriesKgMetadata.setDurationUnit(chronoUnit1);

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.CUMULATIVETOTAL);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);

        TimeSeriesSparql.CustomDuration customDuration = sparqlClient.getCustomDuration(tsIRI1);
        Assert.assertEquals(customDuration.getValue(), numericDuration1, epsilon);
        Assert.assertEquals(customDuration.getUnit(), temporalUnit1);
        Assert.assertNull(sparqlClient.getCustomDuration(tsIRI2));
    }

    @Test
    public void testGetAveragingPeriod() {
        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata.setDuration(duration1);
        timeSeriesKgMetadata.setDurationUnit(chronoUnit1);

        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.STEPWISECUMULATIVE);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);

        String avgPeriodIRI = sparqlClient.getAveragingPeriod(tsIRI1);
        Assert.assertTrue(avgPeriodIRI.contains(TIMESERIES_NAMESPACE + "AveragingPeriod_"));
        Assert.assertNull(sparqlClient.getAveragingPeriod(tsIRI2));

        sparqlClient.removeTimeSeries(tsIRI2);

        timeSeriesKgMetadata2.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata2.setDuration(duration1);
        timeSeriesKgMetadata2.setDurationUnit(chronoUnit1);

        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertEquals(sparqlClient.getAveragingPeriod(tsIRI1), sparqlClient.getAveragingPeriod(tsIRI2));
    }

    @Test
    public void testGetTimeSeriesType() {
        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata.setDuration(duration1);
        timeSeriesKgMetadata.setDurationUnit(chronoUnit1);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);

        Assert.assertEquals(TimeSeriesSparql.AVERAGE_TYPE_STRING, sparqlClient.getTimeSeriesType(tsIRI1));
    }

    @Test
    public void testInitTs2() {
        TimeSeriesSparql sparqlClient = new TimeSeriesSparql(mockClient);
        // Averaging Period IRI
        String durIRI = TIMESERIES_NAMESPACE + "AveragingPeriod_" + UUID.randomUUID();

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(TimeSeriesSparql.AVERAGE_TIMESERIES);
        timeSeriesKgMetadata.setDurationIri(durIRI);
        timeSeriesKgMetadata.setDurationValue(numericDuration1);
        timeSeriesKgMetadata.setDurationUnitIri(temporalUnit1);

        // Initialise AVERAGE time series in knowledge base
        sparqlClient.reInitTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        // Retrieve the updated knowledge base from the mock client
        OntModel testKnowledgeBase = mockClient.getKnowledgeBase();

        // Test that the IRIs are correctly set as individuals
        Assert.assertNotNull(testKnowledgeBase.getIndividual(tsIRI1));
        for (String iri : dataIRI1) {
            Assert.assertNotNull(testKnowledgeBase.getIndividual(iri));
        }
        Assert.assertNotNull(testKnowledgeBase.getIndividual(durIRI));

        // Test timeseries instance
        Assert.assertEquals(TimeSeriesSparql.TIMESERIES_NAMESPACE + "AverageTimeSeries",
                testKnowledgeBase.getIndividual(tsIRI1).getRDFType().getURI());
        RDFNode object = testKnowledgeBase.getIndividual(tsIRI1)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasRDB"))
                .getObject();
        Assert.assertTrue(object.isLiteral());
        Assert.assertEquals(dbURL, object.asLiteral().getString());
        object = testKnowledgeBase.getIndividual(tsIRI1)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeUnit"))
                .getObject();
        Assert.assertTrue(object.isLiteral());
        Assert.assertEquals(timeUnit, object.asLiteral().getString());

        // Test that data IRIs are attached to time series instance
        for (String iri : dataIRI1) {
            object = testKnowledgeBase.getIndividual(iri)
                    .getProperty(
                            ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasTimeSeries"))
                    .getObject();
            Assert.assertTrue(object.isResource());
            Assert.assertEquals(tsIRI1, object.asResource().getURI());
        }

        // Averaging Period IRI
        object = testKnowledgeBase.getIndividual(tsIRI1)
                .getProperty(
                        ResourceFactory.createProperty(TimeSeriesSparql.TIMESERIES_NAMESPACE + "hasAveragingPeriod"))
                .getObject();
        Assert.assertTrue(object.isResource());
        Assert.assertEquals(durIRI, object.asResource().getURI());

        // Temporal unit linked to averaging period IRI
        object = testKnowledgeBase.getIndividual(durIRI)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.NS_TIME + "unitType")).getObject();
        Assert.assertTrue(object.isResource());
        Assert.assertEquals(temporalUnit1, object.asResource().getURI());

        // Numerical duration linked to averaging period
        object = testKnowledgeBase.getIndividual(durIRI)
                .getProperty(ResourceFactory.createProperty(TimeSeriesSparql.NS_TIME + "numericDuration")).getObject();
        Assert.assertTrue(object.isLiteral());
        Assert.assertEquals(numericDuration1, object.asLiteral().getDouble(), epsilon);

        // Trying to init different time series but same data IRI should result in an
        // exception
        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI1);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(TimeSeriesSparql.TIMESERIES);

        Exception exception = Assert.assertThrows(JPSRuntimeException.class,
                () -> sparqlClient.reInitTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass));
        String errorMessage = exception.getMessage();
        Assert.assertTrue(errorMessage.contains("One or more of the provided data IRI has an existing time series"));
        Assert.assertTrue(errorMessage.contains(TimeSeriesSparql.class.getSimpleName()));

        // Trying to init average time series with a negative duration value should
        // result in an exception
        TimeSeriesKgMetadata timeSeriesKgMetadata3 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata3.setDataIriList(dataIRI2);
        timeSeriesKgMetadata3.setTimeUnit(timeUnit);
        timeSeriesKgMetadata3.setTimeSeriesType(TimeSeriesSparql.AVERAGE_TIMESERIES);
        timeSeriesKgMetadata3.setDurationValue(-12.0);
        timeSeriesKgMetadata3.setDurationUnitIri(temporalUnit1);
        timeSeriesKgMetadata3.setDurationIri(durIRI);

        exception = Assert.assertThrows(JPSRuntimeException.class,
                () -> sparqlClient.reInitTS(timeSeriesKgMetadata3, dbURL, schema, timeClass, rdbClientClass));
        errorMessage = exception.getMessage();
        Assert.assertTrue(errorMessage.contains("Numeric Duration must be a positive value"));
        Assert.assertTrue(errorMessage.contains(TimeSeriesSparql.class.getSimpleName()));

        // Trying to init average time series with an invalid temporal unit should
        // result in an exception
        TimeSeriesKgMetadata timeSeriesKgMetadata4 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata4.setDataIriList(dataIRI2);
        timeSeriesKgMetadata4.setTimeUnit(timeUnit);
        timeSeriesKgMetadata4.setTimeSeriesType(TimeSeriesSparql.AVERAGE_TIMESERIES);
        timeSeriesKgMetadata4.setDurationIri(durIRI);
        timeSeriesKgMetadata4.setDurationValue(23.0);
        timeSeriesKgMetadata4.setDurationUnitIri("invalidUnit");

        exception = Assert.assertThrows(JPSRuntimeException.class,
                () -> sparqlClient.reInitTS(timeSeriesKgMetadata4, dbURL, schema, timeClass, rdbClientClass));
        errorMessage = exception.getMessage();
        Assert.assertTrue(errorMessage.contains("Temporal Unit: invalidUnit of invalid type"));
        Assert.assertTrue(errorMessage.contains(TimeSeriesSparql.class.getSimpleName()));
    }

}
