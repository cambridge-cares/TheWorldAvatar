package uk.ac.cam.cares.jps.base.timeseries;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;


/**
 * This class provides unit tests for the TimeSeriesClient class
 */

public class TimeSeriesClientTest {

    // Instance of the class to test
    private TimeSeriesClient<Instant> testClient;
    // Instance of the class to test with mocked sub-clients
    private TimeSeriesClient<Instant> testClientWithMocks;
    // Time series test data
    private List<String> dataIRIs;
    private List<Class<?>> dataClasses;
    private final String timeUnit = "http://s";

    @Mock(answer = Answers.RETURNS_DEEP_STUBS) private TimeSeriesSparql mockSparqlClient;
    @Mock private TimeSeriesRDBClient<Instant> mockRDBClient;
    @Mock private TimeSeries<Instant> mockTimeSeries;
    private AutoCloseable closeMocks;

    @Before
    public void setUpClient() throws URISyntaxException, IOException {
        testClient = new TimeSeriesClient<>(Instant.class,
                Paths.get(Objects.requireNonNull(getClass().getResource("/timeseries.properties")).toURI()).toString());
        testClientWithMocks = new TimeSeriesClient<>(Instant.class,
                Paths.get(Objects.requireNonNull(getClass().getResource("/timeseries.properties")).toURI()).toString());
    }

    @Before
    public void setUpTestData() {
         // Initialise time series with 3 associated data series
        dataIRIs = Arrays.asList("http://data1", "http://data2", "http://data3");
        // Specify type of data for each column (most data will be in doubles, but one can specify different data types)
        dataClasses = Arrays.asList(Double.class, String.class, Integer.class);
    }

    @Test
    public void testConstructorWithKBClientAndRdbProperties() throws IOException, NoSuchFieldException, IllegalAccessException, URISyntaxException {
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint("sparql_query");
        kbClient.setUpdateEndpoint("sparql_update");
        String db_url = "jdbc:postgresql:test";
        String db_user = "test_user";
        TimeSeriesClient<Instant> client = new TimeSeriesClient<>(kbClient, Instant.class, db_url, db_user, "test_pw");

        // Retrieve the rdf client to test whether it is set correctly
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        TimeSeriesSparql rdfClient = (TimeSeriesSparql) rdfClientField.get(client);
        Field kbClientField = TimeSeriesSparql.class.getDeclaredField("kbClient");
        kbClientField.setAccessible(true);
        StoreClientInterface setKBClient = (StoreClientInterface) kbClientField.get(rdfClient);
        Assert.assertEquals(kbClient.getQueryEndpoint(), setKBClient.getQueryEndpoint());
        Assert.assertEquals(kbClient.getUpdateEndpoint(), setKBClient.getUpdateEndpoint());
        // Retrieve the rdb client to test whether it is set correctly
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        TimeSeriesRDBClient<Instant> rdbClient = (TimeSeriesRDBClient<Instant>) rdbClientField.get(client);
        Assert.assertEquals(db_url, rdbClient.getRdbURL());
        Assert.assertEquals(db_user, rdbClient.getRdbUser());
    }
    
    @Test
    public void testConstructorWithKBClient() throws IOException, NoSuchFieldException, IllegalAccessException, URISyntaxException {
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint("sparql_query");
        kbClient.setUpdateEndpoint("sparql_update");
        TimeSeriesClient<Instant> client = new TimeSeriesClient<>(kbClient, Instant.class,
                Paths.get(Objects.requireNonNull(getClass().getResource("/timeseries.properties")).toURI()).toString());

        // Retrieve the rdf client to test whether it is set correctly
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        TimeSeriesSparql rdfClient = (TimeSeriesSparql) rdfClientField.get(client);
        Field kbClientField = TimeSeriesSparql.class.getDeclaredField("kbClient");
        kbClientField.setAccessible(true);
        StoreClientInterface setKBClient = (StoreClientInterface) kbClientField.get(rdfClient);
        Assert.assertEquals(kbClient.getQueryEndpoint(), setKBClient.getQueryEndpoint());
        Assert.assertEquals(kbClient.getUpdateEndpoint(), setKBClient.getUpdateEndpoint());
        // Retrieve the rdb client to test whether it is set correctly
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        TimeSeriesRDBClient<Instant> rdbClient = (TimeSeriesRDBClient<Instant>) rdbClientField.get(client);
        Assert.assertEquals("jdbc:postgresql:timeseries", rdbClient.getRdbURL());
        Assert.assertEquals("postgres", rdbClient.getRdbUser());
    }

    @Before
    public void openMocks() {
        closeMocks = MockitoAnnotations.openMocks(this);
    }

    @After
    public void releaseMocks() throws Exception {
        closeMocks.close();
    }

    @Test
    public void testConstructorWithOnlyPropertiesFile() throws NoSuchFieldException, IllegalAccessException {
        // Retrieve the rdf client to test whether it is set correctly
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        TimeSeriesSparql rdfClient = (TimeSeriesSparql) rdfClientField.get(testClient);
        Field kbClientField = TimeSeriesSparql.class.getDeclaredField("kbClient");
        kbClientField.setAccessible(true);
        StoreClientInterface setKBClient = (StoreClientInterface) kbClientField.get(rdfClient);
        Assert.assertEquals("http://localhost:9999/blazegraph/namespace/timeseries/sparql", setKBClient.getQueryEndpoint());
        Assert.assertEquals("http://localhost:9999/blazegraph/namespace/timeseries/sparql", setKBClient.getUpdateEndpoint());
        // Retrieve the rdb client to test whether it is set correctly
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        TimeSeriesRDBClient<Instant> rdbClient = (TimeSeriesRDBClient<Instant>) rdbClientField.get(testClient);
        Assert.assertEquals("jdbc:postgresql:timeseries", rdbClient.getRdbURL());
        Assert.assertEquals("postgres", rdbClient.getRdbUser());
    }

    @Test
    public void testSetKBClient() throws NoSuchFieldException, IllegalAccessException {
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint("sparql_query");
        kbClient.setUpdateEndpoint("sparql_update");
        testClient.setKBClient(kbClient);
        // Retrieve the rdf client to test whether it is set correctly
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        TimeSeriesSparql rdfClient = (TimeSeriesSparql) rdfClientField.get(testClient);
        Field kbClientField = TimeSeriesSparql.class.getDeclaredField("kbClient");
        kbClientField.setAccessible(true);
        StoreClientInterface setKBClient = (StoreClientInterface) kbClientField.get(rdfClient);
        Assert.assertEquals(kbClient.getQueryEndpoint(), setKBClient.getQueryEndpoint());
        Assert.assertEquals(kbClient.getUpdateEndpoint(), setKBClient.getUpdateEndpoint());
    }

    @Test
    public void testSetRDBClient() throws NoSuchFieldException, IllegalAccessException {
        testClient.setRDBClient("testURL", "user", "password");
        // Retrieve the rdb client to test whether it is set correctly
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        TimeSeriesRDBClient<Instant> rdbClient = (TimeSeriesRDBClient<Instant>) rdbClientField.get(testClient);
        Assert.assertEquals("testURL", rdbClient.getRdbURL());
        Assert.assertEquals("user", rdbClient.getRdbUser());
        Field passwordField = TimeSeriesRDBClient.class.getDeclaredField("rdbPassword");
        passwordField.setAccessible(true);
        Assert.assertEquals("password", passwordField.get(rdbClient));
    }

    @Test
    public void testInitTimeSeriesExceptionAfterStep1() throws NoSuchFieldException, IllegalAccessException {
        // Set-up stubbing
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).
                initTS(Mockito.anyString(), Mockito.anyList(), Mockito.anyString(), Mockito.anyString());
        setRDFMock();
        
        try {
            testClientWithMocks.initTimeSeries(dataIRIs, dataClasses, timeUnit);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Timeseries was not created"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertEquals("KG down", e.getCause().getMessage());
            Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        }
    }

    @Test
    public void testInitTimeSeriesExceptionAfterStep2() throws NoSuchFieldException, IllegalAccessException {
        // KG reversion works //
        // Set-up stubbing
        Mockito.doNothing().when(mockSparqlClient).
                initTS(Mockito.anyString(), Mockito.anyList(), Mockito.anyString(), Mockito.anyString());
        setRDFMock();
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).
                initTimeSeriesTable(Mockito.anyList(), Mockito.anyList(), Mockito.anyString());
        setRDBMock();
        // Set private fields accessible to insert the mock
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        rdfClientField.set(testClientWithMocks, mockSparqlClient);
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        rdbClientField.set(testClientWithMocks, mockRDBClient);
        
        try {
            testClientWithMocks.initTimeSeries(dataIRIs, dataClasses, timeUnit);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Timeseries was not created"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertEquals("RDB down", e.getCause().getMessage());
            Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        }
        // KG reversion does not work //
        // Set-up stubbing
        Mockito.doNothing().when(mockSparqlClient).
                initTS(Mockito.anyString(), Mockito.anyList(), Mockito.anyString(), Mockito.anyString());
        ArgumentCaptor<String> tsIRI = ArgumentCaptor.forClass(String.class);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).
                removeTimeSeries(tsIRI.capture());
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).
                initTimeSeriesTable(Mockito.anyList(), Mockito.anyList(), Mockito.anyString());
        // Set private fields accessible to insert the mock
        rdbClientField.setAccessible(true);
        rdbClientField.set(testClientWithMocks, mockRDBClient);
        try {
            testClientWithMocks.initTimeSeries(dataIRIs, dataClasses, timeUnit);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Inconsistent state created when initialising time series"));
            Assert.assertTrue(e.getMessage().contains(tsIRI.getValue()));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        }
    }

    @Test
    public void testDeleteIndividualTimeSeriesNoTSIRI() throws NoSuchFieldException, IllegalAccessException {
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRIs.get(0))).thenReturn(null);
        setRDFMock();
        try {
            testClientWithMocks.deleteIndividualTimeSeries(dataIRIs.get(0));
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("not associated with any timeseries"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertTrue(e.getMessage().contains(dataIRIs.get(0)));
        }
    }

    @Test
    public void testDeleteIndividualTimeSeriesExceptionAfterStep1() throws NoSuchFieldException, IllegalAccessException {
        String dataIRI = dataIRIs.get(0);
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRI)).thenReturn("tsIRI");
        Mockito.when(mockSparqlClient.getAssociatedData(dataIRI).size()).thenReturn(2);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeTimeSeriesAssociation(dataIRI);
        setRDFMock();
        try {
            testClientWithMocks.deleteIndividualTimeSeries(dataIRI);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("was not deleted"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertTrue(e.getMessage().contains(dataIRI));
            Assert.assertEquals("KG down", e.getCause().getMessage());
            Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        }
    }

    @Test
    public void testDeleteIndividualTimeSeriesExceptionAfterStep2() throws NoSuchFieldException, IllegalAccessException {
        String dataIRI = dataIRIs.get(0);
        // KG reversion works //
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRI)).thenReturn("tsIRI");
        Mockito.when(mockSparqlClient.getAssociatedData(dataIRI).size()).thenReturn(2);
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeriesAssociation(dataIRI);
        setRDFMock();
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteTimeSeries(dataIRI);
        setRDBMock();
        try {
            testClientWithMocks.deleteIndividualTimeSeries(dataIRI);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("was not deleted"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertTrue(e.getMessage().contains(dataIRI));
            Assert.assertEquals("RDB down", e.getCause().getMessage());
            Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        }
        // KG reversion does not work //
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRI)).thenReturn("tsIRI");
        Mockito.when(mockSparqlClient.getAssociatedData(dataIRI).size()).thenReturn(2);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient)
                .insertTimeSeriesAssociation(Mockito.anyString(), Mockito.anyString());
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteTimeSeries(dataIRI);
        try {
            testClientWithMocks.deleteIndividualTimeSeries(dataIRI);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Inconsistent state created when deleting time series"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertTrue(e.getMessage().contains(dataIRI));
        }
    }
    
    @Test
    public void testDeleteTimeSeriesNoTSIRI() throws NoSuchFieldException, IllegalAccessException {
    	String tsIRI = "tsIRI";
    	// Set-up stubbing
        Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(false);
        setRDFMock();      
        
        try {
            testClientWithMocks.deleteTimeSeries(tsIRI);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("does not exist in KG"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertTrue(e.getMessage().contains(tsIRI));
        }
    }

    @Test
    public void testDeleteTimeSeriesExceptionAfterStep1() throws NoSuchFieldException, IllegalAccessException {
    	String tsIRI = "tsIRI";
        // Set-up stubbing
    	Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeTimeSeries(tsIRI);
        setRDFMock();
        
        try {
            testClientWithMocks.deleteTimeSeries(tsIRI);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("was not deleted"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertTrue(e.getMessage().contains(tsIRI));
            Assert.assertEquals("KG down", e.getCause().getMessage());
            Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        }
    }

    @Test
    public void testDeleteTimeSeriesExceptionAfterStep2() throws NoSuchFieldException, IllegalAccessException {
    	String tsIRI = "tsIRI";
        // KG reversion works //
        // Set-up stubbing
    	Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeries(tsIRI);
        setRDFMock();
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteTimeSeriesTable(dataIRIs.get(0));
        setRDBMock();
        
        try {
            testClientWithMocks.deleteTimeSeries(tsIRI);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("was not deleted"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertTrue(e.getMessage().contains(tsIRI));
            Assert.assertEquals("RDB down", e.getCause().getMessage());
            Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        }
        
        // KG reversion does not work //
        // Set-up stubbing
    	Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeries(tsIRI);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient)
                .initTS(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteTimeSeriesTable(dataIRIs.get(0));
        
        try {
            testClientWithMocks.deleteTimeSeries(tsIRI);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Inconsistent state created when deleting time series"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertTrue(e.getMessage().contains(tsIRI));
        }
    }
    
    @Test
    public void testDeleteAllException() throws NoSuchFieldException, IllegalAccessException {
        // KG Exception //
        // Set-up stubbing
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeAllTimeSeries();
        setRDFMock();
        
        try {
            testClientWithMocks.deleteAll();
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Not all timeseries were deleted from KG!"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertEquals("KG down", e.getCause().getMessage());
            Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        }
        // RDB Exception //
        // Set-up stubbing
        Mockito.doNothing().when(mockSparqlClient).removeAllTimeSeries();
        setRDFMock();
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteAll();
        setRDBMock();
        
        try {
            testClientWithMocks.deleteAll();
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Not all timeseries were deleted from database!"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertEquals("RDB down", e.getCause().getMessage());
            Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        }
    }
    
    @Test
    public void testAddTimeSeriesException() throws NoSuchFieldException, IllegalAccessException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
          
        try {
            testClientWithMocks.addTimeSeriesData(mockTimeSeries);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Central RDB lookup table has not been initialised yet"));
        }
    }
    
    @Test
    public void testGetTimeSeriesWithinBoundsException() throws NoSuchFieldException, IllegalAccessException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
      
        try {
            testClientWithMocks.getTimeSeriesWithinBounds(dataIRIs, null, null);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Central RDB lookup table has not been initialised yet"));
        }
    }
    
    @Test
    public void testGetTimeSeriesException() throws NoSuchFieldException, IllegalAccessException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
       
        try {
            testClientWithMocks.getTimeSeries(dataIRIs);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Central RDB lookup table has not been initialised yet"));
        }
    }
    
    @Test
    public void testGetAverageException() throws NoSuchFieldException, IllegalAccessException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
    	        
        try {
            testClientWithMocks.getAverage(dataIRIs.get(0));
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("TimeSeriesRDBClient: Error while executing SQL command"));
        }
    }
    
    @Test
    public void testGetMaxValueException() throws NoSuchFieldException, IllegalAccessException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
        
        try {
            testClientWithMocks.getMaxValue(dataIRIs.get(0));
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("TimeSeriesRDBClient: Error while executing SQL command"));
        }
    }
    
    @Test
    public void testGetMinValueException() throws NoSuchFieldException, IllegalAccessException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
        
        try {
            testClientWithMocks.getMinValue(dataIRIs.get(0));
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("TimeSeriesRDBClient: Error while executing SQL command"));
        }
    }
    
    @Test
    public void testGetMaxTimeException() throws NoSuchFieldException, IllegalAccessException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
        
        try {
            testClientWithMocks.getMaxTime(dataIRIs.get(0));
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("TimeSeriesRDBClient: Error while executing SQL command"));
        }
    }
    
    @Test
    public void testGetMinTimeException() throws NoSuchFieldException, IllegalAccessException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
        
        try {
            testClientWithMocks.getMinTime(dataIRIs.get(0));
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("TimeSeriesRDBClient: Error while executing SQL command"));
        }
    }
    
    @Test
    public void testDeleteTimeSeriesHistoryException() throws NoSuchFieldException, IllegalAccessException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
        
        try {
            testClientWithMocks.deleteTimeSeriesHistory(dataIRIs.get(0), null, null);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("TimeSeriesRDBClient: Error while executing SQL command"));
        }
    }

    private void setRDFMock() throws NoSuchFieldException, IllegalAccessException {
        // Set private fields accessible to insert the mock
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        rdfClientField.set(testClientWithMocks, mockSparqlClient);
    }

    private void setRDBMock() throws NoSuchFieldException, IllegalAccessException {
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        rdbClientField.set(testClientWithMocks, mockRDBClient);
    }

}
