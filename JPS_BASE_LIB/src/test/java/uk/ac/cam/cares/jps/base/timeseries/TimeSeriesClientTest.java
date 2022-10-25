package uk.ac.cam.cares.jps.base.timeseries;

import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.jooq.tools.jdbc.MockConnection;
import org.jooq.tools.jdbc.MockDataProvider;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


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
    MockDataProvider mockRDB = new PostgresMock();
    MockConnection conn = new MockConnection(mockRDB);

    @Mock(answer = Answers.RETURNS_DEEP_STUBS) private TimeSeriesSparql mockSparqlClient;
    @Mock private TimeSeriesRDBClient<Instant> mockRDBClient;
    @Mock private TimeSeries<Instant> mockTimeSeries;
    private AutoCloseable closeMocks;

    @Before
    public void setUpClient() throws URISyntaxException, IOException {
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint("sparql_query");
        kbClient.setUpdateEndpoint("sparql_update");
        testClient = new TimeSeriesClient<>(kbClient, Instant.class);
        testClientWithMocks = new TimeSeriesClient<>(kbClient, Instant.class);
    }
    
    @Before
    public void openMocks() {
        closeMocks = MockitoAnnotations.openMocks(this);
    }

    @After
    public void releaseMocks() throws Exception {
        closeMocks.close();
    }

    @Before
    public void setUpTestData() {
         // Initialise time series with 3 associated data series
        dataIRIs = Arrays.asList("http://data1", "http://data2", "http://data3");
        // Specify type of data for each column (most data will be in doubles, but one can specify different data types)
        dataClasses = Arrays.asList(Double.class, String.class, Integer.class);
    }


    @Test
    public void testConstructor() throws NoSuchFieldException, IllegalAccessException {
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setQueryEndpoint("sparql_query");
        kbClient.setUpdateEndpoint("sparql_update");
        TimeSeriesClient<Instant> client = new TimeSeriesClient<>(kbClient, Instant.class);

        // Retrieve the rdf client to test whether it is set correctly
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        TimeSeriesSparql rdfClient = (TimeSeriesSparql) rdfClientField.get(client);
        Field kbClientField = TimeSeriesSparql.class.getDeclaredField("kbClient");
        kbClientField.setAccessible(true);
        TripleStoreClientInterface setKBClient = (TripleStoreClientInterface) kbClientField.get(rdfClient);
        Assert.assertEquals(kbClient.getQueryEndpoint(), setKBClient.getQueryEndpoint());
        Assert.assertEquals(kbClient.getUpdateEndpoint(), setKBClient.getUpdateEndpoint());
        // Retrieve the rdb client to test whether it is set correctly
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        TimeSeriesRDBClient<Instant> rdbClient = (TimeSeriesRDBClient<Instant>) rdbClientField.get(client);
        Field timecolumn = TimeSeriesRDBClient.class.getDeclaredField("timeColumn");
        timecolumn.setAccessible(true);
        org.jooq.Field timeColumnField = (org.jooq.Field) timecolumn.get(rdbClient);
        Assert.assertEquals("time", timeColumnField.getName());
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
        TripleStoreClientInterface setKBClient = (TripleStoreClientInterface) kbClientField.get(rdfClient);
        Assert.assertEquals(kbClient.getQueryEndpoint(), setKBClient.getQueryEndpoint());
        Assert.assertEquals(kbClient.getUpdateEndpoint(), setKBClient.getUpdateEndpoint());
    }

    @Test
    public void testInitTimeSeriesExceptionAfterStep1() throws NoSuchFieldException, IllegalAccessException, SQLException {
        // Set-up stubbing
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).
                initTS(Mockito.anyString(), Mockito.anyList(), Mockito.anyString(), Mockito.anyString());
        setRDFMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.initTimeSeries(dataIRIs, dataClasses, timeUnit, conn));
        Assert.assertTrue(e.getMessage().contains("Timeseries was not created"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertEquals("KG down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
    }

    @Test
    public void testInitTimeSeriesExceptionAfterStep2() throws NoSuchFieldException, IllegalAccessException {
        // KG reversion works //
        // Set-up stubbing
        Mockito.doNothing().when(mockSparqlClient).
                initTS(Mockito.anyString(), Mockito.anyList(), Mockito.anyString(), Mockito.anyString());
        setRDFMock();
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).
                initTimeSeriesTable(Mockito.anyList(), Mockito.anyList(), Mockito.anyString(), Mockito.any(Connection.class));
        setRDBMock();
        // Set private fields accessible to insert the mock
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        rdfClientField.set(testClientWithMocks, mockSparqlClient);
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        rdbClientField.set(testClientWithMocks, mockRDBClient);
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,() -> testClientWithMocks.initTimeSeries(dataIRIs, dataClasses, timeUnit, conn));
        Assert.assertTrue(e.getMessage().contains("Timeseries was not created"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertEquals("RDB down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        // KG reversion does not work //
        // Set-up stubbing
        Mockito.doNothing().when(mockSparqlClient).
                initTS(Mockito.anyString(), Mockito.anyList(), Mockito.anyString(), Mockito.anyString());
        ArgumentCaptor<String> tsIRI = ArgumentCaptor.forClass(String.class);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).
                removeTimeSeries(tsIRI.capture());
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).
                initTimeSeriesTable(Mockito.anyList(), Mockito.anyList(), Mockito.anyString(), Mockito.any(Connection.class));
        // Set private fields accessible to insert the mock
        rdbClientField.setAccessible(true);
        rdbClientField.set(testClientWithMocks, mockRDBClient);

        e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.initTimeSeries(dataIRIs, dataClasses, timeUnit, conn));
        Assert.assertTrue(e.getMessage().contains("Inconsistent state created when initialising time series"));
        Assert.assertTrue(e.getMessage().contains(tsIRI.getValue()));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
    }

    @Test
    public void testDeleteIndividualTimeSeriesNoTSIRI() throws NoSuchFieldException, IllegalAccessException {
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRIs.get(0))).thenReturn(null);
        setRDFMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteIndividualTimeSeries(dataIRIs.get(0), conn));
        Assert.assertTrue(e.getMessage().contains("not associated with any timeseries"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(dataIRIs.get(0)));
    }

    @Test
    public void testDeleteIndividualTimeSeriesExceptionAfterStep1() throws NoSuchFieldException, IllegalAccessException, SQLException {
        String dataIRI = dataIRIs.get(0);
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRI)).thenReturn("tsIRI");
        Mockito.when(mockSparqlClient.getAssociatedData(dataIRI).size()).thenReturn(2);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeTimeSeriesAssociation(dataIRI);
        setRDFMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteIndividualTimeSeries(dataIRI, conn));
        Assert.assertTrue(e.getMessage().contains("was not deleted"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(dataIRI));
        Assert.assertEquals("KG down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
    }

    @Test
    public void testDeleteIndividualTimeSeriesExceptionAfterStep2() throws NoSuchFieldException, IllegalAccessException, SQLException {
        String dataIRI = dataIRIs.get(0);
        // KG reversion works //
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRI)).thenReturn("tsIRI");
        Mockito.when(mockSparqlClient.getAssociatedData(dataIRI).size()).thenReturn(2);
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeriesAssociation(dataIRI);
        setRDFMock();

        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteTimeSeries(dataIRI, conn);
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteIndividualTimeSeries(dataIRI, conn));
        Assert.assertTrue(e.getMessage().contains("was not deleted"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(dataIRI));
        Assert.assertEquals("RDB down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        // KG reversion does not work //
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRI)).thenReturn("tsIRI");
        Mockito.when(mockSparqlClient.getAssociatedData(dataIRI).size()).thenReturn(2);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient)
                .insertTimeSeriesAssociation(Mockito.anyString(), Mockito.anyString());
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteTimeSeries(dataIRI, conn);
        e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteIndividualTimeSeries(dataIRI, conn));
        Assert.assertTrue(e.getMessage().contains("Inconsistent state created when deleting time series"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(dataIRI));
    }

    @Test
    public void testDeleteTimeSeriesNoTSIRI() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	String tsIRI = "tsIRI";
    	// Set-up stubbing
        Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(false);
        setRDFMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteTimeSeries(tsIRI, conn));
        Assert.assertTrue(e.getMessage().contains("does not exist in KG"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(tsIRI));
    }

    @Test
    public void testDeleteTimeSeriesExceptionAfterStep1() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	String tsIRI = "tsIRI";
        // Set-up stubbing
    	Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeTimeSeries(tsIRI);
        setRDFMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteTimeSeries(tsIRI, conn));
        Assert.assertTrue(e.getMessage().contains("was not deleted"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(tsIRI));
        Assert.assertEquals("KG down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
    }

    @Test
    public void testDeleteTimeSeriesExceptionAfterStep2() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	String tsIRI = "tsIRI";
        // KG reversion works //
        // Set-up stubbing
    	Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeries(tsIRI);
        setRDFMock();

        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteTimeSeriesTable(dataIRIs.get(0), conn);
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteTimeSeries(tsIRI, conn));
        Assert.assertTrue(e.getMessage().contains("was not deleted"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(tsIRI));
        Assert.assertEquals("RDB down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());

        // KG reversion does not work //
        // Set-up stubbing
        Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeries(tsIRI);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient)
                .initTS(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteTimeSeriesTable(dataIRIs.get(0), conn);

        e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteTimeSeries(tsIRI, conn));
        Assert.assertTrue(e.getMessage().contains("Inconsistent state created when deleting time series"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(tsIRI));
    }

    @Test
    public void testDeleteAllException() throws NoSuchFieldException, IllegalAccessException, SQLException {
        // KG Exception //
        // Set-up stubbing
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeAllTimeSeries();
        setRDFMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteAll(conn));
        Assert.assertTrue(e.getMessage().contains("Not all timeseries were deleted from KG!"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertEquals("KG down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());

        // RDB Exception //
        // Set-up stubbing
        Mockito.doNothing().when(mockSparqlClient).removeAllTimeSeries();
        setRDFMock();
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteAll(conn);
        setRDBMock();

        e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteAll(conn));
        Assert.assertTrue(e.getMessage().contains("Not all timeseries were deleted from database!"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertEquals("RDB down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
    }

    @Test
    public void testAddTimeSeriesException() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	// Only tests for the first Exception to occur when called without prior initialised time series
        // Set-up stubbing
        Mockito.doCallRealMethod().when(mockRDBClient).addTimeSeriesData(Mockito.any(), Mockito.any(Connection.class));
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.addTimeSeriesData(mockTimeSeries, conn));
        Assert.assertTrue(e.getMessage().contains("Central RDB lookup table has not been initialised yet"));
    }

    @Test
    public void testGetTimeSeriesWithinBoundsException() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	// Only tests for the first Exception to occur when called without prior initialised time series

        // Set-up stubbing
        Mockito.doCallRealMethod().when(mockRDBClient).getTimeSeriesWithinBounds(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.getTimeSeriesWithinBounds(dataIRIs, null, null, conn));
        Assert.assertTrue(e.getMessage().contains("Central RDB lookup table has not been initialised yet"));
    }

    @Test
    public void testGetTimeSeriesException() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	// Only tests for the first Exception to occur when called without prior initialised time series

        // Set-up stubbing
        Mockito.doCallRealMethod().when(mockRDBClient).getTimeSeriesWithinBounds(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.getTimeSeries(dataIRIs, conn));
        Assert.assertTrue(e.getMessage().contains("Central RDB lookup table has not been initialised yet"));
    }

    @Test
    public void testGetAverageException() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	// Only tests for the first Exception to occur when called without prior initialised time series

        // Set-up stubbing
        Mockito.doCallRealMethod().when(mockRDBClient).getAverage(Mockito.any(), Mockito.any());
        Mockito.doCallRealMethod().when(mockRDBClient).getAggregate(Mockito.any(), Mockito.any(), Mockito.any());
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.getAverage(dataIRIs.get(0), conn));
        Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
    }

    @Test
    public void testGetMaxValueException() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	// Only tests for the first Exception to occur when called without prior initialised time series

        // Set-up stubbing
        Mockito.doCallRealMethod().when(mockRDBClient).getMaxValue(Mockito.any(), Mockito.any());
        Mockito.doCallRealMethod().when(mockRDBClient).getAggregate(Mockito.any(), Mockito.any(), Mockito.any());
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.getMaxValue(dataIRIs.get(0), conn));
        Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
    }

    @Test
    public void testGetMinValueException() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	// Only tests for the first Exception to occur when called without prior initialised time series

        // Set-up stubbing
        Mockito.doCallRealMethod().when(mockRDBClient).getMinValue(Mockito.any(), Mockito.any());
        Mockito.doCallRealMethod().when(mockRDBClient).getAggregate(Mockito.any(), Mockito.any(), Mockito.any());
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.getMinValue(dataIRIs.get(0), conn));
        Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
    }

    @Test
    public void testGetMaxTimeException() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	// Only tests for the first Exception to occur when called without prior initialised time series

        // Set-up stubbing
        Mockito.doCallRealMethod().when(mockRDBClient).getMaxTime(Mockito.any(), Mockito.any());
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.getMaxTime(dataIRIs.get(0), conn));
        Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
    }

    @Test
    public void testGetMinTimeException() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	// Only tests for the first Exception to occur when called without prior initialised time series

        // Set-up stubbing
        Mockito.doCallRealMethod().when(mockRDBClient).getMinTime(Mockito.any(), Mockito.any());
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.getMinTime(dataIRIs.get(0), conn));
        Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
    }

    @Test
    public void testDeleteTimeSeriesHistoryException() throws NoSuchFieldException, IllegalAccessException, SQLException {
    	// Only tests for the first Exception to occur when called without prior initialised time series

        // Set-up stubbing
        Mockito.doCallRealMethod().when(mockRDBClient).deleteRows(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
        setRDBMock();
        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> testClientWithMocks.deleteTimeSeriesHistory(dataIRIs.get(0), null, null, conn));
        Assert.assertTrue(e.getMessage().contains("Error while executing SQL command"));
    }

    @Test
    public void testConvertToJSON() {
    	List<Instant> instantList = new ArrayList<>();
    	List<List<?>> dataToAdd = new ArrayList<>();
    	List<Double> data1 = new ArrayList<>();
    	List<String> data2 = new ArrayList<>();
    	List<Integer> data3 = new ArrayList<>();
    	dataIRIs = new ArrayList<>();
    	dataIRIs.add("http://data1"); dataIRIs.add("http://data2"); dataIRIs.add("http://data3");
		for (int i = 0; i < 10; i++) {
			instantList.add(Instant.now().plusSeconds(i));
			data1.add(Double.valueOf(i));
			data2.add(String.valueOf(i));
			data3.add(Integer.valueOf(i));
		}
		dataToAdd.add(data1); dataToAdd.add(data2); dataToAdd.add(data3);
    	TimeSeries<Instant> ts_instant = new TimeSeries<Instant>(instantList, dataIRIs, dataToAdd);

    	List<Map<String,String>> units = new ArrayList<>();
    	Map<String,String> unit = new HashMap<>();
    	unit.put("http://data1", "unit1");
    	unit.put("http://data2", "unit2");
    	unit.put("http://data3", "unit3");
    	units.add(unit);

    	JSONArray ts_jarray = testClient.convertToJSON(Arrays.asList(ts_instant), Arrays.asList(1,2), units, null);

    	JSONObject ts_jo = ts_jarray.getJSONObject(0);
    	List<String> keys = ts_jo.keySet().stream().collect(Collectors.toList());
    	Assert.assertTrue(keys.contains("data"));
    	Assert.assertTrue(keys.contains("values"));
    	Assert.assertTrue(keys.contains("timeClass"));
    	Assert.assertTrue(keys.contains("valuesClass"));
    	Assert.assertTrue(keys.contains("id"));
    	Assert.assertTrue(keys.contains("units"));
    	Assert.assertTrue(keys.contains("time"));
    }

    @Test
    public void testConvertToJSONwithMissingValues() {
    	List<Instant> instantList = new ArrayList<>();
    	List<List<?>> dataToAdd = new ArrayList<>();
    	List<Double> data1 = new ArrayList<>();
    	List<String> data2 = new ArrayList<>();
    	List<Integer> data3 = new ArrayList<>();
    	dataIRIs = new ArrayList<>();
    	dataIRIs.add("http://data1"); dataIRIs.add("http://data2"); dataIRIs.add("http://data3");
		for (int i = 0; i < 10; i++) {
			instantList.add(Instant.now().plusSeconds(i));
			data1.add(Double.valueOf(i));
			// Include data series with fully missing data
			data2.add(null);
			data3.add(Integer.valueOf(i));
		}
		// Include test data series with partially missing data
		data1.set(0, null); data1.set(1, null);
		dataToAdd.add(data1); dataToAdd.add(data2); dataToAdd.add(data3);
    	TimeSeries<Instant> ts_instant = new TimeSeries<Instant>(instantList, dataIRIs, dataToAdd);

    	List<Map<String,String>> units = new ArrayList<>();
    	Map<String,String> unit = new HashMap<>();
    	unit.put("http://data1", "unit1");
    	unit.put("http://data2", "unit2");
    	unit.put("http://data3", "unit3");
    	units.add(unit);

    	JSONArray ts_jarray = testClient.convertToJSON(Arrays.asList(ts_instant), Arrays.asList(1,2), units, null);

    	JSONObject ts_jo = ts_jarray.getJSONObject(0);
    	List<String> keys = ts_jo.keySet().stream().collect(Collectors.toList());
    	Assert.assertTrue(keys.contains("data"));
    	Assert.assertTrue(keys.contains("values"));
    	Assert.assertTrue(keys.contains("timeClass"));
    	Assert.assertTrue(keys.contains("valuesClass"));
    	// Verify that valuesClass contains Unknown and twice Number
        Assert.assertTrue(ts_jo.get("valuesClass").toString().contains("Unknown"));
    	Assert.assertEquals(3, ts_jo.get("valuesClass").toString().split("Number").length);
    	Assert.assertTrue(keys.contains("id"));
    	Assert.assertTrue(keys.contains("units"));
    	Assert.assertTrue(keys.contains("time"));
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
