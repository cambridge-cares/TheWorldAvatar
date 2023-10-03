package uk.ac.cam.cares.jps.base.timeseries;

import org.jooq.tools.jdbc.MockConnection;
import org.jooq.tools.jdbc.MockDataProvider;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.mocks.PostgresMock;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;

/**
 * This class provides unit tests for the TimeSeriesClient class, particularly
 * for methods that do not have connection in the argument
 */

public class TimeSeriesClientWithoutConnTest {
    // Instance of the class to test with mocked sub-clients
    private TimeSeriesClient<Instant> testClientWithMocks;
    // Time series test data
    private List<String> dataIRIs;
    private List<Class<?>> dataClasses;
    private final String timeUnit = "http://s";

    @Mock(answer = Answers.RETURNS_DEEP_STUBS)
    private TimeSeriesSparql mockSparqlClient;
    @Mock
    private TimeSeriesRDBClient<Instant> mockRDBClient;
    @Mock
    private TimeSeries<Instant> mockTimeSeries;
    MockDataProvider mockRDB = new PostgresMock();
    MockConnection conn = new MockConnection(mockRDB);
    private AutoCloseable closeMocks;

    @Before
    public void setUpClient() throws URISyntaxException, IOException {
        String kgEndpoint = "http://localhost:9999/blazegraph/namespace/timeseries/sparql";
        RemoteStoreClient remoteStoreClient = new RemoteStoreClient(kgEndpoint, kgEndpoint);
        TimeSeriesRDBClient<Instant> timeSeriesRDBClient = new TimeSeriesRDBClient<>(Instant.class);
        testClientWithMocks = new TimeSeriesClient<>(remoteStoreClient, timeSeriesRDBClient);
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
        // Specify type of data for each column (most data will be in doubles, but one
        // can specify different data types)
        dataClasses = Arrays.asList(Double.class, String.class, Integer.class);
    }

    @Test
    public void testInitTimeSeriesExceptionAfterStep1()
            throws NoSuchFieldException, IllegalAccessException, SQLException {
        // Set-up stubbing
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).initTS(Mockito.any(), Mockito.any(),
                Mockito.any(), Mockito.any());
        setRDFMock();
        setRDBMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
                () -> testClientWithMocks.initTimeSeries(dataIRIs, dataClasses, timeUnit));
        Assert.assertTrue(e.getMessage().contains("Timeseries was not created"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertEquals("KG down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
    }

    @Test
    public void testInitTimeSeriesExceptionAfterStep2()
            throws NoSuchFieldException, IllegalAccessException, SQLException {
        // KG reversion works //
        // Set-up stubbing
        Mockito.doNothing().when(mockSparqlClient).initTS(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
        setRDFMock();
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).initTimeSeriesTable(Mockito.anyList(),
                Mockito.anyList(), Mockito.anyString(), Mockito.any(Connection.class));
        setRDBMock();
        // Set private fields accessible to insert the mock
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        rdfClientField.set(testClientWithMocks, mockSparqlClient);
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        rdbClientField.set(testClientWithMocks, mockRDBClient);

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
                () -> testClientWithMocks.initTimeSeries(dataIRIs, dataClasses, timeUnit));
        Assert.assertTrue(e.getMessage().contains("Timeseries was not created"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertEquals("RDB down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());

        // KG reversion does not work //
        // Set-up stubbing
        Mockito.doNothing().when(mockSparqlClient).initTS(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
        ArgumentCaptor<String> tsIRI = ArgumentCaptor.forClass(String.class);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeTimeSeries(tsIRI.capture());
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).initTimeSeriesTable(Mockito.anyList(),
                Mockito.anyList(), Mockito.anyString());
        // Set private fields accessible to insert the mock
        rdbClientField.setAccessible(true);
        rdbClientField.set(testClientWithMocks, mockRDBClient);

        JPSRuntimeException e2 = Assert.assertThrows(JPSRuntimeException.class,
                () -> testClientWithMocks.initTimeSeries(dataIRIs, dataClasses, timeUnit));
        Assert.assertTrue(e2.getMessage().contains("Inconsistent state created when initialising time series"));
        Assert.assertTrue(e2.getMessage().contains(tsIRI.getValue()));
        Assert.assertTrue(e2.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
    }

    @Test
    public void testDeleteTimeSeriesNoTSIRI() throws NoSuchFieldException, IllegalAccessException, SQLException {
        String tsIRI = "tsIRI";
        // Set-up stubbing
        Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(false);
        setRDFMock();
        setRDBMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
                () -> testClientWithMocks.deleteTimeSeries(tsIRI));
        Assert.assertTrue(e.getMessage().contains("does not exist in KG"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(tsIRI));
    }

    @Test
    public void testDeleteTimeSeriesExceptionAfterStep1()
            throws NoSuchFieldException, IllegalAccessException, SQLException {
        String tsIRI = "tsIRI";
        // Set-up stubbing
        Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.when(mockSparqlClient.getTimeSeriesType(tsIRI))
                .thenReturn(TimeSeriesSparql.TIMESERIES_NAMESPACE + "TimeSeries");
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeTimeSeries(tsIRI);
        setRDFMock();
        setRDBMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
                () -> testClientWithMocks.deleteTimeSeries(tsIRI));
        Assert.assertTrue(e.getMessage().contains("was not deleted"));
        Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e.getMessage().contains(tsIRI));
        Assert.assertEquals("KG down", e.getCause().getMessage());
        Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
    }

    @Test
    public void testDeleteTimeSeriesExceptionAfterStep2()
            throws NoSuchFieldException, IllegalAccessException, SQLException {
        String tsIRI = "tsIRI";
        // KG reversion works //
        // Set-up stubbing
        Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.when(mockSparqlClient.getTimeSeriesType(tsIRI))
                .thenReturn(TimeSeriesSparql.TIMESERIES_NAMESPACE + "TimeSeries");
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeries(tsIRI);
        setRDFMock();
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteEntireTimeSeries(dataIRIs.get(0),
                conn);
        setRDBMock();

        JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
                () -> testClientWithMocks.deleteTimeSeries(tsIRI));
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
        Mockito.when(mockSparqlClient.getTimeSeriesType(tsIRI))
                .thenReturn(TimeSeriesSparql.TIMESERIES_NAMESPACE + "TimeSeries");
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeries(tsIRI);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient)
                .reInitTS(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient)
                .deleteEntireTimeSeries(dataIRIs.get(0));

        JPSRuntimeException e2 = Assert.assertThrows(JPSRuntimeException.class,
                () -> testClientWithMocks.deleteTimeSeries(tsIRI));
        Assert.assertTrue(e2.getMessage().contains("Inconsistent state created when deleting time series"));
        Assert.assertTrue(e2.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
        Assert.assertTrue(e2.getMessage().contains(tsIRI));
    }

    private void setRDFMock() throws NoSuchFieldException, IllegalAccessException {
        // Set private fields accessible to insert the mock
        Field rdfClientField = TimeSeriesClient.class.getDeclaredField("rdfClient");
        rdfClientField.setAccessible(true);
        rdfClientField.set(testClientWithMocks, mockSparqlClient);
    }

    private void setRDBMock() throws NoSuchFieldException, IllegalAccessException, SQLException {
        Field rdbClientField = TimeSeriesClient.class.getDeclaredField("rdbClient");
        rdbClientField.setAccessible(true);
        rdbClientField.set(testClientWithMocks, mockRDBClient);
        Mockito.when(mockRDBClient.getConnection()).thenReturn(conn);
    }
}
