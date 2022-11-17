package uk.ac.cam.cares.jps.base.timeseries;

import org.jooq.tools.jdbc.MockConnection;
import org.jooq.tools.jdbc.MockDataProvider;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.mocks.PostgresMock;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;


/**
 * This class provides unit tests for the TimeSeriesClient class, particularly for methods that do not have connection in the argument
 */

public class TimeSeriesClientWithoutConnTest {
    // Instance of the class to test with mocked sub-clients
    private TimeSeriesClient<Instant> testClientWithMocks;
    // Time series test data
    private List<String> dataIRIs;
    private List<Class<?>> dataClasses;
    private final String timeUnit = "http://s";

    @Mock(answer = Answers.RETURNS_DEEP_STUBS) private TimeSeriesSparql mockSparqlClient;
    @Mock private TimeSeriesRDBClient<Instant> mockRDBClient;
    @Mock private TimeSeries<Instant> mockTimeSeries;
    MockDataProvider mockRDB = new PostgresMock();
    MockConnection conn = new MockConnection(mockRDB);
    private AutoCloseable closeMocks;

    @Before
    public void setUpClient() throws URISyntaxException, IOException {
        testClientWithMocks = new TimeSeriesClient<>(Instant.class,
                Paths.get(Objects.requireNonNull(getClass().getResource("/timeseries.properties")).toURI()).toString());
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
    public void testInitTimeSeriesExceptionAfterStep1() throws NoSuchFieldException, IllegalAccessException, SQLException {
        // Set-up stubbing
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).
                initTS(Mockito.anyString(), Mockito.anyList(), Mockito.anyString(), Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
        setRDFMock();
        setRDBMock();

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
    public void testInitTimeSeriesExceptionAfterStep2() throws NoSuchFieldException, IllegalAccessException, SQLException {
        // KG reversion works //
        // Set-up stubbing
        Mockito.doNothing().when(mockSparqlClient).
                initTS(Mockito.anyString(), Mockito.anyList(), Mockito.anyString(), Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
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
                initTS(Mockito.anyString(), Mockito.anyList(), Mockito.anyString(), Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
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
    public void testDeleteIndividualTimeSeriesNoTSIRI() throws NoSuchFieldException, IllegalAccessException, SQLException {
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRIs.get(0))).thenReturn(null);
        setRDFMock();
        setRDBMock();
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
    public void testDeleteIndividualTimeSeriesExceptionAfterStep1() throws NoSuchFieldException, IllegalAccessException, SQLException {
        String dataIRI = dataIRIs.get(0);
        // Set-up stubbing
        Mockito.when(mockSparqlClient.getTimeSeries(dataIRI)).thenReturn("tsIRI");
        Mockito.when(mockSparqlClient.getAssociatedData(dataIRI).size()).thenReturn(2);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeTimeSeriesAssociation(dataIRI);
        setRDFMock();
        setRDBMock();
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
    public void testDeleteTimeSeriesNoTSIRI() throws NoSuchFieldException, IllegalAccessException, SQLException {
        String tsIRI = "tsIRI";
        // Set-up stubbing
        Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(false);
        setRDFMock();
        setRDBMock();

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
    public void testDeleteTimeSeriesExceptionAfterStep1() throws NoSuchFieldException, IllegalAccessException, SQLException {
        String tsIRI = "tsIRI";
        // Set-up stubbing
        Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.when(mockSparqlClient.getTimeSeriesType(tsIRI)).thenReturn(TimeSeriesSparql.ns_ontology+"TimeSeries");
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeTimeSeries(tsIRI);
        setRDFMock();
        setRDBMock();

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
    public void testDeleteTimeSeriesExceptionAfterStep2() throws NoSuchFieldException, IllegalAccessException, SQLException {
        String tsIRI = "tsIRI";
        // KG reversion works //
        // Set-up stubbing
        Mockito.when(mockSparqlClient.checkTimeSeriesExists(tsIRI)).thenReturn(true);
        Mockito.when(mockSparqlClient.getAssociatedData(tsIRI)).thenReturn(dataIRIs);
        Mockito.when(mockSparqlClient.getTimeUnit(tsIRI)).thenReturn(timeUnit);
        Mockito.when(mockSparqlClient.getTimeSeriesType(tsIRI)).thenReturn(TimeSeriesSparql.ns_ontology+"TimeSeries");
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeries(tsIRI);
        setRDFMock();
        Mockito.doThrow(new JPSRuntimeException("RDB down")).when(mockRDBClient).deleteTimeSeriesTable(dataIRIs.get(0), conn);
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
        Mockito.when(mockSparqlClient.getTimeSeriesType(tsIRI)).thenReturn(TimeSeriesSparql.ns_ontology+"TimeSeries");
        Mockito.doNothing().when(mockSparqlClient).removeTimeSeries(tsIRI);
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient)
                .initTS(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
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
    public void testDeleteAllException() throws NoSuchFieldException, IllegalAccessException, SQLException {
        // KG Exception //
        // Set-up stubbing
        Mockito.doThrow(new JPSRuntimeException("KG down")).when(mockSparqlClient).removeAllTimeSeries();
        setRDFMock();
        setRDBMock();

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

        try {
            testClientWithMocks.deleteAll();
            // Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("Not all timeseries were deleted from database!"));
            Assert.assertTrue(e.getMessage().contains(testClientWithMocks.getClass().getSimpleName()));
            Assert.assertEquals("RDB down", e.getCause().getMessage());
            Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
        }
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
