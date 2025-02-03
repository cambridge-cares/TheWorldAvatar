package uk.ac.cam.cares.jps.base.timeseries;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import org.jooq.Table;

import uk.ac.cam.cares.jps.base.BlazegraphContainer;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This class provides integration tests for the TimeSeriesClient class
 */

// @Disabled("Requires both triple store endpoint and postgreSQL database set up
// and running (using testcontainers)\n" +
// "Requires Docker to run the tests. When on Windows, WSL2 as backend is
// required to ensure proper execution")
@Testcontainers
public class TimeSeriesClientIntegrationTest {
    // TimeSeries client (with RDB and Sparql client)
    protected TimeSeriesClient<Instant> tsClient;

    private static RemoteRDBStoreClient rdbStoreClient;
    private static RemoteStoreClient kbClient;

    // Time series test data
    private List<String> dataIRI_1, dataIRI_2;
    private List<Class<?>> dataClass_1, dataClass_2;
    private String timeUnit;
    private Duration duration;
    private ChronoUnit chronoUnit;
    private Double numericalDuration;
    private String temporalUnit;
    private static final double epsilon = 0.000001d;

    // Will create two Docker containers for Blazegraph and postgreSQL
    @Container
    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private static final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    // Initialise 2 test time series data sets
    @BeforeEach
    void initialiseData() {
        // Initialise time unit for all test data series
        timeUnit = "http://s";
        /*
         * Initialise 1st time series with 3 associated data series
         */
        dataIRI_1 = new ArrayList<>();
        dataIRI_1.add("http://data1");
        dataIRI_1.add("http://data2");
        dataIRI_1.add("http://data3");
        // Specify type of data for each column (most data will be in doubles, but one
        // can specify different data types)
        dataClass_1 = new ArrayList<>();
        dataClass_1.add(Double.class);
        dataClass_1.add(String.class);
        dataClass_1.add(Integer.class);
        /*
         * Initialise 2nd time series with only one associated data series
         */
        dataIRI_2 = new ArrayList<>();
        dataIRI_2.add("http://data4");
        // Specify type of data for each column (most data will be in doubles, but one
        // can specify different data types)
        dataClass_2 = new ArrayList<>();
        dataClass_2.add(Double.class);

        duration = Duration.ofDays(33 * 9);
        chronoUnit = ChronoUnit.MONTHS;
        numericalDuration = 9.0;
        temporalUnit = TimeSeriesSparql.NS_TIME + "unitMonth";
    }

    // Create clean slate (new Docker containers) for each test
    @BeforeEach
    void initialiseTimeSeriesClient() {
        try {
            if (!blazegraph.isRunning()) {
                // Start Blazegraph container
                blazegraph.start();
            }

            if (!postgres.isRunning()) {
                // Start postgreSQL container
                postgres.start();
            }

            // Set up a kb client that points to the location of the triple store
            kbClient = blazegraph.getRemoteStoreClient();

            // Initialise TimeSeriesClient client with pre-configured kb client
            setTsClient(kbClient);

            // Configure database access
            rdbStoreClient = new RemoteRDBStoreClient(postgres.getJdbcUrl(), postgres.getUsername(),
                    postgres.getPassword());

            clearTriples();
            clearDatabase();

        } catch (Exception e) {
            throw new JPSRuntimeException(
                    "TimeSeriesClientIntegrationTest: Docker container startup failed. Please try running tests again",
                    e);
        }
    }

    protected void setTsClient(RemoteStoreClient remoteStoreClient) {
        tsClient = new TimeSeriesClient<>(remoteStoreClient, Instant.class);
    }

    // Clear all tables after each test to ensure clean slate
    private static void clearDatabase() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            DSLContext context = DSL.using(conn, SQLDialect.POSTGRES);
            List<Table<?>> tables = context.meta().getTables();
            for (Table<?> table : tables) {
                context.dropTable(table).cascade().execute();
            }
        }
    } // Clear all tables after each test to ensure clean slate

    private static void clearTriples() {
        kbClient.executeUpdate(BlazegraphContainer.DELETE_ALL_QUERY);
    }

    @Test
    void testInitTimeSeriesWithoutExceptions() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Verify kb is initially empty
            Assertions.assertEquals(0, tsClient.countTimeSeries());

            // Initialise time series (3 dataIRIs, 1 tsIRI) in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.AVERAGE, duration,
                    chronoUnit);

            // Verify correct instantiation in both kb and database
            Assertions.assertEquals(1, tsClient.countTimeSeries());
            Assertions.assertEquals(3, tsClient.getAssociatedData(tsClient.getTimeSeriesIRI(dataIRI_1.get(0))).size());
            TimeSeriesSparql.CustomDuration customDuration = tsClient
                    .getCustomDuration(tsClient.getTimeSeriesIRI(dataIRI_1.get(0)));
            Assertions.assertEquals(customDuration.getUnit(), temporalUnit);
            Assertions.assertEquals(customDuration.getValue(), numericalDuration, epsilon);
            TimeSeries<Instant> ts = tsClient.getTimeSeries(dataIRI_1, conn);
            Assertions.assertEquals(3, ts.getDataIRIs().size());
            for (String iri : dataIRI_1) {
                Assertions.assertTrue(ts.getDataIRIs().contains(iri));
            }
            List<String> kb = ts.getDataIRIs();
            List<String> db = tsClient.getAssociatedData(tsClient.getTimeSeriesIRI(dataIRI_1.get(0)));
            kb.sort(null);
            db.sort(null);
            Assertions.assertEquals(kb, db);
        }
    }

    @Test
    void testInitTimeSeriesWithKGInitException() throws SQLException {
        // Interrupt triple store connection
        blazegraph.stop();
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn,
                            TimeSeriesClient.Type.INSTANTANEOUS, null, null));
            Assertions.assertTrue(e.getMessage().contains("Timeseries was not created!"));
        }
    }

    @Test
    void testInitTimeSeriesWithUnavailableRDB() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Interrupt database connection
            postgres.stop();
            // Initialise time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn,
                            TimeSeriesClient.Type.CUMULATIVETOTAL, null, null));
            Assertions.assertTrue(e.getMessage().contains("Timeseries was not created!"));
        }
    }

    @Test
    void testInitTimeSeriesWithUnavailableRDBAndKGRevertException() throws NoSuchFieldException,
            SecurityException, IllegalArgumentException, IllegalAccessException, SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Retrieve the value of the private field 'rdfClient' of the time series client
            Field RDFClient = tsClient.getClass().getDeclaredField("rdfClient");
            RDFClient.setAccessible(true);
            TimeSeriesSparql rdfClient = (TimeSeriesSparql) RDFClient.get(tsClient);

            // Create a spy object of the real rdfClient and substitute the initial
            // rdfClient with it
            // Spy's behave exactly like normal instances, except for particularly stubbed
            // methods
            TimeSeriesSparql rdfClient_spy = spy(rdfClient);
            RDFClient.set(tsClient, rdfClient_spy);
            // Throw error when removal of time series in KG is intended (after RDB
            // interaction failed) to simulate connection error etc.
            doThrow(new JPSRuntimeException("")).when(rdfClient_spy).removeTimeSeries(Mockito.anyString());

            // Interrupt database connection
            postgres.stop();
            // Initialise time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.GENERAL,
                            null, null));
            Assertions.assertTrue(e.getMessage().contains("Inconsistent state created when initialising time series"));
        }

    }

    @Test
    void testDeleteIndividualTimeSeriesWithoutExceptions() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.GENERAL, null, null);

            // Verify correct instantiation in both kb and database
            Assertions.assertEquals(1, tsClient.countTimeSeries());
            Assertions.assertEquals(dataIRI_1.size(),
                    tsClient.getAssociatedData(tsClient.getTimeSeriesIRI(dataIRI_1.get(0))).size());
            TimeSeries<Instant> ts = tsClient.getTimeSeries(dataIRI_1, conn);
            Assertions.assertEquals(dataIRI_1.size(), ts.getDataIRIs().size());

            // Delete 1st data series - verify deletion and that other data series are still
            // unaltered
            String dataIRI = dataIRI_1.remove(0);
            tsClient.deleteIndividualTimeSeries(dataIRI, conn);
            Assertions.assertEquals(1, tsClient.countTimeSeries());
            Assertions.assertEquals(dataIRI_1.size(),
                    tsClient.getAssociatedData(tsClient.getTimeSeriesIRI(dataIRI_1.get(0))).size());
            Assertions.assertNull(tsClient.getTimeSeriesIRI(dataIRI));
            ts = tsClient.getTimeSeries(dataIRI_1, conn);
            Assertions.assertFalse(ts.getDataIRIs().contains(dataIRI));
            String finalDataIRI = dataIRI;
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteIndividualTimeSeries(finalDataIRI, conn));
            Assertions
                    .assertTrue(e.getMessage().contains("DataIRI " + dataIRI + " not associated with any timeseries."));

            // Delete 2nd data series - verify deletion and that other data series are still
            // unaltered
            dataIRI = dataIRI_1.remove(0);
            tsClient.deleteIndividualTimeSeries(dataIRI, conn);
            Assertions.assertEquals(1, tsClient.countTimeSeries());
            Assertions.assertEquals(dataIRI_1.size(),
                    tsClient.getAssociatedData(tsClient.getTimeSeriesIRI(dataIRI_1.get(0))).size());
            Assertions.assertNull(tsClient.getTimeSeriesIRI(dataIRI));
            ts = tsClient.getTimeSeries(dataIRI_1, conn);
            Assertions.assertFalse(ts.getDataIRIs().contains(dataIRI));
            String finalDataIRI1 = dataIRI;
            e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteIndividualTimeSeries(finalDataIRI1, conn));
            Assertions
                    .assertTrue(e.getMessage().contains("DataIRI " + dataIRI + " not associated with any timeseries."));

            // Delete 3rd data series - verify deletion
            dataIRI = dataIRI_1.remove(0);
            tsClient.deleteIndividualTimeSeries(dataIRI, conn);
            Assertions.assertEquals(0, tsClient.countTimeSeries());
            Assertions.assertNull(tsClient.getTimeSeriesIRI(dataIRI));

            // Exception from executing SQL command with empty dataIRI
            e = Assertions.assertThrows(JPSRuntimeException.class, () -> tsClient.getTimeSeries(dataIRI_1, conn));
            Assertions.assertTrue(e.getMessage().contains("Error while executing SQL command"));
            String finalDataIRI2 = dataIRI;
            e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteIndividualTimeSeries(finalDataIRI2, conn));
            Assertions
                    .assertTrue(e.getMessage().contains("DataIRI " + dataIRI + " not associated with any timeseries."));
        }
    }

    @Test
    void testDeleteIndividualTimeSeriesWithUnavailableKG() throws SQLException {

        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.INSTANTANEOUS, null,
                    null);
            String dataIRI = dataIRI_1.get(0);

            // Interrupt triple store connection
            blazegraph.stop();

            // Delete time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteIndividualTimeSeries(dataIRI, conn));
            Assertions.assertTrue(e.getCause().getMessage().contains("Error occurred during SPARQL query evaluation"));
        }
    }

    @Test
    void testDeleteIndividualTimeSeriesWithKGDeleteException() throws IllegalArgumentException,
            IllegalAccessException, NoSuchFieldException, SecurityException, SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.INSTANTANEOUS, null,
                    null);
            String dataIRI = dataIRI_1.get(0);

            // Retrieve the value of the private field 'rdfClient' of the time series client
            Field RDFClient = tsClient.getClass().getDeclaredField("rdfClient");
            RDFClient.setAccessible(true);
            TimeSeriesSparql rdfClient = (TimeSeriesSparql) RDFClient.get(tsClient);

            // Create a spy object of the real rdfClient and substitute the initial
            // rdfClient with it
            // Spy's behave exactly like normal instances, except for particularly stubbed
            // methods
            TimeSeriesSparql rdfClient_spy = spy(rdfClient);
            RDFClient.set(tsClient, rdfClient_spy);
            // Throw error when removal of time series in KG is intended
            doThrow(new JPSRuntimeException("")).when(rdfClient_spy).removeTimeSeriesAssociation(Mockito.anyString());

            // Delete time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteIndividualTimeSeries(dataIRI, conn));
            Assertions
                    .assertTrue(e.getMessage().contains("Timeseries association for " + dataIRI + " was not deleted!"));

            // Check that knowledge base and database are still consistent
            TimeSeries<Instant> ts = tsClient.getTimeSeries(dataIRI_1, conn);
            List<String> kb = ts.getDataIRIs();
            List<String> db = rdfClient.getAssociatedData(rdfClient.getTimeSeries(dataIRI));
            kb.sort(null);
            db.sort(null);
            Assertions.assertEquals(kb, db);
        }
    }

    @Test
    void testDeleteIndividualTimeSeriesWithUnavailableRDB() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.GENERAL, null, null);

            // Retrieve latest database state and interrupt connection
            TimeSeries<Instant> ts = tsClient.getTimeSeries(dataIRI_1, conn);
            postgres.stop();

            // DataIRI to be deleted
            String dataIRI = dataIRI_1.get(0);
            // Delete time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteIndividualTimeSeries(dataIRI, conn));
            Assertions
                    .assertTrue(e.getMessage().contains("Timeseries association for " + dataIRI + " was not deleted!"));

            // Check that knowledge base and database are still consistent
            List<String> kb = ts.getDataIRIs();
            List<String> db = tsClient.getAssociatedData(tsClient.getTimeSeriesIRI(dataIRI));
            kb.sort(null);
            db.sort(null);
            Assertions.assertEquals(kb, db);
            Assertions.assertEquals(dataIRI_1.size(), kb.size());
        }
    }

    @Test
    void testDeleteIndividualTimeSeriesWithKGRevertException() throws IllegalArgumentException,
            IllegalAccessException, NoSuchFieldException, SecurityException, SQLException {

        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.STEPWISECUMULATIVE,
                    null, null);
            String dataIRI = dataIRI_1.get(0);

            // Retrieve the value of the private field 'rdfClient' of the time series client
            Field RDFClient = tsClient.getClass().getDeclaredField("rdfClient");
            RDFClient.setAccessible(true);
            TimeSeriesSparql rdfClient = (TimeSeriesSparql) RDFClient.get(tsClient);

            // Create a spy object of the real rdfClient and substitute the initial
            // rdfClient with it
            // Spy's behave exactly like normal instances, except for particularly stubbed
            // methods
            TimeSeriesSparql rdfClient_spy = spy(rdfClient);
            RDFClient.set(tsClient, rdfClient_spy);
            // Throw error when removal of time series in KG is intended
            doThrow(new JPSRuntimeException("")).when(rdfClient_spy).insertTimeSeriesAssociation(Mockito.any(),
                    Mockito.any());

            // Interrupt postgreSQL connection
            postgres.stop();

            // Delete time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteIndividualTimeSeries(dataIRI, conn));
            Assertions.assertTrue(e.getMessage()
                    .contains("Inconsistent state created when deleting time series association for " + dataIRI));
        }
    }

    @Test
    void testDeleteTimeSeriesWithoutExceptions() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.CUMULATIVETOTAL, null,
                    null);
            tsClient.initTimeSeries(dataIRI_2, dataClass_2, timeUnit, conn, TimeSeriesClient.Type.INSTANTANEOUS, null,
                    null);

            // Verify correct instantiation in both kb and database
            Assertions.assertEquals(2, tsClient.countTimeSeries());
            TimeSeries<Instant> ts1 = tsClient.getTimeSeries(dataIRI_1, conn);
            Assertions.assertEquals(dataIRI_1.size(), ts1.getDataIRIs().size());
            TimeSeries<Instant> ts2 = tsClient.getTimeSeries(dataIRI_2, conn);
            Assertions.assertEquals(dataIRI_2.size(), ts2.getDataIRIs().size());

            // Delete 1st time series - verify deletion and that 2nd time series is still
            // unaltered
            String tsIRI = tsClient.getTimeSeriesIRI(dataIRI_1.get(0));
            tsClient.deleteTimeSeries(tsIRI, conn);
            Assertions.assertEquals(1, tsClient.countTimeSeries());
            Assertions.assertNull(tsClient.getTimeSeriesIRI(dataIRI_1.get(0)));
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.getTimeSeries(dataIRI_1, conn));
            Assertions.assertTrue(e.getMessage()
                    .contains("<" + dataIRI_1.get(0) + "> does not have an assigned time series instance"));

            TimeSeries<Instant> ts3 = tsClient.getTimeSeries(dataIRI_2, conn);
            Assertions.assertEquals(ts2.getDataIRIs(), ts3.getDataIRIs());

            // Delete 2nd time series - verify deletion and that nothing remains in KG and
            // database
            tsIRI = tsClient.getTimeSeriesIRI(dataIRI_2.get(0));
            tsClient.deleteTimeSeries(tsIRI, conn);
            Assertions.assertEquals(0, tsClient.countTimeSeries());
            Assertions.assertNull(tsClient.getTimeSeriesIRI(dataIRI_2.get(0)));
            e = Assertions.assertThrows(JPSRuntimeException.class, () -> tsClient.getTimeSeries(dataIRI_2, conn));
            Assertions.assertTrue(e.getMessage()
                    .contains("<" + dataIRI_2.get(0) + "> does not have an assigned time series instance"));
        }
    }

    @Test
    void testDeleteTimeSeriesWithUnavailableKG() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.AVERAGE, duration,
                    chronoUnit);
            // Retrieve tsIRI to be deleted
            String tsIRI = tsClient.getTimeSeriesIRI(dataIRI_1.get(0));

            // Interrupt triple store connection
            blazegraph.stop();

            // Delete time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteTimeSeries(tsIRI, conn));
            Assertions.assertTrue(e.getCause().getMessage().contains("Error occurred during SPARQL query evaluation"));
        }
    }

    @Test
    void testDeleteTimeSeriesWithKGDeleteException() throws IllegalArgumentException, IllegalAccessException,
            NoSuchFieldException, SecurityException, SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.CUMULATIVETOTAL, null,
                    null);

            // Retrieve the value of the private field 'rdfClient' of the time series client
            Field RDFClient = tsClient.getClass().getDeclaredField("rdfClient");
            RDFClient.setAccessible(true);
            TimeSeriesSparql rdfClient = (TimeSeriesSparql) RDFClient.get(tsClient);

            // Create a spy object of the real rdfClient and substitute the initial
            // rdfClient with it
            // Spy's behave exactly like normal instances, except for particularly stubbed
            // methods
            TimeSeriesSparql rdfClient_spy = spy(rdfClient);
            RDFClient.set(tsClient, rdfClient_spy);
            // Throw error when removal of time series in KG is intended
            doThrow(new JPSRuntimeException("")).when(rdfClient_spy).removeTimeSeries(Mockito.anyString());

            // Retrieve tsIRI to be deleted
            String tsIRI = rdfClient.getTimeSeries(dataIRI_1.get(0));
            // Delete time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteTimeSeries(tsIRI, conn));
            Assertions.assertTrue(e.getMessage().contains("Timeseries " + tsIRI + " was not deleted!"));

            // Check that knowledge base and database are still consistent
            TimeSeries<Instant> ts = tsClient.getTimeSeries(dataIRI_1, conn);
            List<String> kb = ts.getDataIRIs();
            List<String> db = rdfClient.getAssociatedData(rdfClient.getTimeSeries(dataIRI_1.get(0)));
            kb.sort(null);
            db.sort(null);
            Assertions.assertEquals(kb, db);
        }
    }

    @Test
    void testDeleteTimeSeriesWithUnavailableRDB() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.GENERAL, null, null);

            // Retrieve tsIRI to be deleted
            String tsIRI = tsClient.getTimeSeriesIRI(dataIRI_1.get(0));

            // Retrieve latest database state and interrupt connection
            TimeSeries<Instant> ts = tsClient.getTimeSeries(dataIRI_1, conn);
            postgres.stop();

            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteTimeSeries(tsIRI, conn));
            Assertions.assertTrue(e.getMessage().contains("Timeseries " + tsIRI + " was not deleted!"));

            // Check that knowledge base and database are still consistent
            List<String> kb = ts.getDataIRIs();
            List<String> db = tsClient.getAssociatedData(tsClient.getTimeSeriesIRI(dataIRI_1.get(0)));
            kb.sort(null);
            db.sort(null);
            Assertions.assertEquals(kb, db);
        }
    }

    @Test
    void testDeleteTimeSeriesWithKGRevertException() throws IllegalArgumentException, IllegalAccessException,
            NoSuchFieldException, SecurityException, SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.INSTANTANEOUS, null,
                    null);

            // Retrieve the value of the private field 'rdfClient' of the time series client
            Field RDFClient = tsClient.getClass().getDeclaredField("rdfClient");
            RDFClient.setAccessible(true);
            TimeSeriesSparql rdfClient = (TimeSeriesSparql) RDFClient.get(tsClient);

            // Create a spy object of the real rdfClient and substitute the initial
            // rdfClient with it
            // Spy's behave exactly like normal instances, except for particularly stubbed
            // methods
            TimeSeriesSparql rdfClient_spy = spy(rdfClient);
            RDFClient.set(tsClient, rdfClient_spy);
            // Throw error when removal of time series in KG is intended
            doThrow(new JPSRuntimeException("")).when(rdfClient_spy).reInitTS(Mockito.any(), Mockito.anyString(),
                    Mockito.anyString(), Mockito.any(), Mockito.any());

            // Interrupt postgreSQL connection
            postgres.stop();

            // Retrieve tsIRI to be deleted
            String tsIRI = rdfClient.getTimeSeries(dataIRI_1.get(0));
            // Delete time series in knowledge base and database
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                    () -> tsClient.deleteTimeSeries(tsIRI, conn));
            Assertions.assertTrue(
                    e.getMessage().contains("Inconsistent state created when deleting time series " + tsIRI));
        }
    }

    @Test
    void testDeleteAllWithoutExceptions() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.AVERAGE, duration,
                    chronoUnit);
            tsClient.initTimeSeries(dataIRI_2, dataClass_2, timeUnit, conn, TimeSeriesClient.Type.INSTANTANEOUS, null,
                    null);

            // Verify correct instantiation in both kb and database
            Assertions.assertEquals(2, tsClient.countTimeSeries());
            TimeSeries<Instant> ts1 = tsClient.getTimeSeries(dataIRI_1, conn);
            Assertions.assertEquals(dataIRI_1.size(), ts1.getDataIRIs().size());
            TimeSeries<Instant> ts2 = tsClient.getTimeSeries(dataIRI_2, conn);
            Assertions.assertEquals(dataIRI_2.size(), ts2.getDataIRIs().size());

            // Delete all data in database and knowledge base
            tsClient.deleteAll(conn);

            // Verify correct deletion
            Assertions.assertEquals(0, tsClient.countTimeSeries());
            List<List<String>> series = Arrays.asList(dataIRI_1, dataIRI_2);
            for (List<String> s : series) {
                JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class,
                        () -> tsClient.getTimeSeries(s, conn));
                Assertions.assertTrue(e.getMessage().contains("Central RDB lookup table has not been initialised yet"));
            }
        }
    }

    @Test
    void testDeleteAllWithExceptions() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            // Initialise time series in knowledge base and database
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn, TimeSeriesClient.Type.AVERAGE, duration,
                    chronoUnit);
            tsClient.initTimeSeries(dataIRI_2, dataClass_2, timeUnit, conn, TimeSeriesClient.Type.GENERAL, null, null);

            // Interrupt database connection
            postgres.stop();

            // Delete all data in database and knowledge base
            JPSRuntimeException e = Assertions.assertThrows(JPSRuntimeException.class, () -> tsClient.deleteAll(conn));
            Assertions.assertTrue(e.getMessage().contains("Not all timeseries were deleted from database!"));

            // Interrupt triple store connection
            blazegraph.stop();

            // Delete all data in database and knowledge base
            e = Assertions.assertThrows(JPSRuntimeException.class, () -> tsClient.deleteAll(conn));
            Assertions.assertTrue(e.getMessage().contains("Not all timeseries were deleted from KG!"));
        }
    }

    @Test
    void testBulkInitTimeSeries() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            List<List<String>> dataIRIs = new ArrayList<>();
            dataIRIs.add(dataIRI_1);
            dataIRIs.add(dataIRI_2);

            List<List<Class<?>>> classes = new ArrayList<>();
            classes.add(dataClass_1);
            classes.add(dataClass_2);

            List<String> units = new ArrayList<>();
            units.add(timeUnit);
            units.add(timeUnit);

            tsClient.bulkInitTimeSeries(dataIRIs, classes, units, conn);

            // Verify correct instantiation in both kb and database
            Assertions.assertEquals(2, tsClient.countTimeSeries());
            TimeSeries<Instant> ts1 = tsClient.getTimeSeries(dataIRI_1, conn);
            Assertions.assertEquals(dataIRI_1.size(), ts1.getDataIRIs().size());
            TimeSeries<Instant> ts2 = tsClient.getTimeSeries(dataIRI_2, conn);
            Assertions.assertEquals(dataIRI_2.size(), ts2.getDataIRIs().size());
        }
    }

    @Test
    void testAddNewColumns() throws SQLException {
        try (Connection conn = rdbStoreClient.getConnection()) {
            tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit, conn);

            // if there are no errors it can be assumed it is initialised correctly
            tsClient.getTimeSeries(dataIRI_1, conn);

            TimeSeriesSparql timeSeriesSparql = new TimeSeriesSparql(kbClient);
            String tsIri = timeSeriesSparql.getTimeSeries(dataIRI_1.get(0));

            tsClient.addColumnsToExistingTimeSeries(tsIri, dataIRI_2, dataClass_2, null, conn);

            List<String> combinedList = new ArrayList<>(dataIRI_1);
            combinedList.addAll(dataIRI_2);
            tsClient.getTimeSeries(combinedList, conn);
        }
    }
}
