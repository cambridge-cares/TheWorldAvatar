package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.sql.SQLException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

/**
 * This integration test targets the DateTSClientWrapper class and its public methods that interact with an external Knowledge graph and Relational database.
 * Will not run when building a new Docker image, as it does not have access to the host Docker daemon.
 */
@Testcontainers
public class DateTSClientDecoratorIntegrationT {
    @Container
    PostgreSQLContainer postgres = new PostgreSQLContainer("postgres:13-alpine");
    @Container
    GenericContainer blazegraph = new GenericContainer(DockerImageName.parse("nawer/blazegraph:latest"))
            .withExposedPorts(9999);
    private TimeSeriesClient<Instant> tsClient;
    private static DateTSClientDecorator testDecorator;
    private static RemoteStoreClient kbClient;
    // Lists of test values
    private static List<Double> measureValues;
    private static List<Double> ratesValues;
    private static List<LocalDateTime> testDates;
    // Maps of test data
    private static Map<String, List<?>> testReadings;
    private static Map<String, String> testMappings;
    private static List<String> testIRIs;
    private static final String testPrefix = "example:prefix/api_";
    private static final String RDF_PREFIX = "rdf";
    private static final String RDF_URI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    private static final String RDF_TYPE = RDF_PREFIX + ":type";
    private static final String TIMESERIES_KEY = "timeseries";
    private static final String TIMESERIES_VAR = "?" + TIMESERIES_KEY;
    private static final String TIMESERIES_CLASS = "<https://www.theworldavatar.com/kg/ontotimeseries/StepwiseCumulativeTimeSeries>";
    private final String instantTimeZone = "T00:00:00Z"; // UTC Time zone

    @BeforeAll
    static void genTestData() {
        // Generate test readings that can be extracted from Excel
        testReadings = new HashMap<>();
        // Excel Date data is retrieved as LocalDateTime if dates are not split into multiple columns
        testDates = Arrays.asList(
                LocalDateTime.of(2022, 4, 14, 0, 0, 0, 0),
                LocalDateTime.of(2022, 5, 14, 0, 0, 0, 0),
                LocalDateTime.of(2022, 6, 14, 0, 0, 0, 0)
        );
        measureValues = new ArrayList<>(Arrays.asList(10.0, 15.0, 20.0));
        ratesValues = new ArrayList<>(Arrays.asList(50.0, 100.0, 150.0));
        testReadings.put("dates", testDates);
        testReadings.put("measures", measureValues);
        testReadings.put("rates", ratesValues);

        // Generate the mappings between the heading name and their dataIRIs
        testIRIs = new ArrayList<>(Arrays.asList(testPrefix + "measures_4012", testPrefix + "rates_5910"));
        testMappings = new HashMap<>();
        testMappings.put("measures", testIRIs.get(0));
        testMappings.put("rates", testIRIs.get(1));
    }

    @BeforeEach
    void initTSClientEndpoints() {
        // Get host and port name to form KG endpoint
        String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql"; // Default namespace is "kb"
        kbClient = new RemoteStoreClient(endpoint, endpoint);

        // Initialise TimeSeriesClient with pre-configured kb client
        tsClient = new TimeSeriesClient<>(kbClient, Instant.class);

        // Set TSClient for the targeted test class
        testDecorator = new DateTSClientDecorator("dates");
        testDecorator.setTsClient(tsClient);
        testDecorator.setRDBClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
    }

    @Test
    void testInitializeTimeSeries() throws SQLException {
        testDecorator.initializeTimeSeries(testReadings, testMappings);
        // Check that there is only one time series added
        assertEquals(1, tsClient.countTimeSeries());
        // Check that all IRIs have a time series and are attached to the same time series
        String testTSIRI;
        for (String iri : testIRIs) {
            assertTrue(tsClient.checkDataHasTimeSeries(iri, testDecorator.getRDBClient().getConnection()));
            testTSIRI = tsClient.getTimeSeriesIRI(iri);
            assertEquals(testTSIRI, tsClient.getTimeSeriesIRI(iri));
        }
    }

    @Test
    void testInitializeTimeSeriesWithExistingTimeSeries() throws SQLException {
        // Create spy to verify executions on the time series client
        TimeSeriesClient<Instant> tsClientSpy = Mockito.spy(tsClient);
        testDecorator.setTsClient(tsClientSpy);
        // Run code twice but should only be initialized once
        testDecorator.initializeTimeSeries(testReadings, testMappings);
        testDecorator.initializeTimeSeries(testReadings, testMappings);
        assertEquals(1, tsClient.countTimeSeries());
        Mockito.verify(tsClientSpy, Mockito.times(1)).
                initTimeSeries(Mockito.anyList(), Mockito.anyList(), Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    }

    @Test
    void testUpdateDataForEmptyDatabase() throws SQLException {
        testDecorator.initializeTimeSeries(testReadings, testMappings);
        testDecorator.updateData(testReadings, testMappings);

        TimeSeries<Instant> postgresTSData = tsClient.getTimeSeries(testIRIs, testDecorator.getRDBClient().getConnection());
        assertAll(
                () -> assertEquals(testReadings.get("dates").size(), postgresTSData.getTimes().size()),
                // Test data will be returned in LocalDate as time series client is set to LocalDate
                () -> assertEquals(Instant.parse("2022-04-14"+instantTimeZone), tsClient.getMinTime(testIRIs.get(0), testDecorator.getRDBClient().getConnection())),
                () -> assertEquals(Instant.parse("2022-06-14"+instantTimeZone), tsClient.getMaxTime(testIRIs.get(0), testDecorator.getRDBClient().getConnection())),
                // Verify test data is accurate
                () -> assertEquals(measureValues, postgresTSData.getValues(testIRIs.get(0))),
                () -> assertEquals(ratesValues, postgresTSData.getValues(testIRIs.get(1)))
        );
    }

    @Test
    void testUpdateDataForPrecedingDatabase() throws SQLException {
        testDecorator.initializeTimeSeries(testReadings, testMappings);
        testDecorator.updateData(testReadings, testMappings);

        // Extract a sub Map of test data
        List<String> keys = new ArrayList<>(Arrays.asList("dates", "rates"));
        Map<String, List<?>> updatedReadings = keys.stream()
                .filter(testReadings::containsKey)
                .collect(Collectors.toMap(Function.identity(), testReadings::get));
        // Add updated data to the map
        List<Double> updatedMeasureValues = new ArrayList<>(Arrays.asList(12.0, 16.0, 22.0));
        updatedReadings.put("measures", updatedMeasureValues);
        testDecorator.updateData(updatedReadings, testMappings);

        // Test if data has been updated to new values and old values are no longer present
        TimeSeries<Instant> postgresTSData = tsClient.getTimeSeries(testIRIs, testDecorator.getRDBClient().getConnection());
        assertAll(
                () -> assertEquals(updatedReadings.get("dates").size(), postgresTSData.getTimes().size()),
                () -> assertEquals(Instant.parse("2022-04-14"+instantTimeZone), tsClient.getMinTime(testIRIs.get(0), testDecorator.getRDBClient().getConnection())),
                () -> assertEquals(Instant.parse("2022-06-14"+instantTimeZone), tsClient.getMaxTime(testIRIs.get(0), testDecorator.getRDBClient().getConnection())),
                () -> assertNotEquals(measureValues, postgresTSData.getValues(testIRIs.get(0))),
                () -> assertEquals(updatedMeasureValues, postgresTSData.getValues(testIRIs.get(0)))
        );
    }
}
