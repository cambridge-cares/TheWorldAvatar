package uk.ac.cam.cares.jps.base.timeseries;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.*;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;

/**
 * This class provides integration tests for the TimeSeriesSparql class
 */

// @Ignore("Requires triple store endpoint set up and running (using
// testcontainers)\n" +
// "Requires Docker to run the tests. When on Windows, WSL2 as backend is
// required to ensure proper execution")
@Testcontainers
public class TimeSeriesSparqlIntegrationTest {

    private static TimeSeriesSparql sparqlClient;
    private final double epsilon = 0.000001d;

    // Will create a container that is shared between tests.
    // For more information regarding the registry, see:
    // https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private static final GenericContainer<?> blazegraph = new GenericContainer<>(
            DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);

    @BeforeClass
    public static void initialiseSparqlClient() {

        // Start the container manually
        blazegraph.start();

        // Set up a kb client that points to the location of the triple store
        // This can be a RemoteStoreClient or the FileBasedStoreClient
        RemoteStoreClient kbClient = new RemoteStoreClient();
        // Set endpoint to the triple store. The host and port are read from the
        // container
        String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb", but in production a specific one
        // should be created
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";
        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQueryEndpoint(endpoint);
        // Initialise TimeSeriesSparql client with kb client
        sparqlClient = new TimeSeriesSparql(kbClient);
    }

    @After
    public void clearKb() {
        // Clear entire knowledge base
        sparqlClient.removeAllTimeSeries();
    }

    @Test
    public void runIntegrationTest() {

        // IRIs for 3 times series:
        // 1. First of type Average with 3 associated data series
        // 2. Second of type Average with only 1 associated data series
        // 3. Third of type Cumulative Total with 2 associated data series
        String tsIRI1 = "http://tsIRI1";
        List<String> dataIRI1 = Arrays.asList("http://data1", "http://data2", "http://data3");
        Duration duration = Duration.ofDays(33 * 5);
        ChronoUnit chronoUnit = ChronoUnit.MONTHS;
        Double numericalDuration = 5.0;
        String temporalUnit = TimeSeriesSparql.NS_TIME + "unitMonth";

        String tsIRI2 = "http://tsIRI2";
        List<String> dataIRI2 = Collections.singletonList("http://data4");

        String tsIRI3 = "http://tsIRI3";
        List<String> dataIRI3 = Arrays.asList("http://data5", "http://data6");

        String dbURL = "jdbc:postgresql:timeseries";
        String schema = "public";
        String timeUnit = "s";

        Class<?> timeClass = Double.class;
        Class<?> rdbClientClass = TimeSeriesRDBClient.class;

        TimeSeriesKgMetadata timeSeriesKgMetadata = new TimeSeriesKgMetadata(tsIRI1);
        timeSeriesKgMetadata.setDataIriList(dataIRI1);
        timeSeriesKgMetadata.setTimeUnit(timeUnit);
        timeSeriesKgMetadata.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata.setDuration(duration);
        timeSeriesKgMetadata.setDurationUnit(chronoUnit);

        // Initialise time series in kb
        sparqlClient.initTS(timeSeriesKgMetadata, dbURL, schema, timeClass, rdbClientClass);
        // Test number of initialised time series in kb
        Assert.assertEquals(1, sparqlClient.countTS());
        // Test whether all data IRI have the time series attached
        for (String iri : dataIRI1) {
            Assert.assertEquals(tsIRI1, sparqlClient.getTimeSeries(iri));
        }
        // Retrieve all data IRIs for the time series
        List<String> dataIRIs = sparqlClient.getAssociatedData(tsIRI1);
        for (String iri : dataIRI1) {
            Assert.assertTrue(dataIRIs.contains(iri));
        }

        // Retrieve time series properties
        Assert.assertEquals(dbURL, sparqlClient.getDbUrl(tsIRI1));
        Assert.assertEquals(timeUnit, sparqlClient.getTimeUnit(tsIRI1));
        Assert.assertNotNull(sparqlClient.getAveragingPeriod(tsIRI1));
        TimeSeriesSparql.CustomDuration customDuration = sparqlClient.getCustomDuration(tsIRI1);
        Assert.assertEquals(numericalDuration, customDuration.getValue(), epsilon);
        Assert.assertEquals(temporalUnit, customDuration.getUnit());

        // Initialise another average time series with same duration and temporal unit
        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata2.setDuration(duration);
        timeSeriesKgMetadata2.setDurationUnit(chronoUnit);

        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertEquals(sparqlClient.getAveragingPeriod(tsIRI1), sparqlClient.getAveragingPeriod(tsIRI2));

        // Initialise another time series without time unit
        TimeSeriesKgMetadata timeSeriesKgMetadata3 = new TimeSeriesKgMetadata(tsIRI3);
        timeSeriesKgMetadata3.setDataIriList(dataIRI3);
        timeSeriesKgMetadata3.setTimeUnit(null);
        timeSeriesKgMetadata3.setTimeSeriesType(Type.CUMULATIVETOTAL);

        sparqlClient.initTS(timeSeriesKgMetadata3, dbURL, schema, timeClass, rdbClientClass);
        Assert.assertNull(sparqlClient.getTimeUnit(tsIRI3));

        // Remove the attachment of one data IRI to a time series
        sparqlClient.removeTimeSeriesAssociation(dataIRI1.get(0));
        // Try to retrieve data IRI association
        Assert.assertNull(sparqlClient.getTimeSeries(dataIRI1.get(0)));

        // Remove one average time series
        sparqlClient.removeTimeSeries(sparqlClient.getTimeSeries(dataIRI1.get(1)));
        Assert.assertNotNull(sparqlClient.getAveragingPeriod(tsIRI2));

        // Retrieve all time series remaining
        List<String> timeSeries = sparqlClient.getAllTimeSeries();
        Assert.assertEquals(2, timeSeries.size());
        Assert.assertTrue(timeSeries.contains(tsIRI2));
        Assert.assertTrue(timeSeries.contains(tsIRI3));

    }
}
