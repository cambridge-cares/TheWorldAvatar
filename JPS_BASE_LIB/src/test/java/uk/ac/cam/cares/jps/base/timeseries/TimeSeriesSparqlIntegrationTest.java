package uk.ac.cam.cares.jps.base.timeseries;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import org.testcontainers.junit.jupiter.Testcontainers;
import uk.ac.cam.cares.jps.base.BlazegraphContainer;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;

/**
 * This class provides integration tests for the TimeSeriesSparql class
 */

// @Disabled("Requires triple store endpoint set up and running (using
// testcontainers)\n" +
// "Requires Docker to run the tests. When on Windows, WSL2 as backend is
// required to ensure proper execution")
@Testcontainers
class TimeSeriesSparqlIntegrationTest {
    private static TimeSeriesSparql sparqlClient;
    private static final double epsilon = 0.000001d;

    // Will create a container that is shared between tests.
    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();

    @BeforeAll
    public static void initialiseSparqlClient() {
        blazegraph.start();
        RemoteStoreClient kbClient = blazegraph.getRemoteStoreClient();
        // Initialise TimeSeriesSparql client with kb client
        sparqlClient = new TimeSeriesSparql(kbClient);
    }

    @AfterEach
    public void clearKb() {
        // Clear entire knowledge base
        sparqlClient.removeAllTimeSeries();
    }

    @Test
    void runIntegrationTest() {
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
        Assertions.assertEquals(1, sparqlClient.countTS());
        // Test whether all data IRI have the time series attached
        for (String iri : dataIRI1) {
            Assertions.assertEquals(tsIRI1, sparqlClient.getTimeSeries(iri));
        }
        // Retrieve all data IRIs for the time series
        List<String> dataIRIs = sparqlClient.getAssociatedData(tsIRI1);
        for (String iri : dataIRI1) {
            Assertions.assertTrue(dataIRIs.contains(iri));
        }

        // Retrieve time series properties
        Assertions.assertEquals(dbURL, sparqlClient.getDbUrl(tsIRI1));
        Assertions.assertEquals(timeUnit, sparqlClient.getTimeUnit(tsIRI1));
        Assertions.assertNotNull(sparqlClient.getAveragingPeriod(tsIRI1));
        TimeSeriesSparql.CustomDuration customDuration = sparqlClient.getCustomDuration(tsIRI1);
        Assertions.assertEquals(numericalDuration, customDuration.getValue(), epsilon);
        Assertions.assertEquals(temporalUnit, customDuration.getUnit());

        // Initialise another average time series with same duration and temporal unit
        TimeSeriesKgMetadata timeSeriesKgMetadata2 = new TimeSeriesKgMetadata(tsIRI2);
        timeSeriesKgMetadata2.setDataIriList(dataIRI2);
        timeSeriesKgMetadata2.setTimeUnit(timeUnit);
        timeSeriesKgMetadata2.setTimeSeriesType(Type.AVERAGE);
        timeSeriesKgMetadata2.setDuration(duration);
        timeSeriesKgMetadata2.setDurationUnit(chronoUnit);

        sparqlClient.initTS(timeSeriesKgMetadata2, dbURL, schema, timeClass, rdbClientClass);
        Assertions.assertEquals(sparqlClient.getAveragingPeriod(tsIRI1), sparqlClient.getAveragingPeriod(tsIRI2));

        // Initialise another time series without time unit
        TimeSeriesKgMetadata timeSeriesKgMetadata3 = new TimeSeriesKgMetadata(tsIRI3);
        timeSeriesKgMetadata3.setDataIriList(dataIRI3);
        timeSeriesKgMetadata3.setTimeUnit(null);
        timeSeriesKgMetadata3.setTimeSeriesType(Type.CUMULATIVETOTAL);

        sparqlClient.initTS(timeSeriesKgMetadata3, dbURL, schema, timeClass, rdbClientClass);
        Assertions.assertNull(sparqlClient.getTimeUnit(tsIRI3));

        // Remove the attachment of one data IRI to a time series
        sparqlClient.removeTimeSeriesAssociation(dataIRI1.get(0));
        // Try to retrieve data IRI association
        Assertions.assertNull(sparqlClient.getTimeSeries(dataIRI1.get(0)));

        // Remove one average time series
        sparqlClient.removeTimeSeries(sparqlClient.getTimeSeries(dataIRI1.get(1)));
        Assertions.assertNotNull(sparqlClient.getAveragingPeriod(tsIRI2));

        // Retrieve all time series remaining
        List<String> timeSeries = sparqlClient.getAllTimeSeries();
        Assertions.assertEquals(2, timeSeries.size());
        Assertions.assertTrue(timeSeries.contains(tsIRI2));
        Assertions.assertTrue(timeSeries.contains(tsIRI3));
    }
}
