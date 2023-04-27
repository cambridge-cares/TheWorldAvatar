package com.cmclinnovations.featureinfo.kg;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;

import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.EndpointType;
import com.cmclinnovations.featureinfo.config.NamespaceGetterTest;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Tests for the TimeHandler class.
 */
public class TimeHandlerTest {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(TimeHandlerTest.class);

    /**
     * Test config store.
     */
    private static final ConfigStore CONFIG = new ConfigStore();

    /**
     * Read in mock config file before running tests.
     */
    @BeforeAll
    public static void setup() {
        try (InputStream is = NamespaceGetterTest.class.getResourceAsStream("/mock-config-file.json")) {
            BufferedReader bufferReader = new BufferedReader(new InputStreamReader(is));
            StringBuilder stringBuilder = new StringBuilder();

            String eachStringLine;
            while ((eachStringLine = bufferReader.readLine()) != null) {
                stringBuilder.append(eachStringLine).append("\n");
            }

            CONFIG.loadFile(stringBuilder.toString());
            FeatureInfoAgent.CONFIG = CONFIG;

        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not read mock config file!");
        }

        // Add mock endpoints to the config
        CONFIG.addEndpoint(new ConfigEndpoint("ONTOP", "http://my-fake-ontop.com/", null, null, EndpointType.ONTOP));
        CONFIG.addEndpoint(new ConfigEndpoint("POSTGRES", "http://my-fake-postgres.com/", null, null, EndpointType.POSTGRES));
        CONFIG.addEndpoint(new ConfigEndpoint("blazegraph-test", "http://fake-website.com/blazegraph/namespace/test/sparql", null, null, EndpointType.BLAZEGRAPH));

        // Write a temporary query file
        try {
            String tmpdir = System.getProperty("java.io.tmpdir");
            Path tmpQuery = Paths.get(tmpdir, "TimeHandlerTest.sparql");
            Files.writeString(tmpQuery, "SAMPLE-QUERY");

            // Add to the config
            CONFIG.addTimeQueryForClass("SAMPLE-CLASS", tmpQuery.toString());
            CONFIG.addTimeLimitForClass("SAMPLE-CLASS", 72);
            LOGGER.info("Written temporary query file for testing (" + tmpQuery.toString() + ").");
        } catch(IOException exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not write temporary query file!");
        }
    }

    /**
     * Clean up after all tests have executed.
     */
    @AfterAll
    public static void cleanup() {
          // Delete the temporary query file
          try {
            String tmpdir = System.getProperty("java.io.tmpdir");
            Path tmpQuery = Paths.get(tmpdir, "MetaHandlerTest.sparql");
            Files.deleteIfExists(tmpQuery);

            LOGGER.info("Removed temporary query file (" + tmpQuery.toString() + ").");
        } catch(IOException exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not delete temporary query file!");
        }
    }

    /**
     * Tests that a query can be submitted and the response parsed.
     */
    @Test
    @SuppressWarnings("unchecked")
    public void testQuery() {
        // Create a handler
        TimeHandler handler = new TimeHandler(
            "http://fake-sample-iri.com", 
            "SAMPLE-CLASS",
            CONFIG.getBlazegraphEndpoints()
        );

        try {
            // Create mock KG client
            RemoteStoreClient rsClient = mock(RemoteStoreClient.class);
            when(rsClient.executeQuery(
                ArgumentMatchers.anyString()))
                .thenReturn(
                    new org.json.JSONArray("[{\"Measurement\":\"http://fake-measurement-iri.com\",\"Name\":\"MeasurementOne\",\"Unit\":\"m/s\",\"PropertyOne\":\"ValueOne\",\"PropertyTwo\":\"ValueTwo\"}]")
                );

            // Mock RDB client
            RemoteRDBStoreClient rdbClient = mock(RemoteRDBStoreClient.class);
            when(rdbClient.getConnection())
            .thenReturn(null);

            // Create mock Timeseries client
            TimeSeriesClient<Instant> tsClient = mock(TimeSeriesClient.class);

            when(tsClient.convertToJSON(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.anyList(), 
                ArgumentMatchers.anyList(),
                ArgumentMatchers.anyList())
                ).thenCallRealMethod();

            when(tsClient.getTimeSeries(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.isNull()))
                .thenReturn(
                    new TimeSeries<Instant>(
                        Arrays.asList(Instant.MIN, Instant.EPOCH, Instant.MAX),
                        Arrays.asList("http://fake-measurement-iri.com"),
                        Arrays.asList(Arrays.asList("1.0", "2.0", "3.0"))
                    )
                );

            when(tsClient.getTimeSeriesWithinBounds(
                ArgumentMatchers.any(),
                ArgumentMatchers.any(),
                ArgumentMatchers.any(),
                ArgumentMatchers.any()))
                .thenReturn(
                    new TimeSeries<Instant>(
                        Arrays.asList(Instant.MIN, Instant.EPOCH, Instant.MAX),
                        Arrays.asList("http://fake-measurement-iri.com"),
                        Arrays.asList(Arrays.asList("1.0", "2.0", "3.0"))
                    )
                );

            // Set up a mock response
            HttpServletResponse httpResponse = mock(HttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(httpResponse.getWriter()).thenReturn(printWriter);

            // Handler setup
            handler.setClients(rsClient, rdbClient, tsClient);

            // Get the resulting JSON object
            JSONArray result = handler.getData(httpResponse);
            Assertions.assertNotNull(result, "Expected a non-null result!");

            // Build the expected response
            JSONArray expected = new JSONArray("""
                [{\"data\":[\"MeasurementOne\"],\"values\":[[\"1.0\",\"2.0\",\"3.0\"]],\"timeClass\":\"Instant\",\"valuesClass\":[\"String\"],
                \"id\":0,\"units\":[\"m/s\"],\"time\":[\"-1000000000-01-01T00:00:00Z\",\"1970-01-01T00:00:00Z\",
                \"+1000000000-12-31T23:59:59.999999999Z\"],\"properties\":[{\"PropertyTwo\":\"ValueTwo\",\"PropertyOne\":\"ValueOne\"}]}]
            """);

            // Compare
            Assertions.assertTrue(result.similar(expected), "Resulting JSON does not match expected result!");

        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            Assertions.fail("Unexpected exception thrown when trying to get timeseries data!");
        }
    }

    /**
     * Tests that multiple measurements, with different time values, can be converted to JSON and
     * correctly returned. This has been added as previous versions of the code contained a bug
     * that would enforce the same time values for all measurements returned in a single query.
     */
    @Test
    public void testDifferentTimings() {
        // Create a handler
        TimeHandler handler = new TimeHandler(
            "http://fake-sample-iri.com", 
            "SAMPLE-CLASS",
            CONFIG.getBlazegraphEndpoints()
        );

        try {
            // Create mock KG client
            RemoteStoreClient rsClient = mock(RemoteStoreClient.class);
            when(rsClient.executeQuery(
                ArgumentMatchers.anyString()))
                .thenReturn(
                    new org.json.JSONArray("""
                        [{\"Measurement\":\"http://fake-measurement-iri.com/one\",\"Name\":\"MeasurementOne\",\"Unit\":\"m/s\"},{\"Measurement\":\"http://fake-measurement-iri.com/two\",\"Name\":\"MeasurementTwo\",\"Unit\":\"cm/s\"}]
                    """)
                );

            // Mock RDB connection
            RemoteRDBStoreClient rdbClient = mock(RemoteRDBStoreClient.class);
            when(rdbClient.getConnection())
            .thenReturn(null);

            // Create mock Timeseries client
            TimeSeriesClient<Instant> tsClient = mock(TimeSeriesClient.class);

            when(tsClient.convertToJSON(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.anyList(), 
                ArgumentMatchers.anyList(),
                ArgumentMatchers.anyList())
                ).thenCallRealMethod();

            // Mock response for first IRI
            when(tsClient.getTimeSeriesWithinBounds(
                ArgumentMatchers.argThat((ArrayList<String> arg) -> arg != null && arg.contains("http://fake-measurement-iri.com/one")),
                ArgumentMatchers.any(),
                ArgumentMatchers.any(),
                ArgumentMatchers.isNull()))
                .thenReturn(
                    new TimeSeries<Instant>(
                        Arrays.asList(
                            Instant.parse("1970-01-01T12:00:00Z"),
                            Instant.parse("1970-01-01T13:00:00Z"),
                            Instant.parse("1970-01-01T14:00:00Z")
                        ),
                        Arrays.asList("http://fake-measurement-iri.com/one"),
                        Arrays.asList(Arrays.asList("1.0", "2.0", "3.0"))
                    )
                );

            // Mock response for second IRI
            when(tsClient.getTimeSeriesWithinBounds(
                ArgumentMatchers.argThat((ArrayList<String> arg) -> arg != null && arg.contains("http://fake-measurement-iri.com/two")),
                ArgumentMatchers.any(),
                ArgumentMatchers.any(),
                ArgumentMatchers.isNull()))
                .thenReturn(
                    new TimeSeries<Instant>(
                        Arrays.asList(
                            Instant.parse("1971-01-01T15:00:00Z"),
                            Instant.parse("1971-01-01T16:00:00Z"),
                            Instant.parse("1971-01-01T17:00:00Z")
                        ),
                        Arrays.asList("http://fake-measurement-iri.com/two"),
                        Arrays.asList(Arrays.asList("4.0", "5.0", "6.0"))
                    )
                );

            // Set up a mock response
            HttpServletResponse httpResponse = mock(HttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(httpResponse.getWriter()).thenReturn(printWriter);

            // Handler setup
            handler.setClients(rsClient, rdbClient, tsClient);

            // Get the resulting JSON object
            JSONArray result = handler.getData(httpResponse);
            Assertions.assertNotNull(result, "Expected a non-null result!");

             // Build the expected response
             JSONArray expected = new JSONArray("""
                [{\"data\":[\"MeasurementOne\"],\"values\":[[\"1.0\",\"2.0\",\"3.0\"]],\"timeClass\":\"Instant\",\"valuesClass\":[\"String\"],\"id\":0,\"units\":[\"m/s\"],\"time\":
                [\"1970-01-01T12:00:00Z\",\"1970-01-01T13:00:00Z\",\"1970-01-01T14:00:00Z\"],\"properties\":[{},{}]},{\"data\":[\"MeasurementTwo\"],\"values\":[[\"4.0\",\"5.0\",\"6.0\"]],
                \"timeClass\":\"Instant\",\"valuesClass\":[\"String\"],\"id\":1,\"units\":[\"cm/s\"],\"time\":[\"1971-01-01T15:00:00Z\",\"1971-01-01T16:00:00Z\",\"1971-01-01T17:00:00Z\"]}]
            """);

            // Compare
            Assertions.assertTrue(result.similar(expected), "Resulting JSON does not match expected result!");


        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            Assertions.fail("Unexpected exception thrown when trying to get timeseries data!");
        }
    }

}
// End of class.