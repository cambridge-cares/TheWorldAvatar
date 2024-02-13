package com.cmclinnovations.featureinfo.core.time;

import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import com.cmclinnovations.featureinfo.TestUtils;
import com.cmclinnovations.featureinfo.config.ConfigEntry;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.ConfigStoreTest;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Tests for the TimeHandler class.
 */
public class TimeHandlerTest {
    
    /**
     * Temporary directory to store test data.
     */
    private static final Path TEMP_DIR = Paths.get(System.getProperty("java.io.tmpdir"));

    /**
     * Copy test data out from the resources directory so it can be loaded in the same
     * manner that files are at runtime.
     * 
     * @throws IOException if temporary test data cannot be created.
     */
    @BeforeAll
    public static void setup() throws IOException {
        FileUtils.deleteDirectory(TEMP_DIR.resolve("mock-config-01").toFile());

        // Copy out test data sets to the temporary directory.
        File mockDir01 = new File(ConfigStoreTest.class.getResource("/mock-config-01").getFile());
        Assertions.assertTrue(
            TestUtils.copyFilesRecusively(mockDir01, TEMP_DIR.toFile()),
            "Could not export test data from within JAR to temporary directory!"
        );
    }

    /**
     * Clean up after tests finish.
     */
    @AfterAll
    public static void cleanUp() throws Exception {
         FileUtils.deleteDirectory(TEMP_DIR.resolve("mock-config-01").toFile());
    }

    /**
     * Tests that time series can be discovered, parsed, and formatted using
     * a single class match with a time query that returns measurable and parent
     * time series IRIs.
     * 
     * @throws Exception if connections to KG or RDB fail.
     */
    @Test
    public void testSingleMatchWithTimeSeries() throws Exception {
        Path configFile = TEMP_DIR.resolve("mock-config-01/config.json");

        // Mock a config store based on the real config file
        ConfigStore configStore = TestUtils.mockConfig(configFile);

        // Initialise a TimeHandler instance
        TimeHandler timeHandler = new TimeHandler(
            "https://test-stack/features/feature-one",
            Optional.empty(),
            configStore
        );

        // Need to return a null connection otherwise it'll fail to connect
        // to a non-existant database
        TimeHandler spiedHandler = spy(timeHandler);
        doReturn(null).when(spiedHandler).connectToDatabase(
            ArgumentMatchers.any(),
            ArgumentMatchers.any()
        );

        // Set clients for communication with KG and RDB
        spiedHandler.setClients(
            mockKGClient(true),
            mockTSClient(),
            mockRDBClient()
        );

        // Attempt to get results
        JSONArray result = spiedHandler.getData(
            configStore.getConfigEntries().subList(1, 2),
            TestUtils.mockResponse()
        );

        JSONArray expected = new JSONArray(
            """
                [
                    {
                        "data": ["Measurement One"],
                        "values": [
                            [1, 2, 3]
                        ],
                        "timeClass": "Instant",
                        "valuesClass": ["Number"],
                        "units": ["mph"],
                        "id": "1",
                        "time": [
                            "1970-01-01T12:00:00Z",
                            "1970-01-01T13:00:00Z",
                            "1970-01-01T14:00:00Z"
                        ]
                    }
                ]    
            """
        );

        Assertions.assertTrue(expected.similar(result), "Returned JSONArray did not match expected one!");
    }

    /**
     * Tests that time series can be discovered, parsed, and formatted using
     * a single class match with a time query that does NOT return measurable
     * with their parent time series IRIs.
     * 
     * @throws Exception if connections to KG or RDB fail.
     */
    @Test
    public void testSingleMatch() throws Exception {
        Path configFile = TEMP_DIR.resolve("mock-config-01/config.json");

        // Mock a config store based on the real config file
        ConfigStore configStore = TestUtils.mockConfig(configFile);

        // Initialise a TimeHandler instance
        TimeHandler timeHandler = new TimeHandler(
            "https://test-stack/features/feature-one",
            Optional.empty(),
            configStore
        );

        // Need to return a null connection otherwise it'll fail to connect
        // to a non-existant database
        TimeHandler spiedHandler = spy(timeHandler);
        doReturn(null).when(spiedHandler).connectToDatabase(
            ArgumentMatchers.any(),
            ArgumentMatchers.any()
        );

        // Set clients for communication with KG and RDB
        spiedHandler.setClients(
            mockKGClient(false),
            mockTSClient(),
            mockRDBClient()
        );

        // Attempt to get results
        JSONArray result = spiedHandler.getData(
            configStore.getConfigEntries().subList(1, 2),
            TestUtils.mockResponse()
        );

        JSONArray expected = new JSONArray(
            """
                [
                    {
                        "data": ["Measurement One"],
                        "values": [
                            [1, 2, 3]
                        ],
                        "timeClass": "Instant",
                        "valuesClass": ["Number"],
                        "units": ["mph"],
                        "id": "1",
                        "time": [
                            "1970-01-01T12:00:00Z",
                            "1970-01-01T13:00:00Z",
                            "1970-01-01T14:00:00Z"
                        ]
                    }
                ]    
            """
        );

        Assertions.assertTrue(expected.similar(result), "Returned JSONArray did not match expected one!");
    }

    /**
     * Tests that time series can be discovered, parsed, and formatted using
     * multiple class matches with a time query that does NOT return measurable
     * with their parent time series IRIs.
     * 
     * @throws Exception if connections to KG or RDB fail.
     */
    @Test
    public void testMultipleMatch() throws Exception {
        Path configFile = TEMP_DIR.resolve("mock-config-01/config.json");

        // Mock a config store based on the real config file
        ConfigStore configStore = TestUtils.mockConfig(configFile);

        // Initialise a TimeHandler instance
        TimeHandler timeHandler = new TimeHandler(
            "https://test-stack/features/feature-one",
            Optional.empty(),
            configStore
        );

        // Need to return a null connection otherwise it'll fail to connect
        // to a non-existant database
        TimeHandler spiedHandler = spy(timeHandler);
        doReturn(null).when(spiedHandler).connectToDatabase(
            ArgumentMatchers.any(),
            ArgumentMatchers.any()
        );

        // Set clients for communication with KG and RDB
        spiedHandler.setClients(
            mockKGClientForMultiple(),
            mockTSClient(),
            mockRDBClient()
        );

        // Mock class matches
        List<ConfigEntry> classMatches = new ArrayList<>();
        classMatches.add(configStore.getConfigEntries().get(1));
        classMatches.add(configStore.getConfigEntries().get(3));

        // Attempt to get results
        JSONArray result = spiedHandler.getData(
            classMatches,
            TestUtils.mockResponse()
        );

        JSONArray expected = new JSONArray(
            """
                [
                    {"data":["Measurement Two","Measurement Three"],"values":[[10,20,30],[100,200,300]],"timeClass":"Instant",
                    "valuesClass":["Number","Number"],"units":["kph","m/s"],"id":"1","time":["1970-01-02T15:00:00Z",
                    "1970-01-02T16:00:00Z","1970-01-02T17:00:00Z"]},{"data":["Measurement One"],"values":[[1,2,3]],
                    "timeClass":"Instant","valuesClass":["Number"],"units":["mph"],"id":"1","time":["1970-01-01T12:00:00Z",
                    "1970-01-01T13:00:00Z","1970-01-01T14:00:00Z"]}
                ]
            """
        );

        Assertions.assertTrue(expected.similar(result), "Returned JSONArray did not match expected one!");
    }

    /**
     * Returns a mocked RemoteStoreClient for mocked interaction with KGs.
     * 
     * @param boolean parentIRI include parent time series IRI in mock result.
     * 
     * @returns mocked RemoteStoreClient instance.
     */
    private RemoteStoreClient mockKGClient(boolean parentIRI) throws Exception {
        RemoteStoreClient spiedClient = Mockito.mock(RemoteStoreClient.class);

        // Create mock result from KG
        JSONArray mockResult = new JSONArray(
            """
            [{
                "Measurement": "https://test-stack/measurables/measurable-one",
                "Time Series": "https://test-stack/time-series/time-series-one",
                "Name": "Measurement One",
                "Unit": "mph"
            }]
            """
        );
        if(!parentIRI) {
            mockResult.getJSONObject(0).remove("Time Series");
        }

        // Mock methods for getting measurable IRIs
        Mockito.when(
            spiedClient.executeQuery("classTwoTime")
        ).thenReturn(
            mockResult
        );

        Mockito.when(
            spiedClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.eq("classTwoTime")
            )
        ).thenReturn(
            mockResult
        );

        // Mock methods for determining missing parent time series IRIs
        Mockito.when(
            spiedClient.executeQuery(
                ArgumentMatchers.contains("hasTimeSeries")
            )
        ).thenReturn(
            new JSONArray(
                """
                [{
                    "timeseries": "https://test-stack/time-series/time-series-one"
                }]
                """
            )
        );

        Mockito.when(
            spiedClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.contains("hasTimeSeries")
            )
        ).thenReturn(
            new JSONArray(
                """
                [{
                    "timeseries": "https://test-stack/time-series/time-series-one"
                }]
                """
            )
        );

        return spiedClient;
    }

    /**
     * Returns a mocked RemoteStoreClient for mocked interaction with KGs.
     * 
     * @returns mocked RemoteStoreClient instance.
     */
    private RemoteStoreClient mockKGClientForMultiple() throws Exception {
        RemoteStoreClient spiedClient = Mockito.mock(RemoteStoreClient.class);

        // Create mock result from KG
        JSONArray mockResultOne = new JSONArray(
            """
            [{
                "Measurement": "https://test-stack/measurables/measurable-one",
                "Name": "Measurement One",
                "Unit": "mph"
            }]
            """
        );
        JSONArray mockResultTwo = new JSONArray(
            """
            [
                {
                    "Measurement": "https://test-stack/measurables/measurable-two",
                    "Name": "Measurement Two",
                    "Unit": "kph"
                },
                {
                    "Measurement": "https://test-stack/measurables/measurable-three",
                    "Name": "Measurement Three",
                    "Unit": "m/s"
                }
            ]
            """
        );

        // Mock methods for getting measurable IRIs
        Mockito.when(
            spiedClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.eq("classTwoTime")
            )
        ).thenReturn(mockResultOne);

        Mockito.when(
            spiedClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.eq("classFourTime")
            )
        ).thenReturn(mockResultTwo);

        // Mock methods for determining missing parent time series IRIs
        Mockito.when(
            spiedClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.contains("https://test-stack/measurables/measurable-one")
            )
        ).thenReturn(
            new JSONArray(
                """
                [{"timeseries": "https://test-stack/time-series/time-series-one"}]
                """
            )
        );

        Mockito.when(
            spiedClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.contains("https://test-stack/measurables/measurable-two")
            )
        ).thenReturn(
            new JSONArray(
                """
                [{"timeseries": "https://test-stack/time-series/time-series-two"}]
                """
            )
        );

        Mockito.when(
            spiedClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.contains("https://test-stack/measurables/measurable-three")
            )
        ).thenReturn(
            new JSONArray(
                """
                [{"timeseries": "https://test-stack/time-series/time-series-two"}]
                """
            )
        );

        return spiedClient;
    }

    /**
     * Returns a mocked TimeSeriesClient for mocked interaction with RDBs.
     * 
     * @returns mocked TimeSeriesClient instance.
     */
    private TimeSeriesClient<Instant> mockTSClient() throws Exception {
        TimeSeriesClient<Instant> spiedClient = Mockito.mock(TimeSeriesClient.class);

        List<String> inputOne = new ArrayList<>(Arrays.asList(
            "https://test-stack/measurables/measurable-one"
        ));
        List<String> inputTwo = new ArrayList<>(Arrays.asList(
            "https://test-stack/measurables/measurable-two",
            "https://test-stack/measurables/measurable-three"
        ));

        Mockito.when(
            spiedClient.getTimeSeriesWithinBounds(
                ArgumentMatchers.eq(inputOne),
                ArgumentMatchers.any(),
                ArgumentMatchers.any(),
                ArgumentMatchers.any()
            )
        ).thenReturn(
            new TimeSeries<Instant>(
                Arrays.asList(
                    Instant.parse("1970-01-01T12:00:00Z"),
                    Instant.parse("1970-01-01T13:00:00Z"),
                    Instant.parse("1970-01-01T14:00:00Z")
                ),
                Arrays.asList("https://test-stack/measurables/measurable-one"),
                Arrays.asList(Arrays.asList(1.0, 2.0, 3.0))
            )
        );

         Mockito.when(
            spiedClient.getTimeSeriesWithinBounds(
                ArgumentMatchers.eq(inputTwo),
                ArgumentMatchers.any(),
                ArgumentMatchers.any(),
                ArgumentMatchers.any()
            )
        ).thenReturn(
            new TimeSeries<Instant>(
                Arrays.asList(
                    Instant.parse("1970-01-02T15:00:00Z"),
                    Instant.parse("1970-01-02T16:00:00Z"),
                    Instant.parse("1970-01-02T17:00:00Z")
                ),
                Arrays.asList(
                    "https://test-stack/measurables/measurable-two",
                    "https://test-stack/measurables/measurable-three"
                ),
                Arrays.asList(
                    Arrays.asList(10.0, 20.0, 30.0),
                    Arrays.asList(100.0, 200.0, 300.0)
                )
            )
        );

        return spiedClient;
    }

    /**
     * Returns a mocked RemoteRDBStoreClient for mocked interaction with RDBs.
     * 
     * @returns mocked RemoteRDBStoreClient instance.
     */
    private RemoteRDBStoreClient mockRDBClient() throws Exception {
        RemoteRDBStoreClient spiedClient = Mockito.mock(RemoteRDBStoreClient.class);

        Mockito.when(
            spiedClient.getConnection()
        ).thenReturn(
            Mockito.mock(Connection.class)
        );

        return spiedClient;
    }

}
// End of class.