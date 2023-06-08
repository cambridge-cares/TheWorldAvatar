package uk.ac.cam.cares.jps.bridge;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.IntegrationTestUtils;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.Connection;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

class TimeSeriesBridgeIntegrationTest {
    private static TimeSeriesBridge TEST_CONNECTOR;
    private static String RESULT_TABLE = "placeholder";
    private static final String[] CONFIG = new String[4];
    private static final JSONObject TEST_DATA = new JSONObject();
    private static final String TIMESTAMP_FIRST = "2022-11-09T03:05:18";
    private static final String TIMESTAMP_SECOND = "2022-11-19T03:05:18";
    private static final String TIMESTAMP_THIRD = "2022-11-29T03:05:18";
    private static final String ELECTRICITY_KEY = "electricity";
    private static final int ELECTRICITY_VALUE_ONE = 50;
    private static final int ELECTRICITY_VALUE_TWO = 60;
    private static final int ELECTRICITY_VALUE_THREE = 52;
    private static final String POWER_KEY = "power";
    private static final int POWER_VALUE_ONE = 32;
    private static final int POWER_VALUE_TWO = 31;
    private static final int POWER_VALUE_THREE = 33;
    private static final String ROW_ONE_KEY = "row1";
    private static final String ROW_TWO_KEY = "row2";
    private static final String ROW_THREE_KEY = "row3";
    private static final String TABLE_NAME_KEY = "tableName";
    private static final String DATA_IRI_KEY = "dataIRI";
    private static final String TIME_SERIES_KEY = "timeseriesIRI";
    private static final String COL_ONE_KEY = "column1";
    private static final String COL_TWO_KEY = "column2";

    @BeforeAll
    static void init() {
        // Initialise the configurations
        CONFIG[0] = IntegrationTestUtils.SQL_DEFAULT_JDBC;
        CONFIG[1] = IntegrationTestUtils.SQL_USER;
        CONFIG[2] = IntegrationTestUtils.SQL_PASS;
        CONFIG[3] = IntegrationTestUtils.SRC_SPARQL_ENDPOINT;
        // Set up a new object
        TEST_CONNECTOR = new TimeSeriesBridge(CONFIG, IntegrationTestUtils.TIME_CLASS);
        // Create sample data parameters
        JSONArray temp = new JSONArray();
        // Add time stamps
        temp.put(TIMESTAMP_FIRST);
        temp.put(TIMESTAMP_SECOND);
        TEST_DATA.put("timestamp", temp);
        // In a nested json object
        JSONObject tempObj = new JSONObject();
        // Add electricity values
        temp = new JSONArray();
        temp.put(ELECTRICITY_VALUE_ONE);
        temp.put(ELECTRICITY_VALUE_TWO);
        tempObj.put(ELECTRICITY_KEY, temp);
        // Add power values
        temp = new JSONArray();
        temp.put(POWER_VALUE_ONE);
        temp.put(POWER_VALUE_TWO);
        tempObj.put(POWER_KEY, temp);
        TEST_DATA.put("values", tempObj);
    }

    @AfterEach
    void cleanUp() {
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.SQL_DEFAULT_JDBC)) {
            // Delete any tables created in the source
            IntegrationTestUtils.updateDatabase(conn, IntegrationTestUtils.dropTableQuery("dbTable"));
            IntegrationTestUtils.queryDatabase(conn, IntegrationTestUtils.dropTableQuery(RESULT_TABLE));
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to remove tables: " + e.getMessage());
        }
    }

    @Test
    void testConstructorInvalidTime() {
        // Execute method should throw right error and response
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> new TimeSeriesBridge(CONFIG, ""));
        assertEquals("Type of timeseries is not allowed. Choose from: Type.AVERAGE, Type.INSTANTANEOUS, Type.STEPWISECUMULATIVE, Type.CUMULATIVETOTAL", thrownError.getMessage());
    }

    @Test
    void testConstructor() {
        // Ensure that object is successfully created
        assertNotNull(new TimeSeriesBridge(CONFIG, IntegrationTestUtils.TIME_CLASS));
    }

    @Test
    void testInstantiateTimeSeriesDifferentValuesLength() {
        // Create a clone of the original
        JSONObject dataClone = new JSONObject(TEST_DATA.toString());
        // Override timestamps with only one to ensure a mismatch between the number of timestamps and values
        JSONArray temp = new JSONArray();
        temp.put(TIMESTAMP_FIRST);
        dataClone.put("timestamp", temp);
        // Execute method
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> TEST_CONNECTOR.instantiateTimeSeries(dataClone));
        assertEquals("Number of time stamps does not match their values for :" + ELECTRICITY_KEY, thrownError.getMessage());
    }

    @Test
    void testInstantiateTimeSeriesDataNewReadings() {
        // Generate new readings
        JSONObject newReadings = new JSONObject();
        JSONArray temp = new JSONArray();
        temp.put(TIMESTAMP_THIRD);
        newReadings.put("timestamp", temp);
        JSONObject tempObj = new JSONObject();
        temp = new JSONArray();
        temp.put(ELECTRICITY_VALUE_THREE);
        tempObj.put(ELECTRICITY_KEY, temp);
        temp = new JSONArray();
        temp.put(POWER_VALUE_THREE);
        tempObj.put(POWER_KEY, temp);
        newReadings.put("values", tempObj);
        // Execute method
        TEST_CONNECTOR.instantiateTimeSeries(TEST_DATA);
        // Execute method for additional data
        TEST_CONNECTOR.instantiateTimeSeries(newReadings);
        // To verify, access the database and query the results
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.SQL_DEFAULT_JDBC)) {
            // For default table
            Map<String, Map<String, Object>> results = IntegrationTestUtils.queryDatabase(conn, IntegrationTestUtils.selectAllDataQuery("dbTable"));
            // Get first row results
            Map<String, Object> currentRow = results.get(ROW_ONE_KEY);
            // Retrieve and set the table name, so that it can be accessed and deleted later
            RESULT_TABLE = currentRow.get(TABLE_NAME_KEY).toString();
            // Retrieve the time series IRI for verification later
            String rowOneTimeseriesIRI = currentRow.get(TIME_SERIES_KEY).toString();
            // Verify the data IRI is correct
            assertEquals(ELECTRICITY_KEY, currentRow.get(DATA_IRI_KEY));
            // Get second row results
            currentRow = results.get(ROW_TWO_KEY);
            // Verify the data IRI is correct
            assertEquals(POWER_KEY, currentRow.get(DATA_IRI_KEY));
            // Verify both rows have the same time series IRI
            assertEquals(rowOneTimeseriesIRI, currentRow.get(TIME_SERIES_KEY));
            // Verify that only two rows are generated for dbTable since the data IRI is the same for both set of readings
            assertNull(results.get(ROW_THREE_KEY));
            // For the generated data table
            results = IntegrationTestUtils.queryDatabase(conn, IntegrationTestUtils.selectAllDataQuery(RESULT_TABLE));
            // Ensure that three rows have been generated
            assertEquals(3, results.size());
            // Verify the old readings are correct
            currentRow = results.get(ROW_ONE_KEY);
            assertEquals(Long.valueOf(ELECTRICITY_VALUE_ONE), currentRow.get(COL_ONE_KEY));
            assertEquals(Long.valueOf(POWER_VALUE_ONE), currentRow.get(COL_TWO_KEY));
            assertEquals(IntegrationTestUtils.convertToTimeStamp(TIMESTAMP_FIRST), currentRow.get("time"));
            // Verify the new readings are added
            currentRow = results.get(ROW_THREE_KEY);
            assertEquals(Long.valueOf(ELECTRICITY_VALUE_THREE), currentRow.get(COL_ONE_KEY));
            assertEquals(Long.valueOf(POWER_VALUE_THREE), currentRow.get(COL_TWO_KEY));
            assertEquals(IntegrationTestUtils.convertToTimeStamp(TIMESTAMP_THIRD), currentRow.get("time"));
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to connect to postgres database: " + e.getMessage());
        }
    }

    @Test
    void testInstantiateTimeSeriesData() {
        // Execute method
        TEST_CONNECTOR.instantiateTimeSeries(TEST_DATA);
        // To verify, access the database and query the results
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.SQL_DEFAULT_JDBC)) {
            // For default table
            Map<String, Map<String, Object>> results = IntegrationTestUtils.queryDatabase(conn, IntegrationTestUtils.selectAllDataQuery("dbTable"));
            // Get first row results
            Map<String, Object> currentRow = results.get(ROW_ONE_KEY);
            // Retrieve and set the table name, so that it can be accessed and deleted later
            RESULT_TABLE = currentRow.get(TABLE_NAME_KEY).toString();
            // Retrieve the time series IRI for verification later
            String rowOneTimeseriesIRI = currentRow.get(TIME_SERIES_KEY).toString();
            // Verify the data IRI is correct
            assertEquals(ELECTRICITY_KEY, currentRow.get(DATA_IRI_KEY));
            // Get second row results
            currentRow = results.get(ROW_TWO_KEY);
            // Verify the data IRI is correct
            assertEquals(POWER_KEY, currentRow.get(DATA_IRI_KEY));
            // Verify both rows have the same time series IRI
            assertEquals(rowOneTimeseriesIRI, currentRow.get(TIME_SERIES_KEY));
            // For the generated data table
            results = IntegrationTestUtils.queryDatabase(conn, IntegrationTestUtils.selectAllDataQuery(RESULT_TABLE));
            // Ensure that two rows have been generated
            assertEquals(2, results.size());
            // Verify the first row values are correct
            currentRow = results.get(ROW_ONE_KEY);
            assertEquals(Long.valueOf(ELECTRICITY_VALUE_ONE), currentRow.get(COL_ONE_KEY));
            assertEquals(Long.valueOf(POWER_VALUE_ONE), currentRow.get(COL_TWO_KEY));
            assertEquals(IntegrationTestUtils.convertToTimeStamp(TIMESTAMP_FIRST), currentRow.get("time"));
            // Verify the second row values are correct
            currentRow = results.get(ROW_TWO_KEY);
            assertEquals(Long.valueOf(ELECTRICITY_VALUE_TWO), currentRow.get(COL_ONE_KEY));
            assertEquals(Long.valueOf(POWER_VALUE_TWO), currentRow.get(COL_TWO_KEY));
            assertEquals(IntegrationTestUtils.convertToTimeStamp(TIMESTAMP_SECOND), currentRow.get("time"));
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to connect to postgres database: " + e.getMessage());
        }
    }
}