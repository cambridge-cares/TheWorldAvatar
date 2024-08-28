package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.IntegrationTestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.sql.Connection;
import java.util.*;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

public class PostGisClientTest {
    public static final String TEMPERATURE_SQL_DATABASE = "temperature";
    public static final String ROOM_SQL_DATABASE = "room";
    public static final String SAMPLE_TEMPERATURE_INSTANCE = TestUtils.genInstance(SparqlClientTest.TEMPERATURE);
    public static final String SAMPLE_TEMPERATURE_TIME_SERIES_INSTANCE = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_TEMPERATURE_TABLE = "12sdv871-e8173vc";
    public static final String SAMPLE_TEMPERATURE_COLUMN = "column1";
    public static final String SAMPLE_ROOM_HUMIDITY_INSTANCE = TestUtils.genInstance(SparqlClientTest.RELATIVE_HUMIDITY.replace(" ", ""));
    public static final String SAMPLE_ROOM_HUMIDITY_TIME_SERIES_INSTANCE = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_ROOM_HUMIDITY_TABLE = "sia2018a";
    public static final String SAMPLE_ROOM_HUMIDITY_COLUMN = "column2";
    public static final String SAMPLE_SYSTEM_ELEC_CONSUMPTION_INSTANCE = TestUtils.genInstance(SparqlClientTest.ELECTRICITY_CONSUMPTION);
    public static final String SAMPLE_SYSTEM_ELEC_CONSUMPTION_TIME_SERIES_INSTANCE = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE = "8jwg18-4817-21";
    public static final String SAMPLE_SYSTEM_ELEC_CONSUMPTION_COLUMN = "column2";

    @BeforeAll
    static void setup() {
        genSampleDatabases();
    }

    @AfterAll
    static void cleanUp() {
        removeSampleDatabases();
    }

    @Test
    void testGetterMethods_Username_Password_JDBC() {
        // Construct the object
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        // Execute methods and verify outputs
        assertEquals(IntegrationTestUtils.TEST_POSTGIS_USER, testClient.getUsername());
        assertEquals(IntegrationTestUtils.TEST_POSTGIS_PASSWORD, testClient.getPassword());
        assertEquals(IntegrationTestUtils.TEST_POSTGIS_JDBC + TEMPERATURE_SQL_DATABASE, testClient.getJdbc(TEMPERATURE_SQL_DATABASE));
    }

    @Test
    void testGetDatabaseNames() {
        // Construct the object
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        // Execute method
        List<String> databaseNames = testClient.getDatabaseNames();
        // Verify outputs
        assertEquals(2, databaseNames.size()); // Two databases should be created/ detected
        databaseNames.forEach((database) -> {
            assertTrue(database.equals(TEMPERATURE_SQL_DATABASE) || database.equals(ROOM_SQL_DATABASE));
        });
    }

    @Test
    void testGetMeasureColAndTableName_NoData() {
        // Set up the test objects
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        // Execute method
        Map<String, Map<String, List<String[]>>> results = testClient.getMeasureColAndTableName(new HashMap<>());
        // Verify outputs
        assertEquals(0, results.size());
    }

    @Test
    void testGetMeasureColAndTableName_ForOnlyAssets() {
        // Set up the test objects
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        Map<String, Queue<String[]>> measures = genSampleAssetMeasures();
        // Execute method
        Map<String, Map<String, List<String[]>>> results = testClient.getMeasureColAndTableName(measures);
        // Verify outputs
        assertEquals(2, results.size());
        // Get list of associated facility item map
        List<String[]> facilityList = results.get(StringHelper.FACILITY_KEY).get(SparqlClientTest.SAMPLE_LAB_NAME);
        assertEquals(1, facilityList.size()); // Only one facility and its mappings should be found
        assertEquals(SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_NAME, facilityList.get(0)[0]); // The only item name
        // Get nested map
        Map<String, List<String[]>> itemMetadata = results.get(SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_TYPE);
        // For the asset key, only one value should be available for the name
        List<String[]> metadataList = itemMetadata.get(StringHelper.ASSET_KEY);
        assertEquals(1, metadataList.size());
        assertEquals(SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_NAME, metadataList.get(0)[0]);
        // For the measure key, verify the following metadata is accurate
        metadataList = itemMetadata.get(SparqlClientTest.TEMPERATURE);
        assertEquals(1, metadataList.size());
        String[] metadata = metadataList.get(0);
        assertEquals(SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_NAME, metadata[0]);
        assertEquals(SAMPLE_TEMPERATURE_COLUMN, metadata[1]);
        assertEquals(SAMPLE_TEMPERATURE_TABLE, metadata[2]);
        assertEquals(TEMPERATURE_SQL_DATABASE, metadata[3]);
        assertEquals(SparqlClientTest.TEMPERATURE_UNIT, metadata[4]);
    }

    @Test
    void testGetMeasureColAndTableName_ForRoomsAndSystems() {
        // Set up the test objects
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        Map<String, Queue<String[]>> measures = genSampleSystemMeasures();
        // Execute method
        Map<String, Map<String, List<String[]>>> results = testClient.getMeasureColAndTableName(measures);
        // Verify outputs
        assertEquals(2, results.size());
        // Get list of associated facility item map
        List<String[]> facilityList = results.get(StringHelper.FACILITY_KEY).get(SparqlClientTest.SAMPLE_OFFICE_NAME);
        assertEquals(1, facilityList.size()); // Only one facility and its mappings should be found
        assertEquals(SparqlClientTest.SAMPLE_OFFICE_SYSTEM_NAME, facilityList.get(0)[0]); // The only SYSTEM name
        // Get nested map
        Map<String, List<String[]>> itemMetadata = results.get(StringHelper.SYSTEM_KEY);
        // For the system key, only one value should be available for the name
        List<String[]> metadataList = itemMetadata.get(StringHelper.SYSTEM_KEY);
        assertEquals(1, metadataList.size());
        assertEquals(SparqlClientTest.SAMPLE_OFFICE_SYSTEM_NAME, metadataList.get(0)[0]);
        // For the measure key, verify the following metadata is accurate
        metadataList = itemMetadata.get(SparqlClientTest.ELECTRICITY_CONSUMPTION);
        assertEquals(1, metadataList.size());
        String[] metadata = metadataList.get(0);
        assertEquals(SparqlClientTest.SAMPLE_OFFICE_SYSTEM_NAME, metadata[0]);
        assertEquals(SAMPLE_SYSTEM_ELEC_CONSUMPTION_COLUMN, metadata[1]);
        assertEquals(SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE, metadata[2]);
        assertEquals(ROOM_SQL_DATABASE, metadata[3]);
        assertEquals(SparqlClientTest.ELECTRICITY_CONSUMPTION_UNIT, metadata[4]);
    }

    @Test
    void testGetMeasureColAndTableName_ForOnlyRooms() {
        // Set up the test objects
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        Map<String, Queue<String[]>> measures = genSampleRoomMeasures(false);
        // Execute method
        Map<String, Map<String, List<String[]>>> results = testClient.getMeasureColAndTableName(measures);
        // Verify outputs
        assertEquals(2, results.size());
        // Get list of associated facility item map
        List<String[]> facilityList = results.get(StringHelper.FACILITY_KEY).get(SparqlClientTest.SAMPLE_OFFICE_NAME);
        assertEquals(1, facilityList.size()); // Only one facility and its mappings should be found
        assertEquals(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, facilityList.get(0)[0]); // The only room name
        // Get nested map
        Map<String, List<String[]>> itemMetadata = results.get(StringHelper.ROOM_KEY);
        // For the room key, only one value should be available for the name
        List<String[]> metadataList = itemMetadata.get(StringHelper.ROOM_KEY);
        assertEquals(1, metadataList.size());
        assertEquals(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, metadataList.get(0)[0]);
        // For the measure key, verify the following metadata is accurate
        metadataList = itemMetadata.get(SparqlClientTest.RELATIVE_HUMIDITY);
        assertEquals(1, metadataList.size());
        String[] metadata = metadataList.get(0);
        assertEquals(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, metadata[0]);
        assertEquals(SAMPLE_ROOM_HUMIDITY_COLUMN, metadata[1]);
        assertEquals(SAMPLE_ROOM_HUMIDITY_TABLE, metadata[2]);
        assertEquals(ROOM_SQL_DATABASE, metadata[3]);
        assertEquals("null", metadata[4]);
    }

    @Test
    void testGetMeasureColAndTableName_ForOnlyRoomsWithThresholds() {
        // Set up the test objects
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        Map<String, Queue<String[]>> measures = genSampleRoomMeasures(true);
        // Execute method
        Map<String, Map<String, List<String[]>>> results = testClient.getMeasureColAndTableName(measures);
        // Verify outputs
        assertEquals(2, results.size());
        // Get list of associated facility item map
        List<String[]> facilityList = results.get(StringHelper.FACILITY_KEY).get(SparqlClientTest.SAMPLE_OFFICE_NAME);
        assertEquals(1, facilityList.size()); // Only one facility and its mappings should be found
        assertEquals(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, facilityList.get(0)[0]); // The only room name
        // Get nested map
        Map<String, List<String[]>> itemMetadata = results.get(StringHelper.ROOM_KEY);
        // For the room key, only one value should be available for the name
        List<String[]> metadataList = itemMetadata.get(StringHelper.ROOM_KEY);
        assertEquals(1, metadataList.size());
        assertEquals(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, metadataList.get(0)[0]);
        // For the threshold key, verify that the min and max threshold is correct
        metadataList = itemMetadata.get(StringHelper.THRESHOLD_KEY);
        assertEquals(1, metadataList.size());
        assertEquals(SparqlClientTest.RELATIVE_HUMIDITY, metadataList.get(0)[0]);
        assertEquals(String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MIN_THRESHOLD), metadataList.get(0)[1]);
        assertEquals(String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MAX_THRESHOLD), metadataList.get(0)[2]);
        // For the measure key, verify the following metadata is accurate
        metadataList = itemMetadata.get(SparqlClientTest.RELATIVE_HUMIDITY);
        assertEquals(1, metadataList.size());
        String[] metadata = metadataList.get(0);
        assertEquals(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, metadata[0]);
        assertEquals(SAMPLE_ROOM_HUMIDITY_COLUMN, metadata[1]);
        assertEquals(SAMPLE_ROOM_HUMIDITY_TABLE, metadata[2]);
        assertEquals(ROOM_SQL_DATABASE, metadata[3]);
        assertEquals("null", metadata[4]);
    }

    public static void genSampleDatabases() {
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.TEST_POSTGIS_JDBC)) {
            String dbCreationQuery = "CREATE DATABASE " + TEMPERATURE_SQL_DATABASE;
            IntegrationTestUtils.updateDatabase(conn, dbCreationQuery);
            dbCreationQuery = "CREATE DATABASE " + ROOM_SQL_DATABASE;
            IntegrationTestUtils.updateDatabase(conn, dbCreationQuery);
            genSampleDbTable();
        } catch (Exception e) {
            throw new RuntimeException("Unable to set up test databases: " + e.getMessage());
        }
    }

    public static void removeSampleDatabases() {
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.TEST_POSTGIS_JDBC)) {
            String dbDeletionQuery = "DROP DATABASE IF EXISTS " + TEMPERATURE_SQL_DATABASE;
            IntegrationTestUtils.updateDatabase(conn, dbDeletionQuery);
            dbDeletionQuery = "DROP DATABASE IF EXISTS " + ROOM_SQL_DATABASE;
            IntegrationTestUtils.updateDatabase(conn, dbDeletionQuery);
        } catch (Exception e) {
            throw new RuntimeException("Unable to clean up databases: " + e.getMessage());
        }
    }

    public static void genSampleDbTable() {
        String insertTemperatureDataSQL = new StringBuilder()
                .append("INSERT INTO \"dbTable\" (\"dataIRI\", \"timeseriesIRI\", \"tableName\", \"columnName\")")
                .append("VALUES ('").append(SAMPLE_TEMPERATURE_INSTANCE).append("', '")
                .append(SAMPLE_TEMPERATURE_TIME_SERIES_INSTANCE).append("', '")
                .append(SAMPLE_TEMPERATURE_TABLE).append("', '")
                .append(SAMPLE_TEMPERATURE_COLUMN).append("')")
                .toString();
        genDbTable(TEMPERATURE_SQL_DATABASE, insertTemperatureDataSQL);
        String insertRoomDataSQL = new StringBuilder()
                .append("INSERT INTO \"dbTable\" (\"dataIRI\", \"timeseriesIRI\", \"tableName\", \"columnName\")")
                // Add first row of values for room
                .append("VALUES ('").append(SAMPLE_ROOM_HUMIDITY_INSTANCE).append("', '")
                .append(SAMPLE_ROOM_HUMIDITY_TIME_SERIES_INSTANCE).append("', '")
                .append(SAMPLE_ROOM_HUMIDITY_TABLE).append("', '")
                .append(SAMPLE_ROOM_HUMIDITY_COLUMN).append("'), ")
                // Add second row of values for system
                .append("('").append(SAMPLE_SYSTEM_ELEC_CONSUMPTION_INSTANCE).append("', '")
                .append(SAMPLE_SYSTEM_ELEC_CONSUMPTION_TIME_SERIES_INSTANCE).append("', '")
                .append(SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE).append("', '")
                .append(SAMPLE_SYSTEM_ELEC_CONSUMPTION_COLUMN).append("')")
                .toString();
        genDbTable(ROOM_SQL_DATABASE, insertRoomDataSQL);
    }

    public static void genDbTable(String database, String insertDataSQL) {
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.TEST_POSTGIS_JDBC + database)) {
            String dbCreationQuery = new StringBuilder().append("CREATE TABLE \"dbTable\" (")
                    .append("\"dataIRI\" character varying NULL,")
                    .append("\"timeseriesIRI\" character varying NULL,")
                    .append("\"tableName\" character varying NULL,")
                    .append("\"columnName\" character varying NULL")
                    .append(")").toString();
            IntegrationTestUtils.updateDatabase(conn, dbCreationQuery);
            IntegrationTestUtils.updateDatabase(conn, insertDataSQL);
        } catch (Exception e) {
            throw new RuntimeException("Unable to generate dbTable for " + database + " . See error message: " + e.getMessage());
        }
    }

    private static Map<String, Queue<String[]>> genSampleAssetMeasures() {
        Map<String, Queue<String[]>> measures = new HashMap<>();
        Queue<String[]> measureMetadata = new ArrayDeque<>();
        measureMetadata.offer(new String[]{SparqlClientTest.TEMPERATURE, SAMPLE_TEMPERATURE_INSTANCE, SAMPLE_TEMPERATURE_TIME_SERIES_INSTANCE, SparqlClientTest.TEMPERATURE_UNIT, SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_TYPE});
        measures.put(SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_NAME, measureMetadata);
        Queue<String[]> facilities = new ArrayDeque<>();
        facilities.offer(genFacilityItemsList(SparqlClientTest.SAMPLE_LAB_NAME, SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_NAME));
        measures.put(StringHelper.FACILITY_KEY, facilities);
        return measures;
    }

    private static String[] genFacilityItemsList(String facilityName, String... facilityItems) {
        String[] facility = Stream.concat(Stream.of(facilityName), Arrays.stream(facilityItems))
                .toArray(String[]::new);
        return facility;
    }

    private static Map<String, Queue<String[]>> genSampleRoomMeasures(boolean requireThresholds) {
        Map<String, Queue<String[]>> measures = new HashMap<>();
        Queue<String[]> measureMetadata = new ArrayDeque<>();
        measureMetadata.offer(new String[]{SparqlClientTest.RELATIVE_HUMIDITY, SAMPLE_ROOM_HUMIDITY_INSTANCE, SAMPLE_ROOM_HUMIDITY_TIME_SERIES_INSTANCE, null, StringHelper.ROOM_KEY});
        measures.put(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, measureMetadata);
        Queue<String[]> facilities = new ArrayDeque<>();
        facilities.offer(genFacilityItemsList(SparqlClientTest.SAMPLE_OFFICE_NAME, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME));
        measures.put(StringHelper.FACILITY_KEY, facilities);
        if (requireThresholds) {
            measureMetadata = new ArrayDeque<>();
            measureMetadata.offer(new String[]{SparqlClientTest.RELATIVE_HUMIDITY, String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MIN_THRESHOLD), String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MAX_THRESHOLD)});
            measures.put(StringHelper.THRESHOLD_KEY, measureMetadata);
        }
        return measures;
    }

    private static Map<String, Queue<String[]>> genSampleSystemMeasures() {
        Map<String, Queue<String[]>> measures = new HashMap<>();
        Queue<String[]> measureMetadata = new ArrayDeque<>();
        measureMetadata.offer(new String[]{SparqlClientTest.ELECTRICITY_CONSUMPTION, SAMPLE_SYSTEM_ELEC_CONSUMPTION_INSTANCE, SAMPLE_SYSTEM_ELEC_CONSUMPTION_TIME_SERIES_INSTANCE, SparqlClientTest.ELECTRICITY_CONSUMPTION_UNIT, StringHelper.SYSTEM_KEY});
        measures.put(SparqlClientTest.SAMPLE_OFFICE_SYSTEM_NAME, measureMetadata);
        Queue<String[]> facilities = new ArrayDeque<>();
        facilities.offer(genFacilityItemsList(SparqlClientTest.SAMPLE_OFFICE_NAME, SparqlClientTest.SAMPLE_OFFICE_SYSTEM_NAME));
        measures.put(StringHelper.FACILITY_KEY, facilities);
        return measures;
    }
}