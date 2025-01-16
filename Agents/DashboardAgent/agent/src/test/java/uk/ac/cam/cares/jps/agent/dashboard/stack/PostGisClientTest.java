package uk.ac.cam.cares.jps.agent.dashboard.stack;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.IntegrationTestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.OrganisationTest;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.ThresholdTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.sql.Connection;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class PostGisClientTest {
    public static final String TEMPERATURE_SQL_DATABASE = "temperature";
    public static final String ALL_SQL_DATABASE = "sql";
    public static final String SAMPLE_TEMPERATURE_TABLE = "12sdv871-e8173vc";
    public static final String SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_COLUMN = "column1";
    public static final String SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_COLUMN = "column4";
    public static final String SAMPLE_SMART_SENSOR_ONE_TEMPERATURE_COLUMN = "column5";
    public static final String SAMPLE_ROOM_HUMIDITY_TABLE = "sia2018a";
    public static final String SAMPLE_OFFICE_STAFF_ROOM_HUMIDITY_COLUMN = "column2";
    public static final String SAMPLE_SYSTEM_ELEC_CONSUMPTION_INSTANCE = TestUtils.genInstance(SparqlClientTest.ELECTRICITY_CONSUMPTION);
    public static final String SAMPLE_SYSTEM_ELEC_CONSUMPTION_TIME_SERIES_INSTANCE = TestUtils.genTimeSeriesInstance();
    public static final String SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE = "8jwg18-4817-21";
    public static final String SAMPLE_OFFICE_SYSTEM_ELEC_CONSUMPTION_COLUMN = "column2";
    public static final String SAMPLE_OFFICE_SUB_SYSTEM_ELEC_CONSUMPTION_COLUMN = "column7";

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
            assertTrue(database.equals(TEMPERATURE_SQL_DATABASE) || database.equals(ALL_SQL_DATABASE));
        });
    }

    @Test
    void testRetrieveMeasureRDBLocation_ForOnlyAssets() {
        // Set up the test objects
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        // Execute method
        testClient.retrieveMeasureRDBLocation(organisation);
        // Verify outputs
        OrganisationTest.verifyOrganisationModel(organisation, OrganisationTest.genExpectedOrgOutputs(false,
                new String[]{SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_NAME, SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_TYPE, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                        SparqlClientTest.SMART_SENSOR_ONE_TEMPERATURE_MEASURE, SparqlClientTest.SMART_SENSOR_ONE_TEMPERATURE_TIME_SERIES_IRI,
                        SAMPLE_SMART_SENSOR_ONE_TEMPERATURE_COLUMN, SAMPLE_TEMPERATURE_TABLE, TEMPERATURE_SQL_DATABASE
                }
        ));
    }

    @Test
    void testRetrieveMeasureRDBLocation_ForRoomsAndSystems() {
        // Set up the test objects
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        Organisation sample = TestUtils.genSampleSystemMeasures(null);
        sample = TestUtils.genSampleRoomMeasures(sample, false);
        // Execute method
        testClient.retrieveMeasureRDBLocation(sample);
        // Verify outputs
        OrganisationTest.verifyOrganisationModel(sample, OrganisationTest.genExpectedOrgOutputs(false,
                new String[]{SparqlClientTest.SAMPLE_OFFICE_SYSTEM_NAME, StringHelper.SYSTEM_KEY, SparqlClientTest.ELECTRICITY_CONSUMPTION, SparqlClientTest.ELECTRICITY_CONSUMPTION_UNIT,
                        SparqlClientTest.SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SparqlClientTest.SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_SYSTEM_ELEC_CONSUMPTION_COLUMN, SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE, ALL_SQL_DATABASE
                },
                new String[]{SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_NAME, StringHelper.SYSTEM_KEY, SparqlClientTest.ELECTRICITY_CONSUMPTION, SparqlClientTest.ELECTRICITY_CONSUMPTION_UNIT,
                        SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_SUB_SYSTEM_ELEC_CONSUMPTION_COLUMN, SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE, ALL_SQL_DATABASE
                },
                new String[]{SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                        SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_COLUMN, SAMPLE_TEMPERATURE_TABLE, TEMPERATURE_SQL_DATABASE
                },
                new String[]{SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                        SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_COLUMN, SAMPLE_TEMPERATURE_TABLE, TEMPERATURE_SQL_DATABASE
                },
                new String[]{SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.RELATIVE_HUMIDITY, null,
                        SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_STAFF_ROOM_HUMIDITY_COLUMN, SAMPLE_ROOM_HUMIDITY_TABLE, ALL_SQL_DATABASE
                }
        ));
    }

    @Test
    void testRetrieveMeasureRDBLocation_ForOnlyRooms() {
        // Set up the test objects
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        Organisation sample = TestUtils.genSampleRoomMeasures(null, false);
        // Execute method
        testClient.retrieveMeasureRDBLocation(sample);
        // Verify outputs
        OrganisationTest.verifyOrganisationModel(sample, OrganisationTest.genExpectedOrgOutputs(false,
                new String[]{SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                        SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_COLUMN, SAMPLE_TEMPERATURE_TABLE, TEMPERATURE_SQL_DATABASE
                },
                new String[]{SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                        SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_COLUMN, SAMPLE_TEMPERATURE_TABLE, TEMPERATURE_SQL_DATABASE
                },
                new String[]{SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.RELATIVE_HUMIDITY, null,
                        SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_STAFF_ROOM_HUMIDITY_COLUMN, SAMPLE_ROOM_HUMIDITY_TABLE, ALL_SQL_DATABASE
                }
        ));
    }

    @Test
    void testRetrieveMeasureRDBLocation_ForOnlyRoomsWithThresholds() {
        // Set up the test objects
        PostGisClient testClient = new PostGisClient(IntegrationTestUtils.TEST_POSTGIS_JDBC, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        Organisation sample = TestUtils.genSampleRoomMeasures(null, true);
        // Execute method
        testClient.retrieveMeasureRDBLocation(sample);
        // Verify outputs
        OrganisationTest.verifyOrganisationModel(sample, OrganisationTest.genExpectedOrgOutputs(false,
                new String[]{SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                        SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_COLUMN, SAMPLE_TEMPERATURE_TABLE, TEMPERATURE_SQL_DATABASE
                },
                new String[]{SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                        SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_COLUMN, SAMPLE_TEMPERATURE_TABLE, TEMPERATURE_SQL_DATABASE
                },
                new String[]{SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.RELATIVE_HUMIDITY, null,
                        SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI,
                        SAMPLE_OFFICE_STAFF_ROOM_HUMIDITY_COLUMN, SAMPLE_ROOM_HUMIDITY_TABLE, ALL_SQL_DATABASE
                }
        ), ThresholdTest.genExpectedThresholds(
                new String[]{SparqlClientTest.RELATIVE_HUMIDITY, String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MIN_THRESHOLD), String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MAX_THRESHOLD)},
                new String[]{SparqlClientTest.TEMPERATURE, String.valueOf(SparqlClientTest.TEMPERATURE_MIN_THRESHOLD), String.valueOf(SparqlClientTest.TEMPERATURE_MAX_THRESHOLD)}
        ));
    }

    public static void genSampleDatabases() {
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.TEST_POSTGIS_JDBC)) {
            String dbCreationQuery = "CREATE DATABASE " + TEMPERATURE_SQL_DATABASE;
            IntegrationTestUtils.updateDatabase(conn, dbCreationQuery);
            dbCreationQuery = "CREATE DATABASE " + ALL_SQL_DATABASE;
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
            dbDeletionQuery = "DROP DATABASE IF EXISTS " + ALL_SQL_DATABASE;
            IntegrationTestUtils.updateDatabase(conn, dbDeletionQuery);
        } catch (Exception e) {
            throw new RuntimeException("Unable to clean up databases: " + e.getMessage());
        }
    }

    public static void genSampleDbTable() {
        String insertTemperatureDataSQL = new StringBuilder()
                .append("INSERT INTO \"dbTable\" (\"dataIRI\", \"timeseriesIRI\", \"tableName\", \"columnName\")")
                .append("VALUES ('").append(SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE).append("', '")
                .append(SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI).append("', '")
                .append(SAMPLE_TEMPERATURE_TABLE).append("', '")
                .append(SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_COLUMN).append("'),")
                .append("('").append(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE).append("', '")
                .append(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI).append("', '")
                .append(SAMPLE_TEMPERATURE_TABLE).append("', '")
                .append(SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_COLUMN).append("'),")
                .append("('").append(SparqlClientTest.SMART_SENSOR_ONE_TEMPERATURE_MEASURE).append("', '")
                .append(SparqlClientTest.SMART_SENSOR_ONE_TEMPERATURE_TIME_SERIES_IRI).append("', '")
                .append(SAMPLE_TEMPERATURE_TABLE).append("', '")
                .append(SAMPLE_SMART_SENSOR_ONE_TEMPERATURE_COLUMN).append("')")
                .toString();
        genDbTable(TEMPERATURE_SQL_DATABASE, insertTemperatureDataSQL);
        String insertRoomDataSQL = new StringBuilder()
                .append("INSERT INTO \"dbTable\" (\"dataIRI\", \"timeseriesIRI\", \"tableName\", \"columnName\")")
                // Add first row of values for room
                .append("VALUES ('").append(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE).append("', '")
                .append(SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI).append("', '")
                .append(SAMPLE_ROOM_HUMIDITY_TABLE).append("', '")
                .append(SAMPLE_OFFICE_STAFF_ROOM_HUMIDITY_COLUMN).append("'), ")
                // Add second row of values for system
                .append("('").append(SAMPLE_SYSTEM_ELEC_CONSUMPTION_INSTANCE).append("', '")
                .append(SAMPLE_SYSTEM_ELEC_CONSUMPTION_TIME_SERIES_INSTANCE).append("', '")
                .append(SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE).append("', '")
                .append(SAMPLE_OFFICE_SYSTEM_ELEC_CONSUMPTION_COLUMN).append("')")
                .toString();
        genDbTable(ALL_SQL_DATABASE, insertRoomDataSQL);
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
}