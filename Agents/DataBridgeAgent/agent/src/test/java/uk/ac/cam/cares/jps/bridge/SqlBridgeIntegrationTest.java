package uk.ac.cam.cares.jps.bridge;

import org.json.JSONObject;
import org.junit.jupiter.api.*;
import uk.ac.cam.cares.jps.IntegrationTestUtils;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.Connection;

import static org.junit.jupiter.api.Assertions.*;

class SqlBridgeIntegrationTest {
    private static final String[] CONFIG = new String[6];
    private static final String EXPECTED_RESPONSE = "[\"Agent is not running on a stack, please run the following command(s) for transfer. " +
            "Please exclude the two additional backslash for the table name ie '-t \\\"'\",\"PGPASSWORD='pg123' pg_dump -U user -h 172.17.0.1 -p 5431 --clean -t \\\\\\\"dbtable\\\\\\\" postgres | PGPASSWORD='pg123' psql -U user -h 172.17.0.1 -p 5431 test\"]";

    @BeforeAll
    static void setupDatabase() {
        CONFIG[0] = IntegrationTestUtils.SQL_DEFAULT_JDBC;
        CONFIG[1] = IntegrationTestUtils.SQL_USER;
        CONFIG[2] = IntegrationTestUtils.SQL_PASS;
        CONFIG[3] = IntegrationTestUtils.SQL_TGT_JDBC;
        CONFIG[4] = IntegrationTestUtils.SQL_USER;
        CONFIG[5] = IntegrationTestUtils.SQL_PASS;
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.SQL_DEFAULT_JDBC)) {
            // Create and populate source table
            String createTableQuery = "CREATE TABLE dbTable (time int, column_1 int, column_2 int)";
            String insertDataQuery = "INSERT INTO dbTable (time, column_1, column_2) VALUES "
                    + "(500, 10, 1),"
                    + "(550, 12, 2)";
            IntegrationTestUtils.updateDatabase(conn, createTableQuery);
            IntegrationTestUtils.updateDatabase(conn, insertDataQuery);
            // Create a target database
            String createDatabaseQuery = "CREATE DATABASE test";
            IntegrationTestUtils.updateDatabase(conn, createDatabaseQuery);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to create test database: " + e.getMessage());
        }
    }

    @AfterAll
    static void cleanUp() {
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.SQL_DEFAULT_JDBC)) {
            // Delete any tables created in the source
            String dropTableQuery = "DROP TABLE dbTable";
            IntegrationTestUtils.updateDatabase(conn, dropTableQuery);
            // Delete target database
            String terminateConnectionQuery = "SELECT pg_terminate_backend(pid) " +
                    "FROM pg_stat_activity " +
                    "WHERE datname = 'test';";
            IntegrationTestUtils.queryDatabase(conn, terminateConnectionQuery);
            String deleteTestDatabaseQuery = "DROP DATABASE IF EXISTS test";
            IntegrationTestUtils.updateDatabase(conn, deleteTestDatabaseQuery);
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to clean up databases: " + e.getMessage());
        }
    }

    @Test
    void testConstructor() {
        try {
            // Ensure that the connections are pinged and no exceptions are thrown
            new SqlBridge(CONFIG);
        } catch (Exception e) {
            // An exception is thrown, so the test fails
            fail("Exception thrown during object instantiation.");
        }
    }

    @Test
    void testTransfer() {
        SqlBridge test = new SqlBridge(CONFIG);
        JSONObject response = test.transfer(false);
        assertEquals(EXPECTED_RESPONSE, response.get("Result").toString());
    }
}