package uk.ac.cam.cares.jps.sql;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.postgresql.util.PSQLException;
import uk.ac.cam.cares.jps.IntegrationTestUtils;

import java.sql.Connection;
import java.sql.SQLException;

import static org.junit.jupiter.api.Assertions.*;

class SqlConnectionPoolIntegrationTest {
    private static SqlConnectionPool TEST_POOL;

    @BeforeAll
    static void initPool() {
        String[] config = new String[6];
        config[0] = IntegrationTestUtils.SQL_DEFAULT_JDBC;
        config[1] = IntegrationTestUtils.SQL_USER;
        config[2] = IntegrationTestUtils.SQL_PASS;
        config[3] = IntegrationTestUtils.SQL_TGT_JDBC;
        config[4] = IntegrationTestUtils.SQL_USER;
        config[5] = IntegrationTestUtils.SQL_PASS;
        TEST_POOL = new SqlConnectionPool(config);
    }

    @Test
    void testGetSourceConnection() throws SQLException {
        // Execute method
        Connection conn = TEST_POOL.getSourceConnection();
        // Connection should be valid if database is available
        assertTrue(conn.isValid(60));
    }

    @Test
    void testGetTargetConnection() {
        // Execute method should throw right error and response
        // Database has not been created and should throw an error
        PSQLException thrownError = assertThrows(PSQLException.class, () -> TEST_POOL.getTargetConnection());
        assertEquals("FATAL: database \"test\" does not exist", thrownError.getMessage());
    }

    @Test
    void testGetConfigs() {
        // Execute method
        String[] results = TEST_POOL.getConfigs();
        // Validate results
        assertEquals(IntegrationTestUtils.SQL_DEFAULT_JDBC, results[0]);
        assertEquals(IntegrationTestUtils.SQL_USER, results[1]);
        assertEquals(IntegrationTestUtils.SQL_PASS, results[2]);
        assertEquals(IntegrationTestUtils.SQL_TGT_JDBC, results[3]);
        assertEquals(IntegrationTestUtils.SQL_USER, results[4]);
        assertEquals(IntegrationTestUtils.SQL_PASS, results[5]);
    }
}