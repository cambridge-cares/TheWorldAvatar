package uk.ac.cam.cares.jps.sql;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.IntegrationTestUtils;

import static org.junit.jupiter.api.Assertions.*;

class PostGISClientTest {
    private static PostGISClient TEST_CLIENT;
    private static final String DB_TABLE = "test";
    private static final String EXPECTED_RESPONSE = "PGPASSWORD='pg123' pg_dump -U user -h 172.17.0.1 -p 5431 --clean -t \\\"" + DB_TABLE + "\\\" postgres | PGPASSWORD='pg123' psql -U user -h 172.17.0.1 -p 5431 test";

    @BeforeAll
    static void initClass() {
        TEST_CLIENT = new PostGISClient(false);
    }

    @Test
    void testTransferTable() {
        // Execute method
        String cmd = TEST_CLIENT.transferTable(DB_TABLE, IntegrationTestUtils.SQL_DEFAULT_JDBC, IntegrationTestUtils.SQL_USER, IntegrationTestUtils.SQL_PASS,
                IntegrationTestUtils.SQL_TGT_JDBC, IntegrationTestUtils.SQL_USER, IntegrationTestUtils.SQL_PASS);
        // A command should be returned as a string
        assertEquals(EXPECTED_RESPONSE, cmd);
    }
}