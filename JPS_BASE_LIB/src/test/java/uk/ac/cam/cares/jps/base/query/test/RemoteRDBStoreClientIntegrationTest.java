package uk.ac.cam.cares.jps.base.query.test;

import org.junit.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

//Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.
public class RemoteRDBStoreClientIntegrationTest {

    // Define RDB database setup (analogous to a triple-store endpoint)
    // Using special testcontainers URL that will spin up a Postgres Docker container when accessed by a driver
    // (see: https://www.testcontainers.org/modules/databases/jdbc/). Note: requires Docker to be installed!
    private static final String dbURL = "jdbc:tc:postgresql:13.3:///timeseries";
    // For easier local debugging, use the following dbURL instead of the testcontainer dbURL
    // NOTE: Requires local postgreSQL database "timeseries" to be set up beforehand
    //private static final String dbURL = "jdbc:postgresql:timeseries";
    private static final String user = "postgres";
    private static final String password = "postgres";

    /**
     * Tests the getConnection method which returns the connection to the RDB when given a valid url
     * and RDB credentials
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     * @throws SQLException
     */
    @Test
    public void getConnectionTest() throws NoSuchFieldException, IllegalAccessException, SQLException {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbURL, user, password);
        Field statement = rdbStoreClient.getClass().getDeclaredField("stmt");
        statement.setAccessible(true);
        Statement stmt = (Statement) statement.get(rdbStoreClient);
        assertNull(stmt);
        Connection conn = rdbStoreClient.getConnection();
        assertNotNull(conn);
        assertFalse(conn.isClosed());
        stmt = (Statement) statement.get(rdbStoreClient);
        assertNotNull(stmt);
        conn.close();
        RemoteRDBStoreClient rdbStoreClient1 = new RemoteRDBStoreClient(null, null, null);
        JPSRuntimeException e = assertThrows(JPSRuntimeException.class, () -> rdbStoreClient1.getConnection());
        assertEquals(e.getMessage(), "The connection attempt failed");

        RemoteRDBStoreClient rdbStoreClient2 = new RemoteRDBStoreClient("jdbc:postgresql://host.docker.internal:5432/timeseries", "postgres", "postgres");
        e = assertThrows(JPSRuntimeException.class, () -> rdbStoreClient2.getConnection());
        assertEquals(e.getMessage(), "The connection attempt failed");
    }


}
