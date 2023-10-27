package uk.ac.cam.cares.jps.base.query;

import org.junit.Test;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.lang.reflect.Field;
import static org.junit.Assert.*;

/**
 * This class provides unit tests for the RemoteRDBStoreClient class
 *
 * @author Mehal Agarwal (ma988@cam.ac.uk)
 */

public class RemoteRDBStoreClientTest {

    private String dbUrl = "jdbc:postgresql://host.docker.internal:5432/timeseries";
    private String user = "postgres";
    private String password = "postgres";

    /**
     * Tests the number of methods in RemoteRDBStoreClient class
     */
    @Test
    public void testNewRemoteRDBStoreClientMethods() {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, user, password);
        assertEquals(16, rdbStoreClient.getClass().getDeclaredMethods().length);
    }

    /**
     * Tests the constructor of RemoteRDBStoreClient such that it correctly sets the RDB url and credentials
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     */
    @Test
    public void testConstructor() throws NoSuchFieldException, IllegalAccessException {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, user, password);

        Field rdbUrl = rdbStoreClient.getClass().getDeclaredField("rdbURL");
        rdbUrl.setAccessible(true);
        String rdbUrlValue = (String) rdbUrl.get(rdbStoreClient);
        Field rdbUser = rdbStoreClient.getClass().getDeclaredField("rdbUser");
        rdbUser.setAccessible(true);
        String rdbUserValue = (String) rdbUser.get(rdbStoreClient);
        Field rdbPassword = rdbStoreClient.getClass().getDeclaredField("rdbPassword");
        rdbPassword.setAccessible(true);
        String rdbPasswordValue = (String) rdbPassword.get(rdbStoreClient);

        assertEquals(dbUrl, rdbUrlValue);
        assertEquals(user, rdbUserValue);
        assertEquals(password, rdbPasswordValue);
    }

    /**
     * Tests setRdbUrl method such that it sets the rdbUrl of RemoteRDBStoreClient correctly.
     */
    @Test
    public void setRdbUrlTest(){
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, user, password);
        rdbStoreClient.setRdbURL("jdbc:postgresql://host.docker.internal:5432/test");
        assertEquals("jdbc:postgresql://host.docker.internal:5432/test", rdbStoreClient.getRdbURL());
    }

    /**
     * Tests getRdbUrl method such that it returns the rdbUrl of RemoteRDBStoreClient correctly.
     */
    @Test
    public void getRdbUrlTest(){
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, user, password);
        assertEquals(dbUrl, rdbStoreClient.getRdbURL());
    }

    /**
     * Tests setUser method such that it sets the user of RemoteRDBStoreClient correctly.
     */
    @Test
    public void setUserTest(){
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, null, password);
        assertNull(rdbStoreClient.getUser());
        rdbStoreClient.setUser("postgres");
        assertEquals("postgres", rdbStoreClient.getUser());
    }

    /**
     * Tests getUser method such that it returns the user of RemoteRDBStoreClient correctly.
     */
    @Test
    public void getUserTest(){
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, user, password);
        assertEquals(user, rdbStoreClient.getUser());
    }

    /**
     * Tests setPassword method such that it sets the password of RemoteRDBStoreClient correctly.
     */
    @Test
    public void setPasswordTest() throws NoSuchFieldException, IllegalAccessException {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, user, null);
        Field rdbPassword = rdbStoreClient.getClass().getDeclaredField("rdbPassword");
        rdbPassword.setAccessible(true);
        String rdbPasswordValue = (String) rdbPassword.get(rdbStoreClient);
        assertNull(rdbPasswordValue);
        rdbStoreClient.setPassword("postgres");
        rdbPasswordValue = (String) rdbPassword.get(rdbStoreClient);
        assertEquals(password, rdbPasswordValue);
    }
}
