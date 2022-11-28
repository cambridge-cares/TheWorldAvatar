package uk.ac.cam.cares.jps.base.config.test;

import org.junit.Test;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import static org.junit.Assert.*;

import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


public class KeyValueManagerTest {

    String testKey = "k1";
    String testValue = "v1" ;

    @Test
    public void setTest(){

        try{
            KeyValueManager.set(testKey, testValue);
        }catch (Exception e){
            assertEquals(JPSRuntimeException.class, e.getClass());
            assertTrue(e.getMessage().contains("/JPS_BASE/keys/set"));
            assertTrue(e.getMessage().contains(testKey));
            assertTrue(e.getMessage().contains(testValue));
        }

        KeyValueMap keyValueMap = KeyValueMap.getInstance();
        assertEquals(testValue, keyValueMap.get(testKey));

    }

    @Test
    public void getTest() {

        //Testing if statement
        String test_host = KeyValueManager.get("host");
        assertEquals("localhost",test_host);

        String test_port = KeyValueManager.get("port");
        assertEquals("8080",test_port);

        //Testing try statement
        try{
            KeyValueManager.get(testKey);
        }catch (Exception e){
            assertEquals(JPSRuntimeException.class, e.getClass());
            assertTrue(e.getMessage().contains("/JPS_BASE/keys/get"));
            assertTrue(e.getMessage().contains(testKey));
        }

    }

    @Test
    public void getServerAddressTest() {

        assertNotNull(KeyValueManager.getServerAddress());
        assertEquals("http://localhost:8080",KeyValueManager.getServerAddress());

    }
}
