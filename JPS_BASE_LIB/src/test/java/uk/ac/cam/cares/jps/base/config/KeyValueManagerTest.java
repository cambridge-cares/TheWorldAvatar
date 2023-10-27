package uk.ac.cam.cares.jps.base.config;

import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.stubbing.Answer;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;



public class KeyValueManagerTest {

    String testKey = "k1";
    String testValue = "v1" ;

    @Test
    public void setTest(){

        try (MockedStatic<AgentCaller> aacMock = mockStatic(AgentCaller.class)) {

            //Do not execute  the executeGet() method of the AgentCaller class -> Make it null
            (aacMock).when(() -> AgentCaller.executeGet((String) any(Object.class)))
                    .thenAnswer((Answer<Void>) invocation -> null);

            KeyValueManager.set(testKey, testValue);
            KeyValueMap keyValueMap = KeyValueMap.getInstance();
            assertEquals(testValue, keyValueMap.get(testKey));
        }
    }

    @Test
    public void getTest() {

        try (MockedStatic<AgentCaller> aacMock = mockStatic(AgentCaller.class)) {

            //Testing the ifs statements
            String test_host = KeyValueManager.get("host");
            assertEquals("localhost",test_host);

            String test_port = KeyValueManager.get("port");
            assertEquals("8080",test_port);

            KeyValueManager.set(testKey, testValue);
            KeyValueMap keyValueMap = KeyValueMap.getInstance();
            //Do not execute  the executeGet() method of the AgentCaller class -> Make it a JSON key and value
            (aacMock).when(() -> AgentCaller.executeGet((String) any(Object.class),(String) any(Object.class)))
                    .thenAnswer((Answer<String>)   invocation -> "{\"k1\": \"" + keyValueMap.get(testKey) + "\"}");

            assertEquals(KeyValueManager.get(testKey),testValue);

        }
    }

    @Test
    public void getServerAddressTest() {

        assertNotNull(KeyValueManager.getServerAddress());
        assertEquals("http://localhost:8080",KeyValueManager.getServerAddress());

    }
}
