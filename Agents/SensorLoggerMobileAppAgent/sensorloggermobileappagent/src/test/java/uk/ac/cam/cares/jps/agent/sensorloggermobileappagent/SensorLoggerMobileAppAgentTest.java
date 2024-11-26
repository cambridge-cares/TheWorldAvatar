package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class SensorLoggerMobileAppAgentTest {
    SensorLoggerMobileAppAgent agent;

    @Before
    public void setup() {
        agent = new SensorLoggerMobileAppAgent();
    }

    @Test
    public void validInputTest() {
        JSONObject object = new JSONObject();
        object.put("messageId", "1");
        object.put("sessionId", "1");
        object.put("deviceId", "123");
        object.put("payload", "");

        assertTrue(agent.validateInput(object));
    }

    @Test
    public void invalidInputTest() {
        JSONObject object = new JSONObject();
        object.put("messageId", "1");
        object.put("sessionId", "1");
        object.put("deviceId", "123");

        assertFalse(agent.validateInput(object));
    }
}
