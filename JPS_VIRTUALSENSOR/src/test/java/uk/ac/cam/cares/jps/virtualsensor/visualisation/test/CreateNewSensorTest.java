package uk.ac.cam.cares.jps.virtualsensor.visualisation.test;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class CreateNewSensorTest{
	@Test
    public void testCreateNewSensor() {
    	JSONObject request = new JSONObject();
    	request.put("lat", 50.35513899998897);
    	request.put("lng", -4.144144999999668);
    	AgentCaller.executeGetWithURLAndJSON("http://localhost:8081/JPS_VIRTUALSENSOR/CreateNewSensor", request.toString());
    }
}
