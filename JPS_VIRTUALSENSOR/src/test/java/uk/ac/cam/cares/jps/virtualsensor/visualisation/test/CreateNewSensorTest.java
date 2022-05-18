package uk.ac.cam.cares.jps.virtualsensor.visualisation.test;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class CreateNewSensorTest{
	@Test
    public void testCreateNewSensor() {
    	JSONObject request = new JSONObject();
    	request.put("lat", 1.267135);
    	request.put("lng", 103.864237);
    	AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/CreateNewSensor", request.toString());
    }
}
