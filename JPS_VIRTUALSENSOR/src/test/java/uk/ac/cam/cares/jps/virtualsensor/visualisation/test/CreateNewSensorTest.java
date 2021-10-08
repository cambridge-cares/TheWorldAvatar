package uk.ac.cam.cares.jps.virtualsensor.visualisation.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class CreateNewSensorTest extends TestCase{
    public void testCreateNewSensor() {
    	JSONObject request = new JSONObject();
    	request.put("lat", 1.267135);
    	request.put("lng", 103.864237);
    	AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/CreateNewSensor", request.toString());
    }
}
