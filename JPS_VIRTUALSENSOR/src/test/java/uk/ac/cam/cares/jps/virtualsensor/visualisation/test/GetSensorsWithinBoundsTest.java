package uk.ac.cam.cares.jps.virtualsensor.visualisation.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;

public class GetSensorsWithinBoundsTest extends TestCase{
	public void testGetSensorsWithinBounds() {
		JSONObject request = new JSONObject();
    	request.put(Region.keyLowerx, 102.58488095957551);
    	request.put(Region.keyLowery, 0.6083971706438808);
    	request.put(Region.keyUppery, 1.9355550294298127);
    	request.put(Region.keyUpperx, 105.15140948477898);
    	AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/GetSensorsWithinBounds", request.toString());
	}
}
