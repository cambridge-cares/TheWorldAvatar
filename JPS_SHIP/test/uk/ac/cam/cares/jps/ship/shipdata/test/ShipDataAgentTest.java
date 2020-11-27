package uk.ac.cam.cares.jps.ship.shipdata.test;

import static org.junit.Assert.assertTrue;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.ship.shipdata.ShipDataAgent;

public class ShipDataAgentTest {
    @Test
	public void testShipDataAgent() {
		JSONObject jo = new JSONObject();
		Region.putRegionAndStation(jo,2);
		AgentCaller.executeGetWithJsonParameter("JPS_SHIP/ShipDataAgent", jo.getJSONObject("region").toString());
	}

    @Test
    public void testValidateInput() {
    	JSONObject jo = new JSONObject();
		Region.putRegionAndStation(jo,2);
		JSONObject region = jo.getJSONObject("region");
		ShipDataAgent sd = new ShipDataAgent();
		assertTrue(sd.validateInput(region));
    }
}
