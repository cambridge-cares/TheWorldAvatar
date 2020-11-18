package uk.ac.cam.cares.jps.ship.shipdata.test;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;

public class ShipDataAgentTest {
    @Test
	public void testShipDataAgent() {
		JSONObject jo = new JSONObject();
		Region.putRegionAndStation(jo,2);
		AgentCaller.executeGetWithJsonParameter("JPS_SHIP/ShipDataAgent", jo.toString());
	}
}
