package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import static org.junit.Assert.assertTrue;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.virtualsensor.agents.ShipDataAgent;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;

public class ShipDataAgentTest {
    @Test
	public void testShipDataAgent() {
    	JSONObject jo = new JSONObject();
		jo.put(DispSimSparql.SimKey, "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5");
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/ShipDataAgent", jo.toString());
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
