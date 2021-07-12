package uk.ac.cam.cares.ess.test;

import static org.junit.Assert.assertTrue;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.ess.BatteryEntityCreator;

public class BatteryLocatorTest {
	/** test validateInput() of BatteryLocator
	 * 
	 */
	
	private String storageIRI = null;
	private String ENIRI=null;
	@Before
	public void setUp() {
		storageIRI = "http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
		ENIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
			
	}
	@Test
	public void testValidateInputBatteryLocator() {
		JSONObject jo = new JSONObject().put("electricalnetwork", ENIRI);
		jo.put("storage", storageIRI);
		assertTrue(new BatteryEntityCreator().validateInput(jo));
		
	}
	/** test BatteryLocator as an agent
	 * 
	 */
	@Test
	public void testBatteryLocatorAgentCall() {
		JSONObject jo = new JSONObject().put("electricalnetwork", ENIRI);
		jo.put("storage", storageIRI);
		String testres = AgentCaller.executeGetWithJsonParameter("JPS_ESS/LocateBattery" , jo.toString());
		JSONArray ja = new  JSONObject(testres).getJSONArray("batterylist" );
		assertTrue(ja.length() > 0);
	}
}
