package uk.ac.cam.cares.ess.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.apache.jena.ontology.OntModel;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.ess.BatteryEntityCreator;
import uk.ac.cam.cares.jps.ess.EnergyStorageSystem;

public class BatteryEntityCreatorTest {
	private String storageIRI = null;
	private String ENIRI=null;
	@Before
	public void setUp() {
		storageIRI = "http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
		ENIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
			
	}
	
	/** test validateInput() of BatteryEntityCreator
	 * 
	 */
	@Test
	public void testValidateInputBatteryEntityCreator() {
		JSONObject jo = new JSONObject().put("electricalnetwork", ENIRI);
		jo.put("storage", storageIRI);
		assertTrue(new BatteryEntityCreator().validateInput(jo));
		
	}
	
	/** test createBatteryOwlFile() of BatteryEntityCreator
	 * 
	 */
	@Test
	public void testBatteryEntityCreator() throws IOException{
		JSONArray listbat;
		Double valueboundary=0.3; //later is extracted from the battery type
		OntModel model = EnergyStorageSystem.readModelGreedy(ENIRI);
		
		listbat = new BatteryEntityCreator().createBatteryOwlFile(model, storageIRI,valueboundary);
		assertNotNull(listbat);
		
	}

	
	/** test BatteryEntityCreator as an agent
	 * 
	 */
	@Test
	public void testBatteryEntityCreatorAgentCall() {
		JSONObject jo = new JSONObject().put("electricalnetwork", ENIRI);
		jo.put("storage", storageIRI);
		String testres = AgentCaller.executeGetWithJsonParameter("JPS_ESS/CreateBattery" , jo.toString());
		JSONArray ja = new  JSONObject(testres).getJSONArray("batterylist" );
		assertTrue(ja.length() > 0);
	}
	

	

}
