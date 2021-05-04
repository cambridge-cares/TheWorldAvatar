package uk.ac.cam.cares.ess.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.List;

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
	OntModel model = null;
	@Before
	public void setUp() {
		storageIRI = "http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
		ENIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		model = EnergyStorageSystem.readModelGreedy(ENIRI);
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

	@Test
	public void testprepareSelectedBranch() {

		List<String[]> SelectedBranch=new BatteryEntityCreator().prepareSelectedBranch(model,0.3);
		System.out.println("size= "+SelectedBranch.size()); //Goes from 7 - 9 depending on the electric grid
		assertTrue(SelectedBranch.size() >= 7);
		assertTrue(SelectedBranch.size() < 10); //If it goes more than 10, then your electric grid has grown out of control
			
	}
	
	
	@Test
	public void testprepareStorageLocation() {
		String busIRI1 = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-149.owl#EBus-149";
		String busIRI2 = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-011.owl#EBus-011";
		double[] midPoint = new BatteryEntityCreator().prepareStorageLocation(model, busIRI1,busIRI2);
		assertEquals(midPoint[0],103.684375, 0.001 );
		assertEquals(midPoint[1], 1.279125, 0.001);
	}
	
	/** test createBatteryOwlFile() of BatteryEntityCreator
	 * 
	 */
	@Test
	public void testBatteryEntityCreator() throws IOException{
		JSONArray listbat;
		Double valueboundary=0.3; //later is extracted from the battery type
		
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
