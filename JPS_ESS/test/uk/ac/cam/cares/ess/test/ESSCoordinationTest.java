package uk.ac.cam.cares.ess.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.ess.coordination.CoordinationESSAgent;

public class ESSCoordinationTest {

	private String ENIRI=null;
	private String pvGenIRI= null;
	private List<String>pvgeniris= null;
	private String batIRI=null;
	private String usecaseID =null;
	
	@Before
	public void setUp() {
		ENIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		pvGenIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-001.owl#PV-001";
		pvgeniris= new ArrayList<String>();
		batIRI="http://www.theworldavatar.com/kb/batterycatalog/BatteryCatalog.owl#BatteryCatalog";
		usecaseID = UUID.randomUUID().toString();
	}
	
	/** test validateInput() of Energy Storage System Coordination Agent
	 * 
	 */
	@Test
	public void testValidateInputCoordinationESSAgent() {
		JSONObject requestParam = new JSONObject().put("electricalnetwork", ENIRI);
		List<String> lstJA = new ArrayList<String>();
		lstJA.add(pvGenIRI);
		JSONArray ja = new JSONArray(lstJA);
        requestParam.put("RenewableEnergyGenerator", ja);
		assertTrue(new CoordinationESSAgent().validateInput(requestParam));
	}
	
	/** calls ESSCoordinate through Agent
	 * 
	 * @throws JSONException
	 */
	@Test
	public void testCreateScenarioAndCallESSCoordinate() throws JSONException {
		
		String scenarioName = "testESSTRIAL01"+usecaseID;	
		JSONObject jo = new JSONObject();
		pvgeniris.add(pvGenIRI);
		jo.put("electricalnetwork", ENIRI);
		jo.put("BatteryCatalog", batIRI);
		jo.put("RenewableEnergyGenerator", pvgeniris);
		String result = new ScenarioClient().call(scenarioName, "http://localhost:8080/JPS_ESS/startsimulationCoordinationESS", jo.toString());
		JSONObject testres = new JSONObject(result);
		assertEquals(batIRI, testres.getString("storage"), "http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB");
		JSONArray ja = testres.getJSONArray("batterylist" );
		assertTrue(ja.length() > 0);
	}
	
}
