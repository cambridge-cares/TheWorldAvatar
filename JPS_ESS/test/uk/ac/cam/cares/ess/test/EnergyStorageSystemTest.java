package uk.ac.cam.cares.ess.test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.jena.ontology.OntModel;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.ess.EnergyStorageSystem;
import uk.ac.cam.cares.jps.ess.coordination.CoordinationESSAgent;


public class EnergyStorageSystemTest extends TestCase {
	
//	String dataPath = QueryBroker.getLocalDataPath();
//	String baseUrl=dataPath+"/JPS_ESS";

	private String modelname="NESS.gms";
	private String ENIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	private String batIRI="http://www.theworldavatar.com/kb/batterycatalog/BatteryCatalog.owl#BatteryCatalog";
	private String pvGenIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-001.owl#PV-001";
	private String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\ESSTest";
	
	List<String>pvgeniris= new ArrayList<String>();
	String usecaseID = UUID.randomUUID().toString();
	
	public void xxxtestGAMSRun() throws IOException, InterruptedException { //only to test the gums code if it's running automatically
		EnergyStorageSystem a = new EnergyStorageSystem();
		a.runGAMS("C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\c8e42983-320e-4748-84e0-a49b7628b9db");
	}
	/** test the inputValidate method of Energy Storage System Coordination Agent
	 * 
	 */
	public void testValidateCoordinationESSAgent() {
		JSONObject requestParam = new JSONObject().put("electricalnetwork", ENIRI);
		List<String> lstJA = new ArrayList<String>();
		lstJA.add(pvGenIRI);
		JSONArray ja = new JSONArray(lstJA);
        requestParam.put("RenewableEnergyGenerator", ja);
		assertTrue(new CoordinationESSAgent().validateInput(requestParam));
	}
	/** test the inputValidate method of Energy Storage System (aka GAMS runner)
	 * 
	 */
	public void testValidateInputEnergyStorageSystem() {
		JSONObject requestParam = new JSONObject().put("electricalnetwork", ENIRI);
		requestParam.put("BatteryCatalog", batIRI);
		assertTrue(new EnergyStorageSystem().validateInput(requestParam));
		
		
	}
	/** test filterPV method of EnergyStorageSystem
	 * 
	 */
	public void testEnergyStorageSystemFilterPV() {
		List<String> ja = new EnergyStorageSystem().filterPV (ENIRI);
		assertNotNull(ja.size());
	}
	/** test prepareCSVPahigh of EnergyStorageSystem
	 * 
	 */
	public void testEnergyStorageSystemprepareCSVPahigh() {
		List<String> lstJA = new ArrayList<String>();
		lstJA.add(pvGenIRI);
		new EnergyStorageSystem(). prepareCSVPahigh( lstJA , baseUrl);
		File file = new File( baseUrl + "/Pa_high.csv");
		assertTrue(file.exists());
		assertTrue(file.length()> 0);
	}
	
	/** test prepareCSVRemaining of EnergyStorageSystem
	 * 
	 */
	public void testEnergyStorageSystemprepareCSVRemaining() {
		OntModel modelbattery=EnergyStorageSystem.readBatteryGreedy(batIRI);
		
		new EnergyStorageSystem(). prepareCSVRemaining( batIRI, baseUrl );
		File file = new File( baseUrl + "/EnvironmentalScore.csv");
		assertTrue(file.exists());
		assertTrue(file.length()> 0);
	}
	/** test readOutput of EnergyStorageSystem
	 * 
	 */
	public void testreadsolutionstocsv() {
		String outputfiledir = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir" + "/solutions.csv";
		List<Double[]> simulationResult=new EnergyStorageSystem().readOutput(outputfiledir);

		System.out.println("simulation element = "+simulationResult.size());
		assertEquals(0.01,simulationResult.get(0)[0]);
		assertEquals(0.53,simulationResult.get(1)[1]);
		assertEquals(61.0,simulationResult.get(0)[2]);
		//ArrayList<String>removedplant=new ArrayList<String>();
		JSONObject result = new JSONObject();
	}
	/** tests  modifyTemplate and runGAMS methods
	 * 
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public void testModifyTemplate() throws IOException, InterruptedException{
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		EnergyStorageSystem a = new EnergyStorageSystem();
		try {
			a.runGAMS(baseUrl);
		   }
		   catch (InterruptedException e) {
		      e.printStackTrace();
		   }
		catch (Exception e) {
			      e.printStackTrace();
			   }
	}
	/** test optimizedBatteryMatching() of EnergyStorageSystem ().
	 * 
	 * @throws IOException
	 */
	public void testoptimizedbattery() throws IOException {
		
		
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		pvgeniris.add(pvGenIRI);
		JSONObject testres= new EnergyStorageSystem ().optimizedBatteryMatching(baseUrl, pvgeniris, batIRI);
		System.out.println("result battery= "+testres.getString("storage"));
		pvgeniris.clear();
		assertEquals("http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB", testres.getString("storage"));
		
	}
	/** calls ESSCoordinate through Agent
	 * 
	 * @throws JSONException
	 */
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
