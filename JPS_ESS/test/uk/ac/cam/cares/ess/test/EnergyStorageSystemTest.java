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
	
	public void testModifyTemplate() throws IOException, InterruptedException{
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
	
//	public void xxxtestgetbatterylocmethod() throws IOException {
//		String indexline ="34"; //--> index no 34
//		String baseUrl="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\123621a1-a8c8-4527-9268-0e132e483082\\JPS_POWSYS_EN";
//	    EnergyStorageSystem c=new EnergyStorageSystem();		
//		OntModel model = c.readModelGreedy(ENIRI);
//		double[]coordinate=c.prepareBatteryLocationData(indexline, baseUrl, model);
//		assertEquals(103.70840835, coordinate[0], 0.001);
//		assertEquals(1.2723166665, coordinate[1], 0.001);
//	}
	
//	public void xxxtestCreateOWLFile() throws IOException {
//
//		String dir="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\123621a1-a8c8-4527-9268-0e132e483082\\JPS_POWSYS_EN";
//		String resultofbattery="http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
//		JSONObject result=new JSONObject();
//		result.put("storage",resultofbattery);
//	
//		EnergyStorageSystem c=new EnergyStorageSystem();
//		JSONArray a= c.createBatteryOwlFile(ENIRI, result, dir);
//		//assertEquals("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/VRB-001.owl", a.get(0));
//	}
	
	
	
	public void testoptimizedbattery() throws IOException { //to test the execution of gams model
		
		
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		pvgeniris.add(pvGenIRI);
		JSONObject testres= new EnergyStorageSystem ().optimizedBatteryMatching(baseUrl, pvgeniris, batIRI);
		System.out.println("result battery= "+testres.getString("storage"));
		pvgeniris.clear();
		assertEquals("http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB", testres.getString("storage"));
		
	}

	public void xxxtestStartSimulationESSScenario() throws IOException  { //need to provide which EN folder to get bat location
		

		JSONObject jo = new JSONObject();
		pvgeniris.add(pvGenIRI);
		jo.put("electricalnetwork", ENIRI);
		jo.put("BatteryCatalog", batIRI);
		jo.put("RenewableEnergyGenerator", pvgeniris);
		String scenarioname="testBatteryESSfin3"+usecaseID;
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioname);
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		JPSHttpServlet.enableScenario(scenarioUrl,usecaseUrl);
	
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_ESS/ESSAgent", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");
		pvgeniris.clear();
	}
	

public void testCoordinationStartSimulationESSScenario() throws IOException  {
	

	JSONObject jo = new JSONObject();
	pvgeniris.add(pvGenIRI);
	jo.put("electricalnetwork", ENIRI);
	jo.put("BatteryCatalog", batIRI);
	jo.put("RenewableEnergyGenerator", pvgeniris);
	String scenarioname="testBatteryESSfin4"+usecaseID;
	String scenarioUrl = BucketHelper.getScenarioUrl(scenarioname);
	//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
	JPSHttpServlet.enableScenario(scenarioUrl);
	JPSContext.putScenarioUrl(jo, scenarioUrl);
	System.out.println("contextusecase= "+JPSContext.getUsecaseUrl());
	System.out.println("contextuse case from jo= "+JPSContext.getUsecaseUrl(jo));
	String usecaseUrl = BucketHelper.getUsecaseUrl();
	JPSContext.putUsecaseUrl(jo, usecaseUrl); //if not using put usecase, each different simulation even in 1 coordination will be separated


	System.out.println(jo.toString());
	String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_ESS/startsimulationCoordinationESS", jo.toString());
	System.out.println(resultStart);
	System.out.println("finished execute");
	System.out.println("USECASE URL="+usecaseUrl);
	System.out.println("SCENARIO URL="+scenarioUrl);
	pvgeniris.clear();
}

public void testCreateScenarioAndCallESSAgent() throws JSONException {
	
	String scenarioName = "testESSTRIAL01"+usecaseID;	
	JSONObject jo = new JSONObject();
	pvgeniris.add(pvGenIRI);
	jo.put("electricalnetwork", ENIRI);
	jo.put("BatteryCatalog", batIRI);
	jo.put("RenewableEnergyGenerator", pvgeniris);
	String result = new ScenarioClient().call(scenarioName, "http://localhost:8080/JPS_ESS/startsimulationCoordinationESS", jo.toString());
	
	System.out.println(result);
	
}
public void testCallENAgentFromCreatedWorld() throws JSONException {
	
	String scenarioName = "testESSTRIAL01";
	
	JSONObject jo = new JSONObject();
	pvgeniris.add(pvGenIRI);
	jo.put("electricalnetwork", ENIRI);
	String result = new ScenarioClient().call(scenarioName, "http://localhost:8080/JPS_POWSYS/ENAgent/startsimulationOPF", jo.toString());
	
	System.out.println(result);
	
}

	
	public void testCreateCSV() throws IOException  {
		//String batIRI="http://www.theworldavatar.com/kb/batterycatalog/BatteryCatalog.owl#BatteryCatalog";
		EnergyStorageSystem a = new EnergyStorageSystem();
		OntModel modelbattery=EnergyStorageSystem.readBatteryGreedy(batIRI);
		pvgeniris.add(pvGenIRI);
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		a.prepareCSVPahigh(pvgeniris, baseUrl);	
		a.prepareCSVRemaining(batIRI,baseUrl);
		pvgeniris.clear();
	}
	
	
	
}
