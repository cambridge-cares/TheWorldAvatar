package uk.ac.cam.cares.ess.test;

import java.io.IOException;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.ess.EnergyStorageSystem;


public class EnergyStorageSystemTest extends TestCase {
	
	public static String ELECTRICAL_NETWORK = "http://www.jparksimulator.com/kb/sgp/pvsingaporenetwork/PVSingaporeNetwork.owl#PVSingaporeNetwork";
//	String dataPath = QueryBroker.getLocalDataPath();
//	String baseUrl=dataPath+"/JPS_ESS";

	private String modelname="NESS.gms";
	private String ENIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	
	public void xxxtestGAMSRun() throws IOException, InterruptedException { //only to test the gums code if it's running automatically
		EnergyStorageSystem a = new EnergyStorageSystem();
		a.runGAMS("C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\c8e42983-320e-4748-84e0-a49b7628b9db");
	}

	
	public void testreadsolutionstocsv() {
		//String outputfiledir="C:/JPS_DATA/workingdir/JPS_SCENARIO/scenario/base/localhost_8080/data/eb00c933-2513-4b8a-8eac-29b6304bf184/solutions.csv";
		String outputfiledir = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir" + "/solutions.csv";
		List<Double[]> simulationResult=new EnergyStorageSystem().readOutput(outputfiledir);

		System.out.println("simulation element = "+simulationResult.size());
		assertEquals(0.01,simulationResult.get(0)[0]);
		assertEquals(0.53,simulationResult.get(1)[1]);
		assertEquals(61.0,simulationResult.get(0)[2]);
		//ArrayList<String>removedplant=new ArrayList<String>();
		JSONObject result = new JSONObject();
	}
	
	public void testgetbatterylocmethod() throws IOException {
		String indexline ="34"; //--> index no 34
		String baseUrl="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\05ad27f6-afd3-4fed-8989-e7c1141029aa\\JPS_POWSYS_EN";
	    EnergyStorageSystem c=new EnergyStorageSystem();		
		OntModel model = c.readModelGreedy(ENIRI);
		double[]coordinate=c.prepareBatteryLocationData(indexline, baseUrl, model);
		assertEquals(103.70840835, coordinate[0], 0.001);
		assertEquals(1.2723166665, coordinate[1], 0.001);
	}
	
	public void testCreateOWLFile() throws IOException {
		String dir="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\05ad27f6-afd3-4fed-8989-e7c1141029aa\\JPS_POWSYS_EN";
		String resultofbattery="http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
		JSONObject result=new JSONObject();
		result.put("battery",resultofbattery);
		EnergyStorageSystem c=new EnergyStorageSystem();
		JSONArray a= c.createBatteryOwlFile(ENIRI, result, dir);
		assertEquals("http://www.jparksimulator.com/kb/batterycatalog/VRB-002.owl", a.get(0));
	}
	
	public void testoptimizedbattery() throws IOException { //to test the execution of gams model
		String batIRI="http://www.theworldavatar.com/kb/batterycatalog/BatteryCatalog.owl#BatteryCatalog";
		String pvGenIRI="http://www.jparksimulator.com/kb/sgp/pvsingaporenetwork/PV1.owl#PV1";
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		JSONObject testres= new EnergyStorageSystem ().optimizedBatteryMatching(baseUrl, pvGenIRI, batIRI);
		System.out.println("result battery= "+testres.getString("battery"));
		
		
	}

	public void unfinishedtestStartSimulationPFAgentCallBaseScenario() throws IOException  {
		

		JSONObject jo = new JSONObject();
		
		jo.put("PVNetwork", ELECTRICAL_NETWORK);
		
		//String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSENSimulationPFCallAgent");
		//JPSHttpServlet.enableScenario(scenarioUrl);	
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);		
		//jo.put(JPSConstants.SCENARIO_URL, scenarioUrl);
		
		//String usecaseUrl = BucketHelper.getUsecaseUrl();
		//JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		//jo.put(JPSConstants.SCENARIO_USE_CASE_URL,  usecaseUrl);
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_ESS/ESSAgent", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");
	}
	
	
	public void testCreateCSV() throws IOException  {
		String batIRI="http://www.theworldavatar.com/kb/batterycatalog/BatteryCatalog.owl#BatteryCatalog";
		EnergyStorageSystem a = new EnergyStorageSystem();
		OntModel modelbattery=a.readBatteryGreedy(batIRI);
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		a.prepareCSVPahigh("http://www.jparksimulator.com/kb/sgp/pvsingaporenetwork/PV1.owl#PV1", baseUrl);	
		a.prepareCSVRemaining(batIRI,baseUrl,modelbattery);
	}
	
	
	@SuppressWarnings("static-access")
	public void testModifyTemplate() throws IOException, InterruptedException{
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		EnergyStorageSystem a = new EnergyStorageSystem();
//		a.runGAMS("D:\\Users\\LONG01\\Documents\\gamsdir\\projdir") ;
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
}
