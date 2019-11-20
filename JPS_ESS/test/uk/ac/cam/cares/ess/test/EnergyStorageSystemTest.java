package uk.ac.cam.cares.ess.test;

import java.io.IOException;
import java.util.List;

import org.apache.jena.ontology.OntModel;
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
	public void xxxtestGAMSRun() throws IOException, InterruptedException { //only to test the gums code if it's running automatically
		EnergyStorageSystem a = new EnergyStorageSystem();
		a.runGAMS("C:/JPS_DATA/workingdir/JPS_SCENARIO/scenario/base/localhost_8080/data/91ecce2b-c758-4d7b-883c-e9e11e7d569b");
	}

	
	public void testreadsolutionstocsv2() {
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
	
	public void testcreateOwlFilemethod() throws IOException {
		String indexline ="34"; //--> index no 34
		String capacity="40";
		String baseUrl="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\testPOWSYSENSimulationOPFDirectCall\\localhost_8080\\data\\8b2c4234-4e6f-44d2-9d27-be5497d42794\\JPS_POWSYS_EN";
	    EnergyStorageSystem c=new EnergyStorageSystem();		
		OntModel model = c.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		c.createOwlFile(indexline, capacity, baseUrl, model);
	}
	
	

	public void testStartSimulationPFAgentCallBaseScenario() throws IOException  {
		

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

		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		new EnergyStorageSystem().prepareCSVPahigh(ELECTRICAL_NETWORK, baseUrl);	
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
