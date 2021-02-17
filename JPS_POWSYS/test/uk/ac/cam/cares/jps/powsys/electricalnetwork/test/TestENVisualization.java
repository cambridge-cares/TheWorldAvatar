package uk.ac.cam.cares.jps.powsys.electricalnetwork.test;

import java.io.IOException;

import javax.xml.transform.TransformerException;

import org.apache.jena.ontology.OntModel;
import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.envisualization.ENVisualization;

public class TestENVisualization extends TestCase {
	private static ENVisualization a = new ENVisualization();
	private static String electricalnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	private OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
	public void testcreateKML() throws TransformerException {
		String b= a.createfinalKML(model);
	}
	public void testcreateLineJS() throws IOException {
		String res=a.createLineJS(model);
		JSONObject jo = new JSONObject(res);
		assertTrue(jo.has("result"));
		
	}
	public void testreadGenerator() throws IOException { //pre requisite : need to prepare http://localhost:8080/jps/kb/cdb65d7d-31af-43e2-88da-1462d8a1dc23/nuclearpowerplants/NucGenerator_1_B0.owl#NucGenerator_1_B0
		String res = "";
		res=a.readGenerator(model);
		System.out.println("resultjs= "+res);
		JSONObject jo = new JSONObject(res);
		assertTrue(jo.has("actual"));
		assertTrue(jo.has("design"));
	}
	public void testcreateMarkers() throws IOException {
		String res= a.createMarkers(model);
		System.out.println(res);
	}
	public void testJPSScenario() throws IOException{//temporary method. Pass value of usecaseurl according to what you have. 
		//read the top node by using jps/scenario
		String scenarioname =  "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario";
		String iri = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork", iri);
        jo.put("scenarioresource",iri);
	}
	public void testcallVisualizationLineJS() throws IOException  {
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork",electricalnetwork );
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/createLineJS", jo.toString());
		System.out.println("resultStart= "+resultStart);
	}
	public static void testcallVisualizationMarker() throws IOException  {

		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork",electricalnetwork);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/createMarkers", jo.toString());
		System.out.println("resultStart= "+resultStart);
	}
	public static void testcallreadGenerator() throws IOException  {

		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork",electricalnetwork);
//		jo.put("flag", "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario");
		jo.put("flag","BASE");
//		jo.put("selectedID", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-009.owl#EGen-009");
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/readGenerator", jo.toString());
		System.out.println("resultStart= "+resultStart);
	}
	
	public void testcallVisualizationKML() throws IOException  {

		JSONObject jo = new JSONObject();
		
		jo.put("electricalnetwork", electricalnetwork);
		jo.put("n", "1505");
		jo.put("flag", "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario");
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/createKMLFile", jo.toString());
		System.out.println("resultStart= "+resultStart);
	}
	
	
	public void testKMLFile() {
		
		String url = "http://www.theworldavatar.com/OntoEN/test2.kml";
		
		String result = AgentCaller.executeGetWithURL(url);
		
		System.out.println(result);
		
	}
	/** test validateInput() of ENVisualization
	 * 
	 */
	public void testInputValidatorENVisualization() {
		JSONObject jo = new JSONObject().put("electricalnetwork", electricalnetwork);
		assertTrue(a.validateInput(jo));
	}
	/** Present Generator markers via Scenario Client to get individual displays through agent
	 * TODO: Figure out how to call via Scenario Client because it's still giving the base Value
	 */
	public void testScenarioAgentCallerENVisualization() {
		JSONObject jo = new JSONObject().put("path","localhost:8080/ENVisualization/createMarkers" );
		jo.put("electricalnetwork", electricalnetwork);
		jo.put("path", "localhost:8080/JPS_POWSYS/ENVisualization/createMarkers");
//		String result = new ScenarioClient()
//				.call(ENVisualization.SCENARIO_NAME_TEST,
//						"http://localhost:8080/JPS_POWSYS/ENVisualization/createMarkers", jo.toString());
//		JSONObject res = new JSONObject(result);
//		JSONArray v = res.getJSONArray("result");
//		assertNotNull(v);
//		assertTrue(v.length()>1);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/createMarkers", jo.toString());
		JSONObject resS = new JSONObject(resultStart );
		JSONArray vS = resS.getJSONArray("result");
		assertNotNull(vS);
		assertTrue(vS.length()>1);
	}
	
	
	
	
}
