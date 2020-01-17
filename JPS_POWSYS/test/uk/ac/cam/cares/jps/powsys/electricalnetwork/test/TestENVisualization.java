package uk.ac.cam.cares.jps.powsys.electricalnetwork.test;

import java.io.IOException;

import javax.xml.transform.TransformerException;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.envisualization.ENVisualization;

public class TestENVisualization extends TestCase {

	public void testcreateKML() throws TransformerException {
		ENVisualization a=new ENVisualization();
		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		String b= a.createfinalKML(model);
	}
	public void testcreateLineJS() throws IOException {
		ENVisualization a=new ENVisualization();
		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		String res=a.createLineJS(model);
		JSONObject jo = new JSONObject(res);
		assertTrue(jo.has("result"));
		
	}
	public void testreadGenerator() throws IOException { //pre requisite : need to prepare http://localhost:8080/jps/kb/cdb65d7d-31af-43e2-88da-1462d8a1dc23/nuclearpowerplants/NucGenerator_1_B0.owl#NucGenerator_1_B0
		ENVisualization a=new ENVisualization();
		String res = "";
		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		res=a.readGenerator(model);
		System.out.println("resultjs= "+res);
		JSONObject jo = new JSONObject(res);
		assertTrue(jo.has("actual"));
		assertTrue(jo.has("design"));
	}
	public void testcreateMarkers() throws IOException {
		ENVisualization a=new ENVisualization();
		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		String res= a.createMarkers(model);
		System.out.println(res);
	}
	public void testJPSScenario() throws IOException{
		//read the top node by using jps/scenario
		String scenarioname =  "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario";
		String iri = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		String url2 = "http://localhost:8080"+ "/jps/scenario/" + scenarioname + "/query?query=";
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork", iri);
        jo.put("scenarioresource",iri);
	}
	public void testcallVisualizationLineJS() throws IOException  {
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/createLineJS", jo.toString());
		System.out.println("resultStart= "+resultStart);
	}
	public static void testcallVisualizationMarker() throws IOException  {

		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork","http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
//		jo.put("flag","BASE");
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/createMarkers", jo.toString());
		System.out.println("resultStart= "+resultStart);
	}
	public static void testcallreadGenerator() throws IOException  {

		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork","http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
//		jo.put("flag", "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario");
		jo.put("flag","BASE");
//		jo.put("selectedID", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-009.owl#EGen-009");
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/readGenerator", jo.toString());
		System.out.println("resultStart= "+resultStart);
	}
	
	public void testcallVisualizationKML() throws IOException  {

		JSONObject jo = new JSONObject();
		
		jo.put("electricalnetwork", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
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
	
	
	
	
}
