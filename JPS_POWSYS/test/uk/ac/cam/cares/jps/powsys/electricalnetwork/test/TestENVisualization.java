package uk.ac.cam.cares.jps.powsys.electricalnetwork.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.transform.TransformerException;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.envisualization.ENVisualization;
import uk.ac.cam.cares.jps.powsys.envisualization.ENVisualization.StaticobjectgenClass;
import uk.ac.cam.cares.jps.powsys.envisualization.MapPoint;

public class TestENVisualization extends TestCase {

	public void testcreateKML() throws TransformerException {
		ENVisualization a=new ENVisualization();
		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		List<String[]> generators=a.queryElementCoordinate(model, "PowerGenerator");
	
		//-----------------------------
		ArrayList<ENVisualization.StaticobjectgenClass> gensmerged = new ArrayList<ENVisualization.StaticobjectgenClass>();
		ArrayList<String> coorddata = new ArrayList<String>();
		for (int e = 0; e < generators.size(); e++) {
			StaticobjectgenClass gh = a.new StaticobjectgenClass();
			gh.setnamegen("/" + generators.get(e)[0]+ ".owl");
			gh.setx(generators.get(e)[1]);
			gh.sety(generators.get(e)[2]);
			System.out.println("/" + generators.get(e)[0].split("#")[1] + ".owl");

			if (coorddata.contains(gh.getx()) && coorddata.contains(gh.gety())) {
				int index = coorddata.indexOf(gh.getx()) / 2;
				gensmerged.get(index).setnamegen(gensmerged.get(index).getnamegen() + gh.getnamegen());
			} else {
				gensmerged.add(gh);
				coorddata.add(generators.get(e)[1]);
				coorddata.add(generators.get(e)[2]);
			}

		}		
		
		for(int g=0;g<gensmerged.size();g++) {
			MapPoint c= new MapPoint(Double.valueOf(gensmerged.get(g).gety()),Double.valueOf(gensmerged.get(g).getx()),0.0,gensmerged.get(g).getnamegen());
			a.addMark(c,"generator");
		}
		
		//--------------------------------
		
		List<String[]> bus=a.queryElementCoordinate(model, "BusNode");
		int size2=bus.size();
		for(int g=0;g<size2;g++) {
		MapPoint c= new MapPoint(Double.valueOf(bus.get(g)[2]),Double.valueOf(bus.get(g)[1]),0.0,"/"+bus.get(g)[0]+".owl");
		a.addMark(c,"bus");
		}
		
		String res=a.writeFiletoString();
		
	}
	
	public void testcreateLineJS() throws IOException {
		ENVisualization a=new ENVisualization();
		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		String res=a.createLineJS(model);
		System.out.println("resultjs= "+res);
	}
	public void testreadGenerator() throws IOException { //pre requisite : need to prepare http://localhost:8080/jps/kb/cdb65d7d-31af-43e2-88da-1462d8a1dc23/nuclearpowerplants/NucGenerator_1_B0.owl#NucGenerator_1_B0
		ENVisualization a=new ENVisualization();
		String flag = "base";

		String scenarioUrl = BucketHelper.getScenarioUrl(flag); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		String res = "";
		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		res=a.readGenerator(model);
		System.out.println("resultjs= "+res);
//		flag = "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario";
//		
//		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
//		res=a.readGenerator(model);
//
//		System.out.println("resultjs= "+res);
	}
	
	public void testTmp() throws IOException {
		ENVisualization a=new ENVisualization();
		String flag = "Base";
		String res = "";
		String url = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-015.owl#EGen-015";
		String sparqlQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
			    + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			    + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
			    + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			    + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			    + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
			    + "PREFIX technical_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
			    + "SELECT ?entity ?V_Actual_CO2_Emission ?V_Design_CO2_Emission "
			    
			    + "WHERE {?entity  a  j1:PowerGenerator  ."
			    + "?entity   technical_system:realizes ?generation ."
			    + "?generation j9:hasEmission ?emission ." 
			    
			    + "?emission a j9:Actual_CO2_Emission ."
			    + "?emission   j2:hasValue ?valueemission ."
			    + "?valueemission   j2:numericalValue ?V_Actual_CO2_Emission ." //

			    
			    + "?generation j9:hasEmission ?v_emission ." 
			    + "?v_emission a j9:CO2_emission ."
			    + "?v_emission   j2:hasValue ?valueemission_d ."
			    + "?valueemission_d   j2:numericalValue ?V_Design_CO2_Emission ." //
			    

			    + "}";
		
		res = new QueryBroker().queryFile(url, sparqlQuery);
		String[] keysplant = JenaResultSetFormatter.getKeys(res);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(res, keysplant);
		float actual = Float.valueOf(resultList.get(0)[1]);
		float design = Float.valueOf(resultList.get(0)[2]);
		System.out.println("resultjs= " + actual + design);
//		flag = "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario";
//		
//		String scenarioUrl = BucketHelper.getScenarioUrl(flag); 
//		JPSHttpServlet.enableScenario(scenarioUrl);	
//		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
//		res=a.readGenerator(flag, model,"http://www.jparksimulator.com/kb/sgp/jurongisland/nuclearpowerplants/NucGenerator_1_B0.owl#NucGenerator_1_B0");
		
	}
	
	public void testcreateMarkers() throws IOException {
		ENVisualization a=new ENVisualization();
//		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
//		String res=a.createMarkers("BASE",model);

		JPSHttpServlet.disableScenario();	
		String flag = "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario";
		String scenarioUrl = BucketHelper.getScenarioUrl(flag); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		OntModel model = ENAgent.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
//		
		String res= a.createMarkers(flag,model);
//		System.out.println(res);
		
	}
	public void testcallVisualizationLineJS() throws IOException  {

		JSONObject jo = new JSONObject();
		
		jo.put("electricalnetwork", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		jo.put("flag", "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario");
//		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/createLineJS", jo.toString());
//		jo = new JSONObject();
//		jo.put("electricalnetwork", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
//		jo.put("flag", "BASE");
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENVisualization/createLineJS", jo.toString());
//		System.out.println("resultStart= "+resultStart);
	}
	public static void testcallVisualizationMarker() throws IOException  {

		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork","http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		jo.put("flag", "testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario");
//		jo.put("flag","BASE");
//		JSONObject jo2 = new JSONObject();
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
