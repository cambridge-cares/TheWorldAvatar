package uk.ac.cam.cares.jps.powsys.nuclear.test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.nuclear.IriMapper;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearAgent;
import uk.ac.cam.cares.jps.powsys.nuclear.IriMapper.IriMapping;

public class TestNuclear extends TestCase {

	public void testStartSimulationAndProcessResultDirectCallForBaseScenario() throws NumberFormatException, IOException, URISyntaxException, InterruptedException {
		NuclearAgent agent = new NuclearAgent();
		
		String lotiri = "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		String dataPath = QueryBroker.getLocalDataPath();
		agent.startSimulation(lotiri, iriofnetwork, dataPath, false);
		
		// copy existing result file from a previous simulation to the data bucket 
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/results.csv";
		File file = new File(source);
		String destinationUrl = dataPath + "/" + NuclearAgent.AGENT_TAG + "/results.csv";
		new QueryBroker().put(destinationUrl, file);
		
		List<String> result = agent.processSimulationResult(dataPath);
		System.out.println("result from processsimulationresult=" + result);
		assertEquals(4, result.size());
	}
	
	public void testStartSimulationAndProcessResultAgentCallForTestScenario() throws NumberFormatException, IOException, URISyntaxException, InterruptedException {
		
		JSONObject jo = new JSONObject();
		jo.put("landlot", "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario"); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl();
		//usecaseUrl = "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/testStartSimulationAndProcessResultAgentCallForTestScenario/kb/d9fbd6f4-9e2f-4c63-9995-9ff88ab8900e";
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put(JPSConstants.RUN_SIMULATION, false);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		
		System.out.println("json input parameter=" + jo);
		// start simulation (since parameter JPSConstants.SCENARIO_USE_CASE_URL is set, GAMS is not started)
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/startsimulation", jo.toString());
		System.out.println("result from startsimulation=" + resultStart);
		
		// copy existing result file from a previous simulation to the data bucket 
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/results.csv";
		File file = new File(source);
		String destinationUrl = QueryBroker.getLocalDataPath() + "/" + NuclearAgent.AGENT_TAG + "/results.csv";
		new QueryBroker().put(destinationUrl, file);
		
		// process the simulation result
		jo = new JSONObject();
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		String resultProcess = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/processresult", jo.toString());
		System.out.println("result from processsimulationresult=" + resultProcess);
		jo = new JSONObject(resultProcess);
		assertEquals(4, jo.getJSONArray("plants").length());
	}
	
	public void testcallNewNuclearAgent() throws IOException, InterruptedException, NumberFormatException, URISyntaxException {
		JSONObject result = new JSONObject();
		JSONArray ja = new JSONArray();
		ja.put("http://www.theworldavatar.com/kb/powerplants/Keppel_Merlimau_Cogen_Power_Plant_Singapore.owl#Keppel_Merlimau_Cogen_Power_Plant_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore.owl#SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore.owl#Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore.owl#PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore");
		result.put("substitutionalpowerplants", ja);
		NuclearAgent agent = new NuclearAgent();
		
		String lotiri = "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		
		ArrayList<String> listofplant= new ArrayList<String>();
		for (int c=0;c<result.getJSONArray("substitutionalpowerplants").length();c++) {
			listofplant.add(result.getJSONArray("substitutionalpowerplants").getString(c));
		}
		
		String dataPath = QueryBroker.getLocalDataPath();
		System.out.println("what is dataPath="+dataPath);
		//agent.startSimulation(lotiri, iriofnetwork,listofplant, dataPath, false);
		prepareCSVPartialRemaining(listofplant,iriofnetwork,dataPath);
		

	}
	
	public void prepareCSVPartialRemaining(ArrayList<String>plantlist,String iriofnetwork,String baseUrl) throws IOException {
		OntModel model = ENAgent.readModelGreedy(iriofnetwork);
		String genplantinfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?entity ?plant ?Pmaxvalue ?xval ?yval "
				+ "WHERE {?entity  a  j1:PowerGenerator  ." 
				+ "?entity   j2:isSubsystemOf ?plant ."
				+ "?entity   j2:isModeledBy ?model ."
				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ." 
				+ "?pmax  j2:hasValue ?vpmax ."
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax
				+ "?entity   j7:hasGISCoordinateSystem ?coordsys ."
				+ "?coordsys   j7:hasProjectedCoordinate_x ?xent ."
				+ "?xent j2:hasValue ?vxent ."
				+ "?vxent   j2:numericalValue ?xval ." // xvalue
				+ "?coordsys   j7:hasProjectedCoordinate_y ?yent ."
				+ "?yent j2:hasValue ?vyent ."
				+ "?vyent   j2:numericalValue ?yval ." // xvalue
				+ "}";
		
		ResultSet resultSet = JenaHelper.query(model, genplantinfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		int x=0;
		double sumcapreplaced=0;
		List<String[]> csvresult= new ArrayList<String[]>();
		String[]header= {"type","capacity","x","y"};
		csvresult.add(header);
		
		while(x<resultList.size()) {
			if(!plantlist.contains(resultList.get(x)[1])) {
				System.out.println("generator remains= "+resultList.get(x)[0]);
				System.out.println("P max= "+resultList.get(x)[2]);
				System.out.println("x= "+resultList.get(x)[3]);
				System.out.println("y= "+resultList.get(x)[4]);
				String[]content= new String[4];
				content[0]="c"+x;
				content[1]=resultList.get(x)[2];
				content[2]=resultList.get(x)[3];
				content[3]=resultList.get(x)[4];
				csvresult.add(content);
				
			}
			else {
				sumcapreplaced=sumcapreplaced+Double.valueOf(resultList.get(x)[2]);
			}

			
			x++;
		}
		System.out.println("sum replaced= "+sumcapreplaced);
		String[]content2= new String[4];
		content2[0]="n";
		content2[1]=""+sumcapreplaced;
		content2[2]="0.0";
		content2[3]="0.0";
		csvresult.add(content2);
		 String s = MatrixConverter.fromArraytoCsv(csvresult);
		 QueryBroker broker = new QueryBroker();
		 broker.put(baseUrl + "/inputgeneratorselection.csv", s);
		
	}
}
