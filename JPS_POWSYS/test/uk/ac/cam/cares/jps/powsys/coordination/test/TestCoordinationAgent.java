package uk.ac.cam.cares.jps.powsys.coordination.test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.powsys.coordination.CoordinationAgent;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.test.TestEN;
import uk.ac.cam.cares.jps.powsys.retrofit.RetrofitAgent;

public class TestCoordinationAgent extends TestCase implements Prefixes, Paths {
	
	
	private void copy(String sourceScenarioName, String destinationScenarioName) throws IOException {
		
		String src = ScenarioHelper.getScenarioBucket(sourceScenarioName);
		String dest =  ScenarioHelper.getScenarioBucket(destinationScenarioName);
		File srcDir = new File(src);
		File destDir = new File(dest);
		
		FileUtils.deleteDirectory(destDir);
		FileUtils.copyDirectory(srcDir, destDir);
	}
	
	public JSONArray getNuclearPowerPlantsFromScenarioaasc5() {
		String plants = "{\"plants\":[\"http://localhost:8080/jps/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7/nuclearpowerplants/NucPP_4.owl#NucPP_4\",\"http://localhost:8080/jps/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7/nuclearpowerplants/NucPP_3.owl#NucPP_3\",\"http://localhost:8080/jps/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7/nuclearpowerplants/NucPP_1.owl#NucPP_1\",\"http://localhost:8080/jps/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7/nuclearpowerplants/NucPP_2.owl#NucPP_2\"]}";
		return new JSONObject(plants).getJSONArray("plants");
	}

	public void testCoordinatePFDirectCall() throws URISyntaxException, IOException {
		
		String scenarioName = "testPOWSYSCoordinatePF";
		copy("aasc5", scenarioName);
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		String electricalNetwork = TestEN.ELECTRICAL_NETWORK;
		List<String> nuclearPowerPlants = MiscUtil.toList(getNuclearPowerPlantsFromScenarioaasc5());

		new RetrofitAgent().retrofit(electricalNetwork, nuclearPowerPlants);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationPF", jo.toString());
	}
	
	public void testCoordinateOPFDirectCall() throws URISyntaxException, IOException {
		
		String scenarioName = "testPOWSYSCoordinateOPF";
		copy("aasc5", scenarioName);
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		String electricalNetwork = TestEN.ELECTRICAL_NETWORK;
		List<String> nuclearPowerPlants =  MiscUtil.toList(getNuclearPowerPlantsFromScenarioaasc5());

		new RetrofitAgent().retrofit(electricalNetwork, nuclearPowerPlants);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationOPF", jo.toString());
	}
	
	private int calculateNumberOfGenerators(String s, String searchpattern, int namelength) {
		
		System.out.println("\n\nscenario = " + JPSContext.getScenarioUrl());
		
		StringTokenizer t = new StringTokenizer(s, "\n");
		int countLines = 0;
		int countGen = 0;
		while (t.hasMoreTokens()) {
			countLines++;
			String line = t.nextToken();
			int i = line.indexOf(searchpattern);
			if (i >= 0) {
				countGen++;
				System.out.println(line.substring(i, i + namelength));
			}
			
		}
		System.out.println("count gen = " + countGen);
		System.out.println("count lines = " + countLines);
		
		return countGen;
	}
	
	/**
	 * First, run the test methods testCoordinateOPFDirectCall() and  testCoordinatePFDirectCall() 
	 * to create two scenarios with name testPOWSYSCoordinateOPF and testPOWSYSCoordinatePF, resp.
	 * 
	 * This method shows how the EN top node is queried for different scenarios, and checks the
	 * number of (modified) generators.
	 */
	public void testReadElectricalNetwork() {
		
		String scenarioName = JPSConstants.SCENARIO_NAME_BASE;
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName); 
		JPSHttpServlet.enableScenario(scenarioUrl, null);	
		String result = new QueryBroker().readFile(TestEN.ELECTRICAL_NETWORK);
		int countgen = calculateNumberOfGenerators(result, "#EGen-", 9);
		assertEquals(14, countgen);
		
		result = new QueryBroker().readFile(TestEN.ELECTRICAL_NETWORK);
		countgen = calculateNumberOfGenerators(result, "#NucGenerator", 18);
		assertEquals(0, countgen);
		
		
		scenarioName = "testPOWSYSCoordinateOPF";
		scenarioUrl = BucketHelper.getScenarioUrl(scenarioName); 
		JPSHttpServlet.enableScenario(scenarioUrl, null);	
		result = new QueryBroker().readFile(TestEN.ELECTRICAL_NETWORK);
		countgen = calculateNumberOfGenerators(result, "#EGen-", 9);
		// generator for slack bus only, all other generators have been removed
		assertEquals(1, countgen);

		result = new QueryBroker().readFile(TestEN.ELECTRICAL_NETWORK);
		countgen = calculateNumberOfGenerators(result, "#NucGenerator", 18);
		assertEquals(14, countgen);
		
		scenarioName = "testPOWSYSCoordinatePF";
		scenarioUrl = BucketHelper.getScenarioUrl(scenarioName); 
		JPSHttpServlet.enableScenario(scenarioUrl, null);	
		result = new QueryBroker().readFile(TestEN.ELECTRICAL_NETWORK);
		countgen = calculateNumberOfGenerators(result, "#EGen-", 9);
		// generator for slack bus only, all other generators have been removed
		assertEquals(1, countgen);
		
		result = new QueryBroker().readFile(TestEN.ELECTRICAL_NETWORK);
		countgen = calculateNumberOfGenerators(result, "#NucGenerator", 18);
		assertEquals(14, countgen);
	}
	
	public void testCoordinateOPFAgentCall() throws URISyntaxException, IOException {
		
		String scenarioName = "testPOWSYSCoordinateOPFAgentCall";
		copy("aasc5", scenarioName);
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
//		String scenarioUrlOfMockedAgent = "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aasc5";
//		jo.put("mergescenariourl", scenarioUrlOfMockedAgent);
		jo.put("plants", getNuclearPowerPlantsFromScenarioaasc5());
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/processresultwithopf", jo.toString());
		System.out.println("result = " + result);
	}
	
	public void testCoordinateStartSimulationDirectCall() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinateStartSimulationDirectCall");
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("carbontax", 52.0);
		jo.put("landlot", "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		jo.put(JPSConstants.RUN_SIMULATION, false);
		
		new CoordinationAgent().startSimulation(jo);
	}
	
	public void testCoordinateStartSimulation() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinateStartSimulation");
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("carbontax", 64.0);
		jo.put("landlot", "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/startsimulation", jo.toString());
		System.out.println(result);
	}
	
	public void testTmp() {
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario"); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		String iri = "http://localhost:8080/jps/kb/c434701e-f9a4-45a0-bb55-2a9dd192b1ce/nuclearpowerplants/NucGenerator_4_B0.owl#NucGenerator_4_B0";
		String result = new QueryBroker().readFile(iri);
		System.out.println(result);
		
		// for resources:
		
		// GET http://localhost:8080/jps/scenario/testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario/read?query={"scenarioresource":"http://localhost:8080/jps/kb/c434701e-f9a4-45a0-bb55-2a9dd192b1ce/nuclearpowerplants/NucGenerator_4_B0.owl#NucGenerator_4_B0"}
		// GET http://localhost:8080/jps/scenario/testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario/read?query=%7B%22scenarioresource%22%3A%22http%3A%2F%2Flocalhost%3A8080%2Fjps%2Fkb%2Fc434701e-f9a4-45a0-bb55-2a9dd192b1ce%2Fnuclearpowerplants%2FNucGenerator_4_B0.owl%23NucGenerator_4_B0%22%7D

		
		//  GET http://localhost:8080/jps/scenario/testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario/read?query={"scenarioresource":"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-183.owl#EBus-183"}
		
		//  GET http://localhost:8080/jps/scenario/testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario/read?query=%7B%22scenarioresource%22%3A%22http%3A%2F%2Fwww.jparksimulator.com%2Fkb%2Fsgp%2Fjurongisland%2Fjurongislandpowernetwork%2FEBus-183.owl%23EBus-183%22%7D
		
		
		// for agents:
		
		//  GET http://localhost:8080/JPS_POWSYS/startsimulation?query={
		//  	"landlot":"http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl",
		// 	 	"carbontax":64,
		//      "jpscontext":{"usecaseurl":"http://localhost:8080/jps/scenario/testPOWSYSCoordinateStartSimulation/kb/0bf1bd30-c738-421c-8c6d-738b44f79f27","scenariourl":"http://localhost:8080/jps/scenario/testPOWSYSCoordinateStartSimulation"},
		//      "electricalnetwork":"http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork"}

	}
	public void testTmp2() {
		String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
//				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>"
				+ "SELECT ?entity ?V_Pd ?V_Pdunit ?V_Pd_Gen ?V_Pd_Genunit ?V_Gd ?V_Gdunit ?V_Gd_Gen ?V_Gd_Genunit" 
				+ "?Gsvalue ?Bsvalue ?V_Vm ?V_Va ?V_Vaunit ?V_BaseKV ?V_BaseKVunit ?VMaxvalue ?VMaxvalueunit ?VMinvalue ?VMinvalueunit  ?valueofx ?valueofxunit ?valueofy ?valueofyunit "

				+ "WHERE {?entity  a  j1:BusNode  ." 
				+ "?entity   j2:isModeledBy ?model ."
				+ "?model   j5:hasModelVariable ?num ." 
				+ "?num  a  j3:BusNumber  ." 
				+ "?num  j2:hasValue ?vnum ."
				+ "?vnum   j2:numericalValue ?V_num ." // number

				+ "?model   j5:hasModelVariable ?Pd ." 
				+ "?Pd  a  j3:PdBus  ." 
				+ "?Pd  j2:hasValue ?vpd ."
				+ "?vpd   j2:numericalValue ?V_Pd ." // pd
//				+ "?vpd   j2:hasUnitOfMeasure ?V_Pdunit ." // unit

				+ "?model   j5:hasModelVariable ?PdGen ." 
				+ "?PdGen  a  j3:PdGen  ." 
				+ "?PdGen  j2:hasValue ?vpdgen ."
				+ "?vpdgen   j2:numericalValue ?V_Pd_Gen ." // pdgen
//				+ "?vpdgen   j2:hasUnitOfMeasure ?V_Pd_Genunit ." // unit
				
				+ "?model   j5:hasModelVariable ?Gd ." 
				+ "?Gd  a  j3:GdBus  ." 
				+ "?Gd  j2:hasValue ?vgd ."
				+ "?vgd   j2:numericalValue ?V_Gd ." // Gd
//				+ "?vgd   j2:hasUnitOfMeasure ?V_Gdunit ." // unit
				
				+ "?model   j5:hasModelVariable ?Gd_Gen ." 
				+ "?Gd_Gen  a  j3:GdGen  ." 
				+ "?Gd_Gen  j2:hasValue ?vgdgen ."
				+ "?vgdgen   j2:numericalValue ?V_Gd_Gen ." // Gdgen
//				+ "?vgdgen   j2:hasUnitOfMeasure ?V_Gd_Genunit ." // unit


				+ "?model   j5:hasModelVariable ?Gsvar ." 
				+ "?Gsvar  a  j3:Gs  ." 
				+ "?Gsvar  j2:hasValue ?vGsvar ."
				+ "?vGsvar   j2:numericalValue ?Gsvalue ." // Gs (has no unit)

				+ "?model   j5:hasModelVariable ?Bsvar ." 
				+ "?Bsvar  a  j3:Bs  ." 
				+ "?Bsvar  j2:hasValue ?vBsvar ."
				+ "?vBsvar   j2:numericalValue ?Bsvalue ." // Bs (has no unit)

				+ "?model   j5:hasModelVariable ?VM ." 
				+ "?VM  a  j3:Vm  ." 
				+ "?VM  j2:hasValue ?vVM ."
				+ "?vVM   j2:numericalValue ?V_Vm ." // Vm
//				+ "?vVM   j2:hasUnitOfMeasure ?V_Vmunit ." 

				+ "?model   j5:hasModelVariable ?VA ." 
				+ "?VA  a  j3:Va  ." 
				+ "?VA  j2:hasValue ?vVA ."
				+ "?vVA   j2:numericalValue ?V_Va ." // Va
//				+ "?vVA   j2:hasUnitOfMeasure ?V_Vaunit ." // unit

				+ "?model   j5:hasModelVariable ?BKV ." 
				+ "?BKV  a  j3:baseKV  ." 
				+ "?BKV  j2:hasValue ?vBKV ."
				+ "?vBKV   j2:numericalValue ?V_BaseKV ." // Base KV
//				+ "?vBKV   j2:hasUnitOfMeasure ?V_BaseKVunit ." // Base KV
				
				+ "?model   j5:hasModelVariable ?vmaxvar ." 
				+ "?vmaxvar  a  j3:VmMax  ."
				+ "?vmaxvar  j2:hasValue ?vvmaxvar ." 
				+ "?vvmaxvar   j2:numericalValue ?VMaxvalue ." // Vmax
//				+ "?vvmaxvar   j2:hasUnitOfMeasure ?VMaxvalueunit ." // Vmax

				+ "?model   j5:hasModelVariable ?vminvar ." 
				+ "?vminvar  a  j3:VmMin  ."
				+ "?vminvar  j2:hasValue ?vvminvar ." 
				+ "?vvminvar   j2:numericalValue ?VMinvalue ." // Vmin
//				+ "?vvminvar   j2:hasUnitOfMeasure ?VMinvalueunit ." // Vmin
				
				+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ." 
				+ "?y  j2:hasValue ?vy ." 
				+ "?vy  j2:numericalValue ?valueofy ."//longitude
//				+ "?vy  j2:hasUnitOfMeasure ?valueofyunit ."//longitude

				+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
				+ "?x  j2:hasValue ?vx ." 
				+ "?vx  j2:numericalValue ?valueofx ."//latitude
//				+ "?vx  j2:hasUnitOfMeasure ?valueofxunit ."//latitude
				

				+ "}";
		
		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
				+ "SELECT ?entity ?BusNumbervalue ?activepowervalue ?activepowervalueunit ?Q_Gen ?Q_Genunit ?Qmaxvalue ?Qminvalue ?Vgvalue ?mBasevalue "
				+ "?Pmaxvalue ?Pmaxvalueunit ?Pminvalue ?Pminvalueunit ?Pc1value ?Pc2value ?Qc1minvalue ?Qc1maxvalue "
				+ "?Qc2minvalue ?Qc2maxvalue ?Rampagcvalue ?Ramp10value ?Ramp30value ?Rampqvalue ?apfvalue "
				+ "?startupcostvalue ?shutdowncostvalue ?gencostnvalue ?gencostn1value ?gencostn2value ?gencostcvalue ?valueofx ?valueofxunit ?valueofy ?valueofyunit "

				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "?entity   j2:isModeledBy ?model ."

				+ "?model   j5:hasModelVariable ?num ." 
				+ "?num  a  j3:BusNumber  ." 
				+ "?num  j2:hasValue ?vnum ."
				+ "?vnum   j2:numericalValue ?BusNumbervalue ." // number

				+ "?model   j5:hasModelVariable ?Pg ." 
				+ "?Pg  a  j3:Pg  ." 
				+ "?Pg  j2:hasValue ?vpg ."
				+ "?vpg   j2:numericalValue ?activepowervalue ." // pg
				+ "?vpg   j2:hasUnitOfMeasure ?activepowervalueunit ." // pg

				+ "?model   j5:hasModelVariable ?Qg ." 
				+ "?Qg  a  j3:Qg  ." 
				+ "?Qg  j2:hasValue ?vqg ."
				+ "?vqg   j2:numericalValue ?Q_Gen ." // qg
				+ "?vqg   j2:hasUnitOfMeasure ?Q_Genunit  ." // qg

				+ "?model   j5:hasModelVariable ?qmax ." 
				+ "?qmax  a  j3:QMax  ." 
				+ "?qmax  j2:hasValue ?vqmax ."
				+ "?vqmax   j2:numericalValue ?Qmaxvalue ." // qmax

				+ "?model   j5:hasModelVariable ?qmin ." 
				+ "?qmin  a  j3:QMin  ." 
				+ "?qmin  j2:hasValue ?vqmin ."
				+ "?vqmin   j2:numericalValue ?Qminvalue ." // qmin

				+ "?model   j5:hasModelVariable ?Vg ." 
				+ "?Vg  a  j3:Vg  ." 
				+ "?Vg  j2:hasValue ?vVg ."
				+ "?vVg   j2:numericalValue ?Vgvalue ." // vg

				+ "?model   j5:hasModelVariable ?mbase ." 
				+ "?mbase  a  j3:mBase  ." 
				+ "?mbase  j2:hasValue ?vmbase ."
				+ "?vmbase   j2:numericalValue ?mBasevalue ." // mbase

				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ." 
				+ "?pmax  j2:hasValue ?vpmax ."
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax
				+ "?vpmax   j2:hasUnitOfMeasure ?Pmaxvalueunit ." // pmax

				+ "?model   j5:hasModelVariable ?pmin ." 
				+ "?pmin  a  j3:PMin  ." 
				+ "?pmin  j2:hasValue ?vpmin ."
				+ "?vpmin   j2:numericalValue ?Pminvalue ." // pmin
				+ "?vpmin   j2:hasUnitOfMeasure ?Pminvalueunit ." // pmin

				+ "?model   j5:hasModelVariable ?pc1 ." 
				+ "?pc1  a  j3:Pc1  ." 
				+ "?pc1  j2:hasValue ?vpc1 ."
				+ "?vpc1   j2:numericalValue ?Pc1value ." // pc1

				+ "?model   j5:hasModelVariable ?pc2 ." 
				+ "?pc2  a  j3:Pc2  ." 
				+ "?pc2  j2:hasValue ?vpc2 ."
				+ "?vpc2   j2:numericalValue ?Pc2value ." // pc2

				+ "?model   j5:hasModelVariable ?qc1min ." 
				+ "?qc1min  a  j3:QC1Min  ."
				+ "?qc1min  j2:hasValue ?vqc1min ." 
				+ "?vqc1min   j2:numericalValue ?Qc1minvalue ." // qc1min

				+ "?model   j5:hasModelVariable ?Qc1max ." 
				+ "?Qc1max  a  j3:QC1Max  ."
				+ "?Qc1max  j2:hasValue ?vQc1max ." 
				+ "?vQc1max   j2:numericalValue ?Qc1maxvalue ." // qc1max

				+ "?model   j5:hasModelVariable ?qc2min ." 
				+ "?qc2min  a  j3:QC2Min  ."
				+ "?qc2min  j2:hasValue ?vqc2min ."
				+ "?vqc2min   j2:numericalValue ?Qc2minvalue ." // qc2min

				+ "?model   j5:hasModelVariable ?Qc2max ."
				+ "?Qc2max  a  j3:QC2Max  ."
				+ "?Qc2max  j2:hasValue ?vQc2max ." 
				+ "?vQc2max   j2:numericalValue ?Qc2maxvalue ." // qc2max

				+ "?model   j5:hasModelVariable ?rampagc ." 
				+ "?rampagc  a  j3:Rampagc  ."
				+ "?rampagc  j2:hasValue ?vrampagc ." 
				+ "?vrampagc   j2:numericalValue ?Rampagcvalue ." // rampagc

				+ "?model   j5:hasModelVariable ?ramp10 ." 
				+ "?ramp10  a  j3:Ramp10  ."
				+ "?ramp10  j2:hasValue ?vramp10 ."
				+ "?vramp10   j2:numericalValue ?Ramp10value ." // ramp10

				+ "?model   j5:hasModelVariable ?ramp30 ." 
				+ "?ramp30  a  j3:Ramp30  ."
				+ "?ramp30  j2:hasValue ?vramp30 ." 
				+ "?vramp30   j2:numericalValue ?Ramp30value ." // ramp30

				+ "?model   j5:hasModelVariable ?rampq ." 
				+ "?rampq  a  j3:Rampq  ." 
				+ "?rampq  j2:hasValue ?vrampq ."
				+ "?vrampq   j2:numericalValue ?Rampqvalue ." // rampq

				+ "?model   j5:hasModelVariable ?apf ."
				+ "?apf  a  j3:APF  ." 
				+ "?apf  j2:hasValue ?vapf ."
				+ "?vapf   j2:numericalValue ?apfvalue ." // apf
				
				+ "?model   j5:hasModelVariable ?startup ." 
				+ "?startup  a  j3:StartCost  ."
				+ "?startup  j2:hasValue ?vstartup ." 
				+ "?vstartup   j2:numericalValue ?startupcostvalue ." //startup cost

				+ "?model   j5:hasModelVariable ?shutdown ." 
				+ "?shutdown  a  j3:StopCost  ."
				+ "?shutdown  j2:hasValue ?vshutdown ." 
				+ "?vshutdown   j2:numericalValue ?shutdowncostvalue ."  //shutdown cost
				
				+ "?model   j5:hasModelVariable ?gencostn ." 
				+ "?gencostn  a  j3:genCostn  ."
				+ "?gencostn  j2:hasValue ?vgencostn ." 
				+ "?vgencostn   j2:numericalValue ?gencostnvalue ." //genCostn

				+ "?model   j5:hasModelVariable ?gencostn1 ." 
				+ "?gencostn1  a  j3:genCostcn-1  ."
				+ "?gencostn1  j2:hasValue ?vgencostn1 ." 
				+ "?vgencostn1   j2:numericalValue ?gencostn1value ." //genCostn-1

				+ "?model   j5:hasModelVariable ?gencostn2 ." 
				+ "?gencostn2  a  j3:genCostcn-2  ."
				+ "?gencostn2  j2:hasValue ?vgencostn2 ." 
				+ "?vgencostn2   j2:numericalValue ?gencostn2value ."//genCostn-2


				+ "?model   j5:hasModelVariable ?gencostc ." 
				+ "?gencostc  a  j3:genCostc0  ."
				+ "?gencostc  j2:hasValue ?vgencostc ." 
				+ "?vgencostc   j2:numericalValue ?gencostcvalue ." //genCostc0

				+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ." 
				+ "?y  j2:hasValue ?vy ." 
				+ "?vy  j2:numericalValue ?valueofy ."//longitude
				+ "?vy  j2:hasUnitOfMeasure ?valueofyunit ."//longitude

				
				+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
				+ "?x  j2:hasValue ?vx ." 
				+ "?vx  j2:numericalValue ?valueofx ."//latitude
				+ "?vx  j2:hasUnitOfMeasure ?valueofxunit ."//latitude

				+ "}";
		String queryResult = new QueryBroker().queryFile("http://localhost:8080/jps/scenario/base/read?query=%7B%22scenarioresource%22%3A%22http%3A%2F%2Fwww.jparksimulator.com%2Fkb%2Fsgp%2Fjurongisland%2Fjurongislandpowernetwork%2FEGen-002.owl%23EGen-002.owl%22%7D", genInfo);
		System.out.println(queryResult);

//		String queryResult = new QueryBroker().queryFile("http://localhost:8080/jps/scenario/base/read?query=%7B%22scenarioresource%22%3A%22http%3A%2F%2Fwww.jparksimulator.com%2Fkb%2Fsgp%2Fjurongisland%2Fjurongislandpowernetwork%2FEBus-149.owl%23EBus-149.owl%22%7D", busInfo);	
//		List<String[]> resultGenAsList = JenaResultSetFormatter.convertToListofStringArrays(queryResult, "entity" , "V_Pd" , "V_Pdunit" , "V_Pd_Gen" , "V_Pd_Genunit" , "V_Gd" , "V_Gdunit" , "V_Gd_Gen" , "V_Gd_Genunit" , "Gsvalue" , "Bsvalue" , "V_Vm" , "V_Va" , "V_Vaunit" , "V_BaseKV" , "V_BaseKVunit" , "VMaxvalue" , "VMaxvalueunit" , "VMinvalue" , "VMinvalueunit" , "valueofx" , "valueofxunit" , "valueofy" , "valueofyunit");

		String[] keysplant = JenaResultSetFormatter.getKeys(queryResult);
    	List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(queryResult, keysplant);
//		for (int i =0; i < resultList.size(); i++) {
//			System.out.println(resultList.get(i)[0]);
//			System.out.println(resultList.get(i)[1]);
//			System.out.println(resultList.get(i)[2]);
//			System.out.println(resultList.get(i)[3]);
//			System.out.println(resultList.get(i)[4]);
//			System.out.println(resultList.get(i)[5]);
//			System.out.println(resultList.get(i)[6]);
//			
//		}
    	System.out.println(queryResult);
    	JSONObject json = new JSONObject(queryResult);
    	JSONObject v = (JSONObject) json.get("results");
    	JSONArray values = (JSONArray) v.get("bindings");
    	System.out.println(values.get(0));
	}
	
} 