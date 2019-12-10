package uk.ac.cam.cares.ess.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.ess.EnergyStorageSystem;


public class EnergyStorageSystemTest extends TestCase {
	
	//public static String ELECTRICAL_NETWORK = "http://www.jparksimulator.com/kb/sgp/pvsingaporenetwork/PVSingaporeNetwork.owl#PVSingaporeNetwork";
//	String dataPath = QueryBroker.getLocalDataPath();
//	String baseUrl=dataPath+"/JPS_ESS";

	private String modelname="NESS.gms";
	private String ENIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	private String batIRI="http://www.theworldavatar.com/kb/batterycatalog/BatteryCatalog.owl#BatteryCatalog";
	private String pvGenIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-001.owl#PV-001";
	List<String>pvgeniris= new ArrayList<String>();
	
	
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
	
	public void xxxtestgetbatterylocmethod() throws IOException {
		String indexline ="34"; //--> index no 34
		String baseUrl="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\123621a1-a8c8-4527-9268-0e132e483082\\JPS_POWSYS_EN";
	    EnergyStorageSystem c=new EnergyStorageSystem();		
		OntModel model = c.readModelGreedy(ENIRI);
		double[]coordinate=c.prepareBatteryLocationData(indexline, baseUrl, model);
		assertEquals(103.70840835, coordinate[0], 0.001);
		assertEquals(1.2723166665, coordinate[1], 0.001);
	}
	
	public void xxxtestCreateOWLFile() throws IOException {

		String dir="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\123621a1-a8c8-4527-9268-0e132e483082\\JPS_POWSYS_EN";
		String resultofbattery="http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
		JSONObject result=new JSONObject();
		result.put("battery",resultofbattery);
	
		EnergyStorageSystem c=new EnergyStorageSystem();
		JSONArray a= c.createBatteryOwlFile(ENIRI, result, dir);
		//assertEquals("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/VRB-001.owl", a.get(0));
	}
	
	
	
	public void testoptimizedbattery() throws IOException { //to test the execution of gams model
		
		
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		pvgeniris.add(pvGenIRI);
		JSONObject testres= new EnergyStorageSystem ().optimizedBatteryMatching(baseUrl, pvgeniris, batIRI);
		System.out.println("result battery= "+testres.getString("battery"));
		pvgeniris.clear();
		assertEquals("http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB", testres.getString("battery"));
		
	}

	public void testStartSimulationESSScenario() throws IOException  {
		

		JSONObject jo = new JSONObject();
		pvgeniris.add(pvGenIRI);
		jo.put("electricalnetwork", ENIRI);
		jo.put("BatteryCatalog", batIRI);
		jo.put("RenewableEnergyGenerator", pvgeniris);
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testBatteryESSfin3");
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
	
	public void testStartSimulationESSScenarioVis() throws IOException {

		JSONObject jo = new JSONObject();
		pvgeniris.add(pvGenIRI);
		jo.put("electricalnetwork", ENIRI);
		jo.put("BatteryCatalog", batIRI);
		jo.put("RenewableEnergyGenerator", pvgeniris);

		String scenarioUrl = BucketHelper.getScenarioUrl("testBatteryESSfin3");
		// new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);

		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_ESS/ESSAgent", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");
		pvgeniris.clear();
	}
public static OntModel readModelGreedy(String iriofnetwork) {
	String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "SELECT ?component "
			+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

	QueryBroker broker = new QueryBroker();
	return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
}
public void testJPSPV() {
		JSONObject jo = new JSONObject();
		pvgeniris.add(pvGenIRI);
		jo.put("electricalnetwork", ENIRI);
		jo.put("BatteryCatalog", batIRI);
		jo.put("RenewableEnergyGenerator", pvgeniris);
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testBatt1");
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		JPSHttpServlet.enableScenario(scenarioUrl,usecaseUrl);
		
		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
			    + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			    + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
			    + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			    + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			    + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
			    + "PREFIX technical_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
			    + "SELECT ?entity ?V_BusNumber ?V_PGen ?V_PGen_unit ?V_QGen ?V_QGen_unit ?V_Qmax ?V_Qmax_unit ?V_Qmin ?V_Qmin_unit ?V_Vg ?V_mBase ?V_mBase_unit "
			    + "?V_Pmax ?V_Pmax_unit ?V_Pmin ?V_Pmin_unit ?V_Pc1 ?V_Pc2 ?V_Qc1Min ?V_Qc1Max "
			    + "?V_Qc2Min ?V_Qc2Max ?V_Ramp_agc ?V_Ramp_10 ?V_Ramp_30 ?V_Ramp_q ?V_APF "
			    + "?V_StartupCost ?V_ShutdownCost ?V_genCostn ?V_genCostn1 ?V_genCostn2 ?V_genCostc0 "

			    + "WHERE {?entity  a  j1:PowerGenerator  ."
			    + "?entity   j2:isModeledBy ?model ."

			    + "?model   j5:hasModelVariable ?num ." 
			    + "?num  a  j3:BusNumber  ." 
			    + "?num  j2:hasValue ?vnum ."
			    + "?vnum   j2:numericalValue ?V_BusNumber ." // number

			    + "?model   j5:hasModelVariable ?Pg ." 
			    + "?Pg  a  j3:Pg  ." 
			    + "?Pg  j2:hasValue ?vpg ."
			    + "?vpg   j2:numericalValue ?V_PGen ." // pg
			    + "?vpg   j2:hasUnitOfMeasure ?V_PGen_unit ." // pg

			    + "?model   j5:hasModelVariable ?Qg ." 
			    + "?Qg  a  j3:Qg  ." 
			    + "?Qg  j2:hasValue ?vqg ."
			    + "?vqg   j2:numericalValue ?V_QGen ." // qg
			    + "?vqg   j2:hasUnitOfMeasure ?V_QGen_unit ." // qg

			    + "?model   j5:hasModelVariable ?qmax ." 
			    + "?qmax  a  j3:QMax  ." 
			    + "?qmax  j2:hasValue ?vqmax ."
			    + "?vqmax   j2:numericalValue ?V_Qmax ." // qmax
			    + "?vqmax   j2:hasUnitOfMeasure ?V_Qmax_unit ." // qmax

			    + "?model   j5:hasModelVariable ?qmin ." 
			    + "?qmin  a  j3:QMin  ." 
			    + "?qmin  j2:hasValue ?vqmin ."
			    + "?vqmin   j2:numericalValue ?V_Qmin ." // qmin
			    + "?vqmin   j2:hasUnitOfMeasure ?V_Qmin_unit ." // qmin

			    + "?model   j5:hasModelVariable ?Vg ." 
			    + "?Vg  a  j3:Vg  ." 
			    + "?Vg  j2:hasValue ?vVg ."
			    + "?vVg   j2:numericalValue ?V_Vg ." // vg

			    + "?model   j5:hasModelVariable ?mbase ." 
			    + "?mbase  a  j3:mBase  ." 
			    + "?mbase  j2:hasValue ?vmbase ."
			    + "?vmbase   j2:numericalValue ?V_mBase ." // mbase
			    + "?vmbase   j2:hasUnitOfMeasure ?V_mBase_unit ." // mbase

			    + "?model   j5:hasModelVariable ?pmax ." 
			    + "?pmax  a  j3:PMax  ." 
			    + "?pmax  j2:hasValue ?vpmax ."
			    + "?vpmax   j2:numericalValue ?V_Pmax ." // pmax
			    + "?vpmax   j2:hasUnitOfMeasure  ?V_Pmax_unit ." // pmax

			    + "?model   j5:hasModelVariable ?pmin ." 
			    + "?pmin  a  j3:PMin  ." 
			    + "?pmin  j2:hasValue ?vpmin ."
			    + "?vpmin   j2:numericalValue ?V_Pmin ." // pmin
			    + "?vpmin   j2:hasUnitOfMeasure?V_Pmin_unit ." // pmin

			    + "?model   j5:hasModelVariable ?pc1 ." 
			    + "?pc1  a  j3:Pc1  ." 
			    + "?pc1  j2:hasValue ?vpc1 ."
			    + "?vpc1   j2:numericalValue ?V_Pc1 ." // pc1

			    + "?model   j5:hasModelVariable ?pc2 ." 
			    + "?pc2  a  j3:Pc2  ." 
			    + "?pc2  j2:hasValue ?vpc2 ."
			    + "?vpc2   j2:numericalValue ?V_Pc2 ." // pc2

			    + "?model   j5:hasModelVariable ?qc1min ." 
			    + "?qc1min  a  j3:QC1Min  ."
			    + "?qc1min  j2:hasValue ?vqc1min ." 
			    + "?vqc1min   j2:numericalValue ?V_Qc1Min ." // qc1min

			    + "?model   j5:hasModelVariable ?Qc1max ." 
			    + "?Qc1max  a  j3:QC1Max  ."
			    + "?Qc1max  j2:hasValue ?vQc1max ." 
			    + "?vQc1max   j2:numericalValue ?V_Qc1Max ." // qc1max

			    + "?model   j5:hasModelVariable ?qc2min ." 
			    + "?qc2min  a  j3:QC2Min  ."
			    + "?qc2min  j2:hasValue ?vqc2min ."
			    + "?vqc2min   j2:numericalValue ?V_Qc2Min ." // qc2min

			    + "?model   j5:hasModelVariable ?Qc2max ."
			    + "?Qc2max  a  j3:QC2Max  ."
			    + "?Qc2max  j2:hasValue ?vQc2max ." 
			    + "?vQc2max   j2:numericalValue ?V_Qc2Max ." // qc2max

			    + "?model   j5:hasModelVariable ?rampagc ." 
			    + "?rampagc  a  j3:Rampagc  ."
			    + "?rampagc  j2:hasValue ?vrampagc ." 
			    + "?vrampagc   j2:numericalValue ?V_Ramp_agc ." // rampagc

			    + "?model   j5:hasModelVariable ?ramp10 ." 
			    + "?ramp10  a  j3:Ramp10  ."
			    + "?ramp10  j2:hasValue ?vramp10 ."
			    + "?vramp10   j2:numericalValue ?V_Ramp_10 ." // ramp10

			    + "?model   j5:hasModelVariable ?ramp30 ." 
			    + "?ramp30  a  j3:Ramp30  ."
			    + "?ramp30  j2:hasValue ?vramp30 ." 
			    + "?vramp30   j2:numericalValue ?V_Ramp_30 ." // ramp30

			    + "?model   j5:hasModelVariable ?rampq ." 
			    + "?rampq  a  j3:Rampq  ." 
			    + "?rampq  j2:hasValue ?vrampq ."
			    + "?vrampq   j2:numericalValue ?V_Ramp_q ." // rampq

			    + "?model   j5:hasModelVariable ?apf ."
			    + "?apf  a  j3:APF  ." 
			    + "?apf  j2:hasValue ?vapf ."
			    + "?vapf   j2:numericalValue ?V_APF ." // apf
			    
			    + "?model   j5:hasModelVariable ?startup ." 
			    + "?startup  a  j3:StartCost  ."
			    + "?startup  j2:hasValue ?vstartup ." 
			    + "?vstartup   j2:numericalValue ?V_StartupCost ." //startup cost

			    + "?model   j5:hasModelVariable ?shutdown ." 
			    + "?shutdown  a  j3:StopCost  ."
			    + "?shutdown  j2:hasValue ?vshutdown ." 
			    + "?vshutdown   j2:numericalValue ?V_ShutdownCost ."  //shutdown cost
			    
			    + "?model   j5:hasModelVariable ?gencostn ." 
			    + "?gencostn  a  j3:genCostn  ."
			    + "?gencostn  j2:hasValue ?vgencostn ." 
			    + "?vgencostn   j2:numericalValue ?V_genCostn ." //genCostn

			    + "?model   j5:hasModelVariable ?gencostn1 ." 
			    + "?gencostn1  a  j3:genCostcn-1  ."
			    + "?gencostn1  j2:hasValue ?vgencostn1 ." 
			    + "?vgencostn1   j2:numericalValue ?V_genCostn1 ." //genCostn-1

			    + "?model   j5:hasModelVariable ?gencostn2 ." 
			    + "?gencostn2  a  j3:genCostcn-2  ."
			    + "?gencostn2  j2:hasValue ?vgencostn2 ." 
			    + "?vgencostn2   j2:numericalValue ?V_genCostn2 ."//genCostn-2

			    + "?model   j5:hasModelVariable ?gencostc ." 
			    + "?gencostc  a  j3:genCostc0  ."
			    + "?gencostc  j2:hasValue ?vgencostc ." 
			    + "?vgencostc   j2:numericalValue ?V_genCostc0 ." //genCostc0

			    + "}";
		QueryBroker broker  = new QueryBroker();
		OntModel model = JenaHelper.createModel();
		JenaHelper.read("http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-001.owl#PV-001", model);
		ResultSet resultSet = JenaHelper.query(model, genInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		//String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		System.out.println(result);
	
}
	public void testESSBattery() throws IOException{
		JSONObject jo = new JSONObject();
		jo.put("electricalnetwork", ENIRI);
		String scenarioUrl = BucketHelper.getScenarioUrl("testBatt1");
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		JPSHttpServlet.enableScenario(scenarioUrl,usecaseUrl);
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_ESS/ESSBatterySearch", jo.toString());
		System.out.println(resultStart);
	}
	
	public void testCreateCSV() throws IOException  {
		//String batIRI="http://www.theworldavatar.com/kb/batterycatalog/BatteryCatalog.owl#BatteryCatalog";
		EnergyStorageSystem a = new EnergyStorageSystem();
		OntModel modelbattery=a.readBatteryGreedy(batIRI);
		pvgeniris.add(pvGenIRI);
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		a.prepareCSVPahigh(pvgeniris, baseUrl);	
		a.prepareCSVRemaining(batIRI,baseUrl,modelbattery);
		pvgeniris.clear();
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
