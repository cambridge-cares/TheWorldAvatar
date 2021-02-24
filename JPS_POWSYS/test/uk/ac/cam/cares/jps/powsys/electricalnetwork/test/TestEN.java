package uk.ac.cam.cares.jps.powsys.electricalnetwork.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.util.Util;


public class TestEN extends TestCase {
	
	public static String ELECTRICAL_NETWORK = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	public static String SGELECTRICAL_NETWORK = "http://localhost:8080/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
	String dataPath = QueryBroker.getLocalDataPath();
	String baseUrl=dataPath+"/JPS_POWSYS_EN";
	private ENAgent agent =new ENAgent();
	
	String genInfocost= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
			+ "SELECT ?entity ?formatvalue ?startupcostvalue ?shutdowncostvalue ?gencostnvalue ?gencostn1value ?gencostn2value ?gencostcvalue "
			+ "WHERE {?entity  a  j1:PowerGenerator  ." 
			+ "?entity   j2:isModeledBy ?model ."
								
			+ "?model   j5:hasModelVariable ?format ."
			+ "?format  a  j3:CostModel  ."
			+ "?format  j2:hasValue ?vformat ."
			+ "?vformat  j2:numericalValue ?formatvalue ." 
			
			+ "?model   j5:hasModelVariable ?startup ."
			+ "?startup  a  j3:StartCost  ."
			+ "?startup  j2:hasValue ?vstartup ."
			+ "?vstartup   j2:numericalValue ?startupcostvalue ." 
			
			+ "?model   j5:hasModelVariable ?shutdown ."
			+ "?shutdown  a  j3:StopCost  ."
			+ "?shutdown  j2:hasValue ?vshutdown ."
			+ "?vshutdown   j2:numericalValue ?shutdowncostvalue ."
			
			+ "?model   j5:hasModelVariable ?gencostn ."
			+ "?gencostn  a  j3:genCostn  ."
			+ "?gencostn  j2:hasValue ?vgencostn ."
			+ "?vgencostn   j2:numericalValue ?gencostnvalue ." 
			
			+ "?model   j5:hasModelVariable ?gencostn1 ."
			+ "?gencostn1  a  j3:genCostcn-1  ."
			+ "?gencostn1  j2:hasValue ?vgencostn1 ."
			+ "?vgencostn1   j2:numericalValue ?gencostn1value ." 
			
			+ "?model   j5:hasModelVariable ?gencostn2 ."
			+ "?gencostn2  a  j3:genCostcn-2  ."
			+ "?gencostn2  j2:hasValue ?vgencostn2 ."
			+ "?vgencostn2   j2:numericalValue ?gencostn2value ." 
			
			+ "?model   j5:hasModelVariable ?gencostc ."
			+ "?gencostc  a  j3:genCostc0  ."
			+ "?gencostc  j2:hasValue ?vgencostc ."
			+ "?vgencostc   j2:numericalValue ?gencostcvalue ." 					
			+ "}";
	
	String branchInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
			+ "SELECT ?entity ?BusNumber1value ?BusNumber2value ?resistancevalue ?reactancevalue ?susceptancevalue ?rateAvalue ?rateBvalue ?rateCvalue ?ratiovalue ?anglevalue ?statusvalue ?angleminvalue ?anglemaxvalue "
			
			+ "WHERE {?entity  a  j1:UndergroundCable  ." 
			+ "?entity   j2:isModeledBy ?model ."
			+ "?model   j5:hasModelVariable ?num1 ."
			+ "?num1  a  j3:BusFrom  ."
			+ "?num1  j2:hasValue ?vnum1 ."
			+ "?vnum1   j2:numericalValue ?BusNumber1value ."  //number 1
			
			+ "?model   j5:hasModelVariable ?num2 ."
			+ "?num2  a  j3:BusTo  ."
			+ "?num2  j2:hasValue ?vnum2 ."
			+ "?vnum2   j2:numericalValue ?BusNumber2value ."  //number 2
			
			+ "?model   j5:hasModelVariable ?res ."
			+ "?res  a  j3:R  ."
			+ "?res  j2:hasValue ?vres ."
			+ "?vres   j2:numericalValue ?resistancevalue ."  //resistance
			
			+ "?model   j5:hasModelVariable ?rea ."
			+ "?rea  a  j3:X  ."
			+ "?rea  j2:hasValue ?vrea ."
			+ "?vrea   j2:numericalValue ?reactancevalue ."  //reactance
			
			+ "?model   j5:hasModelVariable ?sus ."
			+ "?sus  a  j3:B  ."
			+ "?sus  j2:hasValue ?vsus ."
			+ "?vsus   j2:numericalValue ?susceptancevalue ."  //susceptance
			
			+ "?model   j5:hasModelVariable ?ratea ."
			+ "?ratea  a  j3:RateA  ."
			+ "?ratea  j2:hasValue ?vratea ."
			+ "?vratea   j2:numericalValue ?rateAvalue ."  //rateA
			
			+ "?model   j5:hasModelVariable ?rateb ."
			+ "?rateb  a  j3:RateB  ."
			+ "?rateb  j2:hasValue ?vrateb ."
			+ "?vrateb   j2:numericalValue ?rateBvalue ."  //rateB
			
			+ "?model   j5:hasModelVariable ?ratec ."
			+ "?ratec  a  j3:RateC  ."
			+ "?ratec  j2:hasValue ?vratec ."
			+ "?vratec   j2:numericalValue ?rateCvalue ."  //rateC
			
			+ "?model   j5:hasModelVariable ?ratio ."
			+ "?ratio  a  j3:RatioCoefficient  ."
			+ "?ratio  j2:hasValue ?vratio ."
			+ "?vratio   j2:numericalValue ?ratiovalue ."  //ratio
			
			+ "?model   j5:hasModelVariable ?ang ."
			+ "?ang  a  j3:Angle  ."
			+ "?ang  j2:hasValue ?vang ."
			+ "?vang   j2:numericalValue ?anglevalue ." //angle
			
			+ "?model   j5:hasModelVariable ?stat ."
			+ "?stat  a  j3:BranchStatus ."
			+ "?stat  j2:hasValue ?vstat ."
			+ "?vstat   j2:numericalValue ?statusvalue ." //status
			
			+ "?model   j5:hasModelVariable ?angmin ."
			+ "?angmin  a  j3:AngleMin  ."
			+ "?angmin  j2:hasValue ?vangmin ."
			+ "?vangmin   j2:numericalValue ?angleminvalue ." //anglemin
			
			+ "?model   j5:hasModelVariable ?angmax ."
			+ "?angmax  a  j3:AngleMax  ."
			+ "?angmax  j2:hasValue ?vangmax ."
			+ "?vangmax   j2:numericalValue ?anglemaxvalue ." //anglemax
			
								
			+ "}";
	
	String busInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
			+ "SELECT ?BusNumbervalue ?typevalue ?activepowervalue ?reactivepowervalue ?Gsvalue ?Bsvalue ?areavalue ?VoltMagvalue ?VoltAnglevalue ?BaseKVvalue ?Zonevalue ?VMaxvalue ?VMinvalue "
			
			+ "WHERE {?entity  a  j1:BusNode  ." 
			+ "?entity   j2:isModeledBy ?model ."
			+ "?model   j5:hasModelVariable ?num ."
			+ "?num  a  j3:BusNumber  ."
			+ "?num  j2:hasValue ?vnum ."
			+ "?vnum   j2:numericalValue ?BusNumbervalue ."  //number
			
			+ "?model   j5:hasModelVariable ?type ."
			+ "?type  a  j3:BusType  ."
			+ "?type  j2:hasValue ?vtype ."
			+ "?vtype   j2:numericalValue ?typevalue ." //type

			+ "?model   j5:hasModelVariable ?Pd ."
			+ "?Pd  a  j3:PdBus  ."
			+ "?Pd  j2:hasValue ?vpd ."
			+ "?vpd   j2:numericalValue ?activepowervalue ."  //pd
			
			+ "?model   j5:hasModelVariable ?Gd ."
			+ "?Gd  a  j3:GdBus  ."
			+ "?Gd  j2:hasValue ?vgd ."
			+ "?vgd   j2:numericalValue ?reactivepowervalue ." //Gd

			+ "?model   j5:hasModelVariable ?Gsvar ."
			+ "?Gsvar  a  j3:Gs  ."
			+ "?Gsvar  j2:hasValue ?vGsvar ."
			+ "?vGsvar   j2:numericalValue ?Gsvalue ." //Gs

			+ "?model   j5:hasModelVariable ?Bsvar ."
			+ "?Bsvar  a  j3:Bs  ."
			+ "?Bsvar  j2:hasValue ?vBsvar ."
			+ "?vBsvar   j2:numericalValue ?Bsvalue ." //Bs

			+ "?model   j5:hasModelVariable ?areavar ."
			+ "?areavar  a  j3:Area  ."
			+ "?areavar  j2:hasValue ?vareavar ."
			+ "?vareavar   j2:numericalValue ?areavalue ." //area
			
			+ "?model   j5:hasModelVariable ?VM ."
			+ "?VM  a  j3:Vm  ."
			+ "?VM  j2:hasValue ?vVM ."
			+ "?vVM   j2:numericalValue ?VoltMagvalue ." //Vm
			
			+ "?model   j5:hasModelVariable ?VA ."
			+ "?VA  a  j3:Va  ."
			+ "?VA  j2:hasValue ?vVA ."
			+ "?vVA   j2:numericalValue ?VoltAnglevalue ." //Va

			+ "?model   j5:hasModelVariable ?BKV ."
			+ "?BKV  a  j3:baseKV  ."
			+ "?BKV  j2:hasValue ?vBKV ."
			+ "?vBKV   j2:numericalValue ?BaseKVvalue ." //Base KV

			+ "?model   j5:hasModelVariable ?zvar ."
			+ "?zvar  a  j3:Zone  ."
			+ "?zvar  j2:hasValue ?vzvar ."
			+ "?vzvar   j2:numericalValue ?Zonevalue ." //Zone

			+ "?model   j5:hasModelVariable ?vmaxvar ."
			+ "?vmaxvar  a  j3:VmMax  ."
			+ "?vmaxvar  j2:hasValue ?vvmaxvar ."
			+ "?vvmaxvar   j2:numericalValue ?VMaxvalue ." //Vmax
			
			+ "?model   j5:hasModelVariable ?vminvar ."
			+ "?vminvar  a  j3:VmMin  ."
			+ "?vminvar  j2:hasValue ?vvminvar ."
			+ "?vvminvar   j2:numericalValue ?VMinvalue ." //Vmin
			
											
			+ "}";
	
	String genInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
			+ "SELECT ?entity ?BusNumbervalue ?activepowervalue ?reactivepowervalue ?Qmaxvalue ?Qminvalue ?Vgvalue ?mBasevalue ?Statusvalue ?Pmaxvalue ?Pminvalue ?Pc1value ?Pc2value ?Qc1minvalue ?Qc1maxvalue ?Qc2minvalue ?Qc2maxvalue ?Rampagcvalue ?Ramp10value ?Ramp30value ?Rampqvalue ?apfvalue "
			
			+ "WHERE {?entity  a  j1:PowerGenerator  ." 
			+ "?entity   j2:isModeledBy ?model ."
			
			+ "?model   j5:hasModelVariable ?num ."
			+ "?num  a  j3:BusNumber  ."
			+ "?num  j2:hasValue ?vnum ."
			+ "?vnum   j2:numericalValue ?BusNumbervalue ."  //number
			
			+ "?model   j5:hasModelVariable ?Pg ."
			+ "?Pg  a  j3:Pg  ."
			+ "?Pg  j2:hasValue ?vpg ."
			+ "?vpg   j2:numericalValue ?activepowervalue ."  //pg
			
			+ "?model   j5:hasModelVariable ?Qg ."
			+ "?Qg  a  j3:Qg  ."
			+ "?Qg  j2:hasValue ?vqg ."
			+ "?vqg   j2:numericalValue ?reactivepowervalue ." //qg
			
			+ "?model   j5:hasModelVariable ?qmax ."
			+ "?qmax  a  j3:QMax  ."
			+ "?qmax  j2:hasValue ?vqmax ."
			+ "?vqmax   j2:numericalValue ?Qmaxvalue ." //qmax
			
			+ "?model   j5:hasModelVariable ?qmin ."
			+ "?qmin  a  j3:QMin  ."
			+ "?qmin  j2:hasValue ?vqmin ."
			+ "?vqmin   j2:numericalValue ?Qminvalue ." //qmin
			
			+ "?model   j5:hasModelVariable ?Vg ."
			+ "?Vg  a  j3:Vg  ."
			+ "?Vg  j2:hasValue ?vVg ."
			+ "?vVg   j2:numericalValue ?Vgvalue ." //vg
			
			+ "?model   j5:hasModelVariable ?mbase ."
			+ "?mbase  a  j3:mBase  ."
			+ "?mbase  j2:hasValue ?vmbase ."
			+ "?vmbase   j2:numericalValue ?mBasevalue ." //mbase
			
			+ "?model   j5:hasModelVariable ?stat ."
			+ "?stat  a  j3:Status ."
			+ "?stat  j2:hasValue ?vstat ."
			+ "?vstat   j2:numericalValue ?Statusvalue ." //status

			+ "?model   j5:hasModelVariable ?pmax ."
			+ "?pmax  a  j3:PMax  ."
			+ "?pmax  j2:hasValue ?vpmax ."
			+ "?vpmax   j2:numericalValue ?Pmaxvalue ." //pmax
			
			+ "?model   j5:hasModelVariable ?pmin ."
			+ "?pmin  a  j3:PMin  ."
			+ "?pmin  j2:hasValue ?vpmin ."
			+ "?vpmin   j2:numericalValue ?Pminvalue ." //pmin
			
			+ "?model   j5:hasModelVariable ?pc1 ."
			+ "?pc1  a  j3:Pc1  ."
			+ "?pc1  j2:hasValue ?vpc1 ."
			+ "?vpc1   j2:numericalValue ?Pc1value ." //pc1
			
			+ "?model   j5:hasModelVariable ?pc2 ."
			+ "?pc2  a  j3:Pc2  ."
			+ "?pc2  j2:hasValue ?vpc2 ."
			+ "?vpc2   j2:numericalValue ?Pc2value ." //pc2
			
			+ "?model   j5:hasModelVariable ?qc1min ."
			+ "?qc1min  a  j3:QC1Min  ."
			+ "?qc1min  j2:hasValue ?vqc1min ."
			+ "?vqc1min   j2:numericalValue ?Qc1minvalue ." //qc1min
			
			+ "?model   j5:hasModelVariable ?Qc1max ."
			+ "?Qc1max  a  j3:QC1Max  ."
			+ "?Qc1max  j2:hasValue ?vQc1max ."
			+ "?vQc1max   j2:numericalValue ?Qc1maxvalue ." //qc1max
			
			+ "?model   j5:hasModelVariable ?qc2min ."
			+ "?qc2min  a  j3:QC2Min  ."
			+ "?qc2min  j2:hasValue ?vqc2min ."
			+ "?vqc2min   j2:numericalValue ?Qc2minvalue ." //qc2min
		
			+ "?model   j5:hasModelVariable ?Qc2max ."
			+ "?Qc2max  a  j3:QC2Max  ."
			+ "?Qc2max  j2:hasValue ?vQc2max ."
			+ "?vQc2max   j2:numericalValue ?Qc2maxvalue ." //qc2max
			
			+ "?model   j5:hasModelVariable ?rampagc ."
			+ "?rampagc  a  j3:Rampagc  ."
			+ "?rampagc  j2:hasValue ?vrampagc ."
			+ "?vrampagc   j2:numericalValue ?Rampagcvalue ." //rampagc

			+ "?model   j5:hasModelVariable ?ramp10 ."
			+ "?ramp10  a  j3:Ramp10  ."
			+ "?ramp10  j2:hasValue ?vramp10 ."
			+ "?vramp10   j2:numericalValue ?Ramp10value ." //ramp10
			
			+ "?model   j5:hasModelVariable ?ramp30 ."
			+ "?ramp30  a  j3:Ramp30  ."
			+ "?ramp30  j2:hasValue ?vramp30 ."
			+ "?vramp30   j2:numericalValue ?Ramp30value ." //ramp30
			
			+ "?model   j5:hasModelVariable ?rampq ."
			+ "?rampq  a  j3:Rampq  ."
			+ "?rampq  j2:hasValue ?vrampq ."
			+ "?vrampq   j2:numericalValue ?Rampqvalue ." //rampq
			
			+ "?model   j5:hasModelVariable ?apf ."
			+ "?apf  a  j3:APF  ."
			+ "?apf  j2:hasValue ?vapf ."
			+ "?vapf   j2:numericalValue ?apfvalue ." //apf
			
			+ "}";
	
	String usecaseID = UUID.randomUUID().toString();
		
	public void testextractOWLinArray() throws IOException, URISyntaxException {
		//String baseurl="C:/JPS_DATA/workingdir/JPS_POWSYS/scenario of powsys";
		//String baseurl="D:/JPS/JParkSimulator-git/JPS_POWSYS/python/model";

		ENAgent b= new ENAgent ();
			//List<String[]>buslist= b.extractOWLinArray(b.readModelGreedy(iriofnetwork),iriofnetwork,genInfo,baseUrl);
			// List<String[]>buslist=  b.extractOWLinArray(b.readModelGreedy(iriofnetwork),iriofnetwork,branchInfo,"branch",baseUrl);
			List<String[]>buslist=b.extractOWLinArray(Util.readModelGreedy(ELECTRICAL_NETWORK),ELECTRICAL_NETWORK,busInfo,"bus",baseUrl);
			//List<String[]>buslist=  b.extractOWLinArray(b.readModelGreedy(iriofnetwork),iriofnetwork,genInfocost,"generatorcost",baseUrl);
			//assertEquals(208, buslist.size());
	}
	
		
	public void testgeninfogathering () throws IOException {
		
		
		ENAgent b= new ENAgent ();
		
	String busmapurl=baseUrl+"/mappingforbus.csv";
	OntModel model = Util.readModelGreedy(ELECTRICAL_NETWORK);
		List<String[]>list=b.extractOWLinArray(model,ELECTRICAL_NETWORK,busInfo,"bus",baseUrl);
	List<String[]>list2=b.extractOWLinArray(model,ELECTRICAL_NETWORK,genInfo,"generator",baseUrl);
	List<String[]>list3=b.extractOWLinArray(model,ELECTRICAL_NETWORK,genInfocost,"generatorcost",baseUrl);
		
		List<String[]>list4=b.extractOWLinArray(model,ELECTRICAL_NETWORK,branchInfo,"branch",baseUrl);
		QueryBroker broker = new QueryBroker();
		
		
		String content=b.createNewTSV(list,baseUrl+"/mappingforbus.csv",busmapurl);
		broker.putLocal(baseUrl+"/bus.txt", content);
		
		content=b.createNewTSV(list2,baseUrl+"/mappingforgenerator.csv",busmapurl);
		broker.putLocal(baseUrl+"/gen.txt", content);
		
		content=b.createNewTSV(list4, baseUrl+"/mappingforbranch.csv",busmapurl);
		broker.putLocal(baseUrl+"/branch.txt", content);
		
		content=b.createNewTSV(list3, baseUrl+"/mappingforgeneratorcost.csv",busmapurl);
		broker.putLocal(baseUrl+"/genCost.txt", content);
	}	
	
	public void testStartSimulationOPFAgentCallNonBaseScenario() throws IOException  { //no more pf

		JSONObject jo = new JSONObject();
		
//		jo.put("electricalnetwork", ELECTRICAL_NETWORK);
//		
//		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSENSimulationOPFCallAgent");
		jo.put("electricalnetwork", ELECTRICAL_NETWORK);
		String scenarioname="testSGPOWSYSOPFCallAgent6"+usecaseID;
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioname);
		JPSHttpServlet.enableScenario(scenarioUrl);	
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true); //optional	
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		System.out.println("contextusecase= "+JPSContext.getUsecaseUrl());
		System.out.println("contextuse case from jo= "+JPSContext.getUsecaseUrl(jo));
//		
		String usecaseUrl = BucketHelper.getUsecaseUrl();	
		System.out.println("usecaseurl= "+usecaseUrl);
		//JPSContext.putUsecaseUrl(jo, usecaseUrl); //if it is used, then the data will be moved to the base

		
		String resultStart = new ScenarioClient().call(scenarioname, "JPS_POWSYS/ENAgent/startsimulationOPF", jo.toString());
	}
	
	//This function is added to be used by the unit test below (JA-Tests check number of columns)
	//This function reads the number of lines in a file (csv compatible) 
	public List<String[]> readResult(String baseUrl,String filename) throws IOException {

        String outputFile = baseUrl + "/"+filename;
        String csv = new QueryBroker().readFileLocal(outputFile);
        List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		
		return simulationResult;
	}
		
	public void testStartSimulationOPFDirectCallNonBaseScenario() throws IOException  {

		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSENSimulationOPFDirectCall");
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);
		//function to copy all the owl file involved ???
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
			
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_POWSYS_EN";
		JSONObject x=new ENAgent().startSimulation(ELECTRICAL_NETWORK, baseUrl, "OPF");
		System.out.println(x.toString());
		
		
		//JA-TESTS 2/02/2021 PyPower Checks
		//Check that the output files are made (branch, bus, gen - in that order below). 
		File file1 = new File(baseUrl +"\\outputBranchOPF.txt");
		assertTrue(file1.exists());
		File file2 = new File(baseUrl +"\\outputBusOPF.txt");
		assertTrue(file2.exists());
		File file3 = new File(baseUrl +"\\outputGenOPF.txt");
		assertTrue(file3.exists());
		
		//Check the number of rows are the same for the inputs and outputs
		//Check Branch In and Out rows are the same length
		List<String[]> rList1 = readResult(baseUrl, "branch.txt"); 
		List<String[]> rList2 = readResult(baseUrl, "outputBranchOPF.txt");
		//System.out.println(rList1.size());
		//System.out.println(rList2.size());
		assertEquals(rList1.size(), rList2.size());
		//Check Bus In and Out rows are the same length
		List<String[]> rList3 = readResult(baseUrl, "bus.txt"); 
		List<String[]> rList4 = readResult(baseUrl, "outputBusOPF.txt");
		//System.out.println(rList3.size());
		//System.out.println(rList4.size());
		assertEquals(rList3.size(), rList4.size());
		//Check Gen In and Out rows are the same length
		List<String[]> rList5 = readResult(baseUrl, "Gen.txt"); 
		List<String[]> rList6 = readResult(baseUrl, "outputGenOPF.txt");
		//System.out.println(rList5.size());
		//System.out.println(rList6.size());
		assertEquals(rList5.size(), rList6.size());
		
	}
	
	public void testStartSimulationOPFDirectCallBaseScenario() throws IOException  {			
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_POWSYS_EN";
		JSONObject x=new ENAgent().startSimulation(ELECTRICAL_NETWORK, baseUrl, "OPF");
		System.out.println(x.toString());
	}
	
	public void xxxtestupdatelocalgenerator() throws IOException {
		String resourceDir = "D:\\tmp\\scenario for testing with more generator\\Final Version\\owl file changed";
		
		for(int s=24;s<=29;s++) {
			String x=String.format("%03d", s);
			String filePath = resourceDir + "/EGen-"+x+".owl"; // the original owl file
			FileInputStream inFile = new FileInputStream(filePath);
			Reader in = new InputStreamReader(inFile, "UTF-8");
			QueryBroker broker = new QueryBroker();
			OntModel jenaOwlModel2 = ModelFactory.createOntologyModel();
			jenaOwlModel2.read(in, null);
			
//add new import to owl files
			//Ontology ont=jenaOwlModel2.getOntology("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl");
			//ont.addImport(jenaOwlModel2.createResource("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl"));

			//put the technology and emission 
			Individual gen = jenaOwlModel2.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#EGen-"+x);
			ObjectProperty realizes=jenaOwlModel2.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#realizes");
			Individual powergenerationgas = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OilGeneration");

//			gen.addProperty(realizes,powergenerationgas);
			ObjectProperty hasEmission=jenaOwlModel2.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission");
			ObjectProperty usestech=jenaOwlModel2.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#usesGenerationTechnology");
			ObjectProperty hasvalue = jenaOwlModel2.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
			ObjectProperty hasunit = jenaOwlModel2.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
			DatatypeProperty numval = jenaOwlModel2.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");;
//			Individual ccgt = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CombinedCycleGasTurbine");
//			Individual ocgt = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OpenCycleGasTurbine");
//			Individual subcritical = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SubCriticalThermal");
//			powergenerationgas.addProperty(usestech,ccgt);
//			Individual ccgtval = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#V_CO2EmissionFactor_CCGT");
//			Individual ocgtval = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#V_CO2EmissionFactor_OCGT");
			Individual subcritval = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#V_CO2EmissionFactor_SubCritical");
//			
//			
//			Individual Pact = jenaOwlModel2.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#V_PGen_EGen-"+x);
			double emfact=subcritval.getPropertyValue(numval).asLiteral().getDouble();
//			double actcap=Pact.getPropertyValue(numval).asLiteral().getDouble();
//			OntClass emissionclass = jenaOwlModel2.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#Actual_CO2_Emission");
			OntClass scalarvalueclass = jenaOwlModel2.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
//			Individual emission = emissionclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#Actual_CO2_Emission_EGen-"+x);
//			Individual vemission = scalarvalueclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#V_Actual_CO2_Emission_EGen-"+x);
//			powergenerationgas.addProperty(hasEmission,emission);
//			emission.addProperty(hasvalue,vemission);
//			vemission.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(emfact*actcap)));
//			Individual t = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_hr");
//			vemission.addProperty(hasunit,t);
//
//			Individual process = jenaOwlModel2.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#PowerGeneration_EGen-"+x);
//			gen.removeProperty(realizes,process );
			
			OntClass designemissionclass = jenaOwlModel2.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#CO2_emission");
			Individual desemission = designemissionclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#Design_CO2_Emission_EGen-"+x);
			Individual vdesemission = scalarvalueclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#V_Design_CO2_Emission_EGen-"+x);
			powergenerationgas.addProperty(hasEmission,desemission);
			desemission.addProperty(hasvalue,vdesemission);
			Individual Pmax = jenaOwlModel2.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#V_Pmax_EGen-"+x);
			double descap=Pmax.getPropertyValue(numval).asLiteral().getDouble();
			vdesemission.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(emfact*descap)));
			Individual tph = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_hr");
			vdesemission.addProperty(hasunit,tph);
//edit the linkage of plant-generator
//			ObjectProperty isSubsystemOf=jenaOwlModel2.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isSubsystemOf");
//			Individual gen = jenaOwlModel2.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#EGen-"+x);
//			Individual plant = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore.owl#PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore");
//			gen.removeProperty(isSubsystemOf,plant );
//			gen.addProperty(isSubsystemOf, "http://www.theworldavatar.com/kb/powerplants/PowerSeraya_Pulau_Seraya_Oil_Power_Station_Singapore.owl#PowerSeraya_Pulau_Seraya_Oil_Power_Station_Singapore");

//edit the x and y for several generator
//			DatatypeProperty numval = jenaOwlModel2.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");;
//			Individual xcoordinatevalue = jenaOwlModel2.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#V_x_EGen-"+x);
//			xcoordinatevalue.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(103.72386)));
//			Individual ycoordinatevalue = jenaOwlModel2.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-"+x+".owl#V_y_EGen-"+x);
//			ycoordinatevalue.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(1.28135)));
			String content = JenaHelper.writeToString(jenaOwlModel2);
			broker.putLocal(filePath, content);
		}
			
		
	}
	
	public void xxxtestread() throws IOException { //need to provide specific directory first
		String baseUrl="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\testPOWSYSENSimulationOPFDirectCall\\localhost_8080\\data\\8b7e6530-54b7-4e51-99cf-1bcc663d5658\\JPS_POWSYS_EN";
		String fileName = baseUrl+"/outputstatus.txt";
		Path path = Paths.get(fileName);
		byte[] bytes = Files.readAllBytes(path);
		List<String> allLines = Files.readAllLines(path, StandardCharsets.UTF_8);
		System.out.println(allLines.size());
		System.out.println(allLines.get(2));
	
	}

	public void xxxtestquerygen() {
		OntModel jenaOwlModel = JenaHelper.createModel("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-008.owl");
		new ENAgent().updateGeneratorEmission(jenaOwlModel);
	}
	
	public void copyFromFolder(String baseUrl, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/sample_input/" + filename);

		String destinationUrl = baseUrl + "/" + filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
	public void testRunPythonScript() {
		String script = "PyPower-PF-OPF-JA-9-Java-1.py";
		String folder = QueryBroker.getLocalDataPath() + "/JPS_POWSYS_EN";
		System.out.println(folder);
		//Create new txt Files here: I'm just copying over from a single folder
		//but create it in this location
		copyFromFolder(folder, "fixedbaseMVA.txt");
		copyFromFolder(folder, "fixedbranch.txt");
		copyFromFolder(folder, "fixedbus.txt");
		copyFromFolder(folder, "fixedgen.txt");
		copyFromFolder(folder, "fixedgenCost.txt");
		
		try {
			String[] fileNames = {"/fixedbaseMVA.txt", "/fixedbus.txt", "/fixedgen.txt", "/fixedbranch.txt", "/fixedoutputBusOPF.txt", "/fixedoutputBranchOPF.txt", "/fixedoutputGenOPF.txt", "/fixedareas.txt", "/fixedgenCost.txt", "/fixedoutputStatus.txt"};
			new ENAgent().runPythonScript(script, folder, fileNames);
			String fileName = folder+"/fixedoutputStatus.txt";
//			Path path = Paths.get(fileName);
//			List<String> allLines = Files.readAllLines(path, StandardCharsets.UTF_8);
//			assertTrue(allLines.get(2).contains("Converged!")); 
			BufferedReader input = new BufferedReader(new FileReader(fileName));
		    String last, line;
		    last = "";
		    while ((line = input.readLine()) != null) { 
		        last = line;
		    }
		    assertTrue(last.contains("Converged")); 
		    
		    //JA-TESTS PyPower Checks for the Fixed Input/Output Test
			//Check that the output files are made (branch, bus, gen - in that order below). 
			File file1 = new File(folder +"\\fixedoutputBranchOPF.txt");
			assertTrue(file1.exists());
			File file2 = new File(folder +"\\fixedoutputBusOPF.txt");
			assertTrue(file2.exists());
			File file3 = new File(folder +"\\fixedoutputGenOPF.txt");
			assertTrue(file3.exists());
			
			//Check the number of rows are the same for the inputs and outputs
			//Check Branch In and Out rows are the same length
			List<String[]> fixedrList1 = readResult(folder, "fixedbranch.txt"); 
			List<String[]> fixedrList2 = readResult(folder, "fixedoutputBranchOPF.txt");
			//System.out.println(fixedrList1.size());
			//System.out.println(fixedrList2.size());
			assertEquals(fixedrList1.size(), fixedrList2.size());
			//Check Bus In and Out rows are the same length
			List<String[]> fixedrList3 = readResult(folder, "fixedbus.txt"); 
			List<String[]> fixedrList4 = readResult(folder, "fixedoutputBusOPF.txt");
			//System.out.println(fixedrList3.size());
			//System.out.println(fixedrList4.size());
			assertEquals(fixedrList3.size(), fixedrList4.size());
			//Check Gen In and Out rows are the same length
			List<String[]> fixedrList5 = readResult(folder, "fixedGen.txt"); 
			List<String[]> fixedrList6 = readResult(folder, "fixedoutputGenOPF.txt");
			//System.out.println(fixedrList5.size());
			//System.out.println(fixedrList6.size());
			assertEquals(fixedrList5.size(), fixedrList6.size());
			
			//Check the output itself. This uses the lists made above.  
			//Firstly, check that the input is correct to what is pre-determined to correspond to the output. 
			//These tests are a bit repetitive, but didn't want to have the for loop in a different function so if there's a failure it's easier to see what line it's on and thus which comparison failed. 
			//Check that the base branch input data hasn't changed. 
			String[] branchBase = {"22	191	0.00949	0.0101	4.54e-05	40.39142483	0	0	1	0	1	-360	360", "203	91	0.0215	0.0228	2.57e-05	20.19571242	0	0	1	0	1	-360	360", "32	50	0.000185	0.00304	3.16e-06	148.6099593	0	0	1	0	1	-360	360", "139	102	0.0368	0.039	0.000176	40.39142483	0	0	1	0	0	-360	360", "171	92	4.7e-05	0.000803	6.3e-06	2246.816308	0	0	1	0	1	-360	360", "107	164	0.0264	0.028	3.16e-05	20.19571242	0	0	1	0	1	-360	360", "49	115	0.000413	0.00678	6.36e-05	445.8298779	0	0	1	0	1	-360	360", "115	200	0.000305	0.005	5.21e-06	148.6099593	0	0	1	0	1	-360	360", "143	16	0.00632	0.0067	7.56e-06	20.19571242	0	0	1	0	1	-360	360", "184	106	0.00014	0.000576	2.4e-06	148.6099593	0	0	1	0	1	-360	360", "116	3	0.000331	0.00542	2.26e-05	297.2199186	0	0	1	0	1	-360	360", "51	181	0.00761	0.00807	3.64e-05	40.39142483	0	0	1	0	1	-360	360", "177	195	0.000799	0.0131	5.47e-05	297.2199186	0	0	1	0	1	-360	360", "132	4	0.00809	0.00859	9.68e-06	20.19571242	0	0	1	0	1	-360	360", "18	138	0.00654	0.00694	7.83e-06	20.19571242	0	0	1	0	1	-360	360", "81	88	0.00368	0.0039	1.76e-05	40.39142483	0	0	1	0	1	-360	360", "137	5	0.00934	0.0099	1.12e-05	20.19571242	0	0	1	0	1	-360	360", "69	129	0.0192	0.0204	2.3e-05	20.19571242	0	0	1	0	1	-360	360", "102	165	0.0368	0.039	0.000176	40.39142483	0	0	1	0	0	-360	360", "149	194	0.000496	0.00813	8.48e-06	148.6099593	0	0	1	0	1	-360	360", "76	144	0.0018	0.00191	8.61e-06	40.39142483	0	0	1	0	1	-360	360", "109	197	0.0368	0.039	0.000176	40.39142483	0	0	1	0	0	-360	360", "121	95	0.0282	0.03	3.38e-05	20.19571242	0	0	1	0	1	-360	360", "27	118	7.94e-05	0.0013	1.36e-06	148.6099593	0	0	1	0	1	-360	360", "124	206	8.51e-05	0.0014	1.31e-05	1685.112231	0	0	1	0	1	-360	360", "133	28	0.00109	0.00115	2.08e-05	80.78284967	0	0	1	0	1	-360	360", "101	176	0.00595	0.00631	7.12e-06	20.19571242	0	0	1	0	1	-360	360", "157	173	0.00511	0.00542	9.78e-05	80.78284967	0	0	1	0	1	-360	360", "47	94	0.0181	0.0192	2.16e-05	20.19571242	0	0	1	0	1	-360	360", "16	89	0.0349	0.037	4.17e-05	20.19571242	0	0	1	0	1	-360	360", "24	41	3.4e-05	0.000582	2.56e-06	1685.112231	0	0	1	0	1	-360	360", "61	120	7.51e-05	0.00123	1.28e-06	148.6099593	0	0	1	0	1	-360	360", "130	43	0.0147	0.0156	1.76e-05	20.19571242	0	0	1	0	1	-360	360", "113	139	0.0368	0.039	0.000176	40.39142483	0	0	1	0	0	-360	360", "20	128	0.000661	0.0108	1.13e-05	148.6099593	0	0	1	0	1	-360	360", "98	110	0.0109	0.0115	1.3e-05	20.19571242	0	0	1	0	1	-360	360", "195	178	0.000857	0.0141	1.46e-05	148.6099593	0	0	1	0	1	-360	360", "116	27	0.000331	0.00542	5.65e-06	148.6099593	0	0	1	0	1	-360	360", "205	119	0.000734	0.000778	8.78e-07	20.19571242	0	0	1	0	1	-360	360", "118	19	5.72e-06	9.38e-05	9.78e-08	148.6099593	0	0	1	0	1	-360	360", "190	85	0.0211	0.0224	2.52e-05	20.19571242	0	0	1	0	1	-360	360", "193	29	0.00603	0.0064	2.88e-05	40.39142483	0	0	1	0	1	-360	360", "159	76	0.0195	0.0207	9.33e-05	40.39142483	0	0	1	0	1	-360	360", "64	135	0.0123	0.013	0.000132	60.58713725	0	0	1	0	1	-360	360", "188	166	8.26e-09	1.36e-07	1.41e-10	148.6099593	0	0	1	0	1	-360	360", "12	174	0.00124	0.0204	2.12e-05	148.6099593	0	0	1	0	1	-360	360", "90	32	3.63e-05	0.000298	6.21e-07	148.6099593	0	0	1	0	1	-360	360", "10	204	0.0101	0.0108	1.21e-05	20.19571242	0	0	1	0	1	-360	360", "129	111	0.0168	0.0178	2.01e-05	20.19571242	0	0	1	0	1	-360	360", "109	35	0.00993	0.0105	1.19e-05	20.19571242	0	0	1	0	1	-360	360", "91	72	0.0368	0.039	4.4e-05	20.19571242	0	0	1	0	1	-360	360", "70	100	0.015	0.0159	7.18e-05	40.39142483	0	0	1	0	1	-360	360", "124	66	6.11e-05	0.00733	0.0	1500	0	0	1	0	1	-360	360", "139	67	0.0441	0.0468	5.28e-05	20.19571242	0	0	1	0	1	-360	360", "183	147	0.0103	0.0109	1.23e-05	20.19571242	0	0	1	0	1	-360	360", "67	207	0.0144	0.0153	1.72e-05	20.19571242	0	0	1	0	1	-360	360", "79	145	0.00971	0.0103	0.000186	80.78284967	0	0	1	0	1	-360	360", "122	131	0.0248	0.0263	2.96e-05	20.19571242	0	0	1	0	1	-360	360", "28	103	0.045	0.045	5.07e-05	20.19571242	0	0	1	0	1	-360	360", "52	104	0.00489	0.00518	2.34e-05	40.39142483	0	0	1	0	1	-360	360", "96	114	0.0285	0.0302	0.000136	40.39142483	0	0	1	0	1	-360	360", "186	74	0.0369	0.0392	4.41e-05	20.19571242	0	0	1	0	1	-360	360", "194	130	0.000333	0.0267	0.0	300	0	0	1	0	1	-360	360", "104	179	0.00724	0.00768	3.47e-05	40.39142483	0	0	1	0	1	-360	360", "105	115	0.00124	0.0203	8.48e-05	297.2199186	0	0	1	0	1	-360	360", "151	46	0.000315	0.00516	2.15e-05	297.2199186	0	0	1	0	1	-360	360", "96	172	0.016	0.017	1.92e-05	20.19571242	0	0	1	0	1	-360	360", "71	148	0.00178	0.00189	3.41e-05	80.78284967	0	0	1	0	1	-360	360", "20	165	0.000333	0.0267	0.0	300	0	0	1	0	1	-360	360", "1	189	3.51e-05	0.0006	4.7e-06	2246.816308	0	0	1	0	1	-360	360", "7	14	0.00838	0.00889	1e-05	20.19571242	0	0	1	0	1	-360	360", "79	193	0.0355	0.0377	0.00017	40.39142483	0	0	1	0	1	-360	360", "151	149	0.00039	0.00639	2.66e-05	297.2199186	0	0	1	0	1	-360	360", "206	180	5.78e-06	9.88e-05	1.94e-07	1123.408154	0	0	1	0	1	-360	360", "158	185	0.00412	0.00437	4.93e-06	20.19571242	0	0	1	0	1	-360	360", "162	159	0.0011	0.00117	5.27e-06	40.39142483	0	0	1	0	1	-360	360", "171	151	0.000122	0.0147	0.0	750	0	0	1	0	1	-360	360", "44	82	0.00581	0.00616	6.95e-06	20.19571242	0	0	1	0	1	-360	360", "4	56	0.00544	0.00577	6.51e-06	20.19571242	0	0	1	0	1	-360	360", "15	75	0.000663	0.000703	1.27e-05	80.78284967	0	0	1	0	1	-360	360", "171	163	5.1e-05	0.000873	1.71e-06	1123.408154	0	0	1	0	1	-360	360", "9	37	0.00434	0.0046	2.08e-05	40.39142483	0	0	1	0	1	-360	360", "167	57	0.0075	0.00796	8.97e-06	20.19571242	0	0	1	0	1	-360	360", "149	197	0.000333	0.0267	0.0	300	0	0	1	0	1	-360	360", "50	184	1.08e-05	0.000177	1.84e-07	148.6099593	0	0	1	0	1	-360	360", "35	101	0.014	0.0148	1.67e-05	20.19571242	0	0	1	0	1	-360	360", "139	157	0.0368	0.039	0.000176	40.39142483	0	0	1	0	0	-360	360", "134	36	5.2e-05	0.000853	8.89e-07	148.6099593	0	0	1	0	1	-360	360", "79	182	0.12	0.128	0.000144	20.19571242	0	0	1	0	1	-360	360", "197	132	0.0246	0.0261	2.95e-05	20.19571242	0	0	1	0	1	-360	360", "175	108	0.013	0.0138	6.23e-05	40.39142483	0	0	1	0	1	-360	360", "100	52	0.00493	0.00523	2.36e-05	40.39142483	0	0	1	0	1	-360	360", "151	168	0.000272	0.00446	1.86e-05	297.2199186	0	0	1	0	1	-360	360", "97	192	0.00736	0.0078	8.8e-06	20.19571242	0	0	1	0	1	-360	360", "199	139	0.000184	0.00459	0.0	300	0	0	1	0	1	-360	360", "196	51	0.00526	0.00558	2.52e-05	40.39142483	0	0	1	0	1	-360	360", "165	186	0.00919	0.00975	0.000176	80.78284967	0	0	1	0	1	-360	360", "204	202	0.0234	0.0248	2.8e-05	20.19571242	0	0	1	0	1	-360	360", "88	201	0.00151	0.0016	7.22e-06	40.39142483	0	0	1	0	1	-360	360", "107	83	0.0268	0.0285	3.21e-05	20.19571242	0	0	1	0	1	-360	360", "117	40	0.00118	0.00126	1.42e-06	20.19571242	0	0	1	0	1	-360	360", "186	70	0.0183	0.0194	8.76e-05	40.39142483	0	0	1	0	1	-360	360", "1	48	6.56e-05	0.00112	4.94e-06	1685.112231	0	0	1	0	1	-360	360", "185	170	0.00677	0.00718	8.1e-06	20.19571242	0	0	1	0	1	-360	360", "12	78	0.00165	0.0271	2.83e-05	148.6099593	0	0	1	0	1	-360	360", "115	79	0.000333	0.0267	0.0	300	0	0	1	0	1	-360	360", "157	133	0.00432	0.00458	8.27e-05	80.78284967	0	0	1	0	1	-360	360", "155	150	0.00489	0.00518	2.34e-05	40.39142483	0	0	1	0	1	-360	360", "177	58	0.000333	0.0267	0.0	300	0	0	1	0	1	-360	360", "68	25	0.000197	0.00324	3.38e-06	148.6099593	0	0	1	0	1	-360	360", "3	199	0.000331	0.00542	2.26e-05	297.2199186	0	0	1	0	1	-360	360", "171	180	5.27e-05	0.000901	3.97e-06	1685.112231	0	0	1	0	1	-360	360", "139	96	0.00544	0.00577	5.86e-05	60.58713725	0	0	1	0	1	-360	360", "42	86	0.0136	0.0144	1.63e-05	20.19571242	0	0	1	0	1	-360	360", "166	90	0.000228	0.000935	3.9e-06	148.6099593	0	0	1	0	1	-360	360", "46	177	0.000775	0.0127	1.32e-05	148.6099593	0	0	1	0	1	-360	360", "66	199	2.04e-05	0.000334	3.14e-06	445.8298779	0	0	1	0	1	-360	360", "197	137	0.00904	0.00959	1.08e-05	20.19571242	0	0	1	0	1	-360	360", "197	113	0.0368	0.039	0.000176	40.39142483	0	0	1	0	0	-360	360", "198	140	0.00415	0.00441	1.99e-05	40.39142483	0	0	1	0	1	-360	360", "65	146	0.00853	0.00905	1.02e-05	20.19571242	0	0	1	0	1	-360	360", "58	22	0.0194	0.0206	9.27e-05	40.39142483	0	0	1	0	1	-360	360", "21	87	0.00798	0.00846	3.82e-05	40.39142483	0	0	1	0	1	-360	360", "12	84	4.13e-05	0.000678	2.83e-06	297.2199186	0	0	1	0	1	-360	360", "115	166	0.000826	0.00339	1.41e-05	148.6099593	0	0	1	0	1	-360	360", "102	201	0.0546	0.058	0.000261	40.39142483	0	0	1	0	1	-360	360", "187	31	0.0207	0.022	2.48e-05	20.19571242	0	0	1	0	1	-360	360", "53	161	0.00368	0.0039	4.4e-06	20.19571242	0	0	1	0	1	-360	360", "154	153	0.0173	0.0183	2.07e-05	20.19571242	0	0	1	0	1	-360	360", "116	157	0.000333	0.0267	0.0	300	0	0	1	0	1	-360	360", "86	18	0.00941	0.00998	1.13e-05	20.19571242	0	0	1	0	1	-360	360", "74	33	0.0201	0.0213	2.4e-05	20.19571242	0	0	1	0	1	-360	360", "141	183	0.0122	0.0129	1.46e-05	20.19571242	0	0	1	0	1	-360	360", "48	20	3.06e-05	0.00367	0.0	3000	0	0	1	0	1	-360	360", "60	205	0.00728	0.00772	8.71e-06	20.19571242	0	0	1	0	1	-360	360", "25	136	0.000849	0.0139	1.45e-05	148.6099593	0	0	1	0	1	-360	360", "56	167	0.0101	0.0107	1.2e-05	20.19571242	0	0	1	0	1	-360	360", "102	79	0.0368	0.039	0.000176	40.39142483	0	0	1	0	0	-360	360", "157	65	0.0137	0.0145	1.64e-05	20.19571242	0	0	1	0	1	-360	360", "152	107	0.00118	0.00125	1.27e-05	60.58713725	0	0	1	0	1	-360	360", "72	155	0.00291	0.00308	1.39e-05	40.39142483	0	0	1	0	1	-360	360", "87	208	0.000586	0.000621	2.8e-06	40.39142483	0	0	1	0	1	-360	360", "170	54	0.0148	0.0157	1.77e-05	20.19571242	0	0	1	0	1	-360	360", "31	6	0.00941	0.00998	1.13e-05	20.19571242	0	0	1	0	1	-360	360", "165	21	0.0184	0.0195	8.8e-05	40.39142483	0	0	1	0	1	-360	360", "165	44	0.0173	0.0184	2.08e-05	20.19571242	0	0	1	0	1	-360	360", "157	97	0.0279	0.0296	3.33e-05	20.19571242	0	0	1	0	1	-360	360", "61	11	8.84e-05	0.00145	1.51e-06	148.6099593	0	0	1	0	1	-360	360", "54	77	0.0354	0.0375	4.23e-05	20.19571242	0	0	1	0	1	-360	360", "124	1	5.71e-05	0.000976	4.3e-06	1685.112231	0	0	1	0	1	-360	360", "109	154	0.0119	0.0126	1.43e-05	20.19571242	0	0	1	0	1	-360	360", "28	152	0.00343	0.00364	3.69e-05	60.58713725	0	0	1	0	1	-360	360", "131	23	0.0086	0.00913	1.03e-05	20.19571242	0	0	1	0	1	-360	360", "99	72	0.00289	0.00307	3.11e-05	60.58713725	0	0	1	0	1	-360	360", "40	42	0.0101	0.0107	1.2e-05	20.19571242	0	0	1	0	1	-360	360", "145	59	0.00145	0.00154	2.78e-05	80.78284967	0	0	1	0	1	-360	360", "14	186	0.0393	0.0417	4.71e-05	20.19571242	0	0	1	0	1	-360	360", "38	7	0.00236	0.0025	2.82e-06	20.19571242	0	0	1	0	1	-360	360", "130	127	0.0233	0.0247	2.79e-05	20.19571242	0	0	1	0	1	-360	360", "79	62	0.0818	0.0868	9.79e-05	20.19571242	0	0	1	0	1	-360	360", "109	143	0.00772	0.00818	9.23e-06	20.19571242	0	0	1	0	1	-360	360", "61	13	6.36e-05	0.00104	1.09e-06	148.6099593	0	0	1	0	1	-360	360", "5	10	0.00471	0.005	5.63e-06	20.19571242	0	0	1	0	1	-360	360", "84	102	0.000333	0.0267	0.0	300	0	0	1	0	1	-360	360", "140	45	0.0253	0.0268	0.000121	40.39142483	0	0	1	0	1	-360	360", "109	113	0.0368	0.039	0.000176	40.39142483	0	0	1	0	0	-360	360", "135	142	0.0106	0.0113	0.000114	60.58713725	0	0	1	0	1	-360	360", "41	123	7.94e-06	0.000136	5.98e-07	1685.112231	0	0	1	0	1	-360	360", "59	71	0.00483	0.00513	9.25e-05	80.78284967	0	0	1	0	1	-360	360", "192	53	0.00368	0.0039	4.4e-06	20.19571242	0	0	1	0	1	-360	360", "123	171	2.27e-05	0.000388	1.71e-06	1685.112231	0	0	1	0	1	-360	360", "62	122	0.00117	0.00124	1.4e-06	20.19571242	0	0	1	0	1	-360	360", "96	73	0.0105	0.0112	5.03e-05	40.39142483	0	0	1	0	1	-360	360", "148	187	0.0284	0.0301	3.39e-05	20.19571242	0	0	1	0	1	-360	360", "1	12	6.11e-05	0.00733	0.0	1500	0	0	1	0	1	-360	360", "12	49	0.000138	0.00226	2.12e-05	445.8298779	0	0	1	0	1	-360	360", "81	125	0.00897	0.00952	4.29e-05	40.39142483	0	0	1	0	1	-360	360", "142	93	0.0105	0.0111	0.000113	60.58713725	0	0	1	0	1	-360	360", "193	8	0.0501	0.0531	0.00024	40.39142483	0	0	1	0	1	-360	360", "148	196	0.0151	0.016	7.23e-05	40.39142483	0	0	1	0	1	-360	360", "208	55	0.00338	0.00359	1.62e-05	40.39142483	0	0	1	0	1	-360	360", "165	64	0.00753	0.00798	8.1e-05	60.58713725	0	0	1	0	1	-360	360", "79	198	0.00982	0.0104	4.7e-05	40.39142483	0	0	1	0	1	-360	360", "112	134	0.000191	0.00313	3.26e-06	148.6099593	0	0	1	0	1	-360	360", "113	58	0.0368	0.039	0.000176	40.39142483	0	0	1	0	0	-360	360", "109	2	0.000734	0.000778	8.78e-07	20.19571242	0	0	1	0	1	-360	360", "98	80	0.0202	0.0215	2.42e-05	20.19571242	0	0	1	0	1	-360	360", "114	9	0.00375	0.00398	1.79e-05	40.39142483	0	0	1	0	1	-360	360", "102	121	0.0458	0.0486	0.000219	40.39142483	0	0	1	0	1	-360	360", "147	63	0.000959	0.00102	1.15e-06	20.19571242	0	0	1	0	1	-360	360", "150	175	0.00239	0.00254	1.14e-05	40.39142483	0	0	1	0	1	-360	360", "146	47	0.0343	0.0364	4.1e-05	20.19571242	0	0	1	0	1	-360	360", "102	99	0.00703	0.00746	7.57e-05	60.58713725	0	0	1	0	1	-360	360", "124	30	4.36e-05	0.000746	3.29e-06	1685.112231	0	0	1	0	1	-360	360", "113	141	0.0172	0.0183	2.06e-05	20.19571242	0	0	1	0	1	-360	360", "113	190	0.00611	0.00648	7.3e-06	20.19571242	0	0	1	0	1	-360	360", "20	112	0.000781	0.0128	1.33e-05	148.6099593	0	0	1	0	1	-360	360", "113	158	0.0163	0.0173	1.95e-05	20.19571242	0	0	1	0	1	-360	360", "109	160	0.0117	0.0124	1.4e-05	20.19571242	0	0	1	0	1	-360	360", "191	144	0.0381	0.0404	0.000182	40.39142483	0	0	1	0	1	-360	360", "160	17	0.00162	0.00172	1.94e-06	20.19571242	0	0	1	0	1	-360	360", "46	113	0.000333	0.0267	0.0	300	0	0	1	0	1	-360	360", "202	34	0.0022	0.00233	2.63e-06	20.19571242	0	0	1	0	1	-360	360", "58	156	0.064	0.0679	7.66e-05	20.19571242	0	0	1	0	1	-360	360", "194	68	0.000356	0.00584	6.09e-06	148.6099593	0	0	1	0	1	-360	360", "119	2	0.00949	0.0101	1.14e-05	20.19571242	0	0	1	0	1	-360	360", "200	169	0.000545	0.00894	9.32e-06	148.6099593	0	0	1	0	1	-360	360", "57	26	0.0478	0.0507	5.72e-05	20.19571242	0	0	1	0	1	-360	360", "108	203	0.0104	0.0111	1.25e-05	20.19571242	0	0	1	0	1	-360	360", "20	105	6.58e-05	0.00108	1.01e-05	445.8298779	0	0	1	0	1	-360	360", "79	29	0.0355	0.0377	0.00017	40.39142483	0	0	1	0	1	-360	360", "173	15	0.00221	0.00234	4.22e-05	80.78284967	0	0	1	0	1	-360	360", "113	117	0.0137	0.0145	1.64e-05	20.19571242	0	0	1	0	1	-360	360", "75	98	0.0186	0.0197	8.9e-05	40.39142483	0	0	1	0	1	-360	360", "19	61	0.000142	0.00233	2.43e-06	148.6099593	0	0	1	0	1	-360	360", "197	126	0.0086	0.00913	1.03e-05	20.19571242	0	0	1	0	1	-360	360", "168	109	0.000333	0.0267	0.0	300	0	0	1	0	1	-360	360", "121	69	0.016	0.017	1.92e-05	20.19571242	0	0	1	0	1	-360	360", "172	39	0.00375	0.00398	4.49e-06	20.19571242	0	0	1	0	1	-360	360"};
			for (int i = 0; i < fixedrList1.size(); i++){
			  String[] arr = fixedrList1.get(i);
			  String str = String.join(",", arr);
			  //System.out.println(str);
			  //System.out.println(branchBase[i]);
			  assertEquals(str, branchBase[i]);
			}
			//Check that the base bus input data hasn't changed. 
			String[] busBase = {"1	1	0.0	0.0	0	0	1	1.0427668646556403	0.202458630313752	230	1	1.05	0.95", "2	1	2.85	1.2801	0	0	1	1.0175322347647784	-2.0621043616258965	22	1	1.05	0.95", "3	1	65.3	9.0612	0	0	1	1.0279569538736146	-1.4051242266590986	66	1	1.05	0.95", "4	1	2.57	1.2447	0	0	1	1.0140887267616712	-2.1409524077700284	22	1	1.05	0.95", "5	1	0.0	0.0	0	0	1	1.0148506165848994	-2.1348215487548754	22	1	1.05	0.95", "6	1	0.0	0.0	0	0	1	0.9750124812841793	-5.490346702399187	22	1	1.05	0.95", "7	1	7.09	3.8263	0	0	1	1.0005662553171843	-3.5041441247207583	22	1	1.05	0.95", "8	1	25.6	11.9915	0	0	1	0.9591266393534287	-5.8295523198864565	22	1	1.05	0.95", "9	1	6.52	3.1859	0	0	1	1.0250842916618883	-0.941631872379373	22	1	1.05	0.95", "10	1	2.47	1.2317	0	0	1	1.0141487238530391	-2.152370658846765	22	1	1.05	0.95", "11	1	27.5	13.1169	0	0	1	1.0184959091802916	-2.303927007860102	66	1	1.05	0.95", "12	1	0.0	0.0	0	0	1	1.0225506272193576	-1.955555344450829	66	1	1.05	0.95", "13	1	4.03	2.0487	0	0	1	1.018683156636038	-2.284789557972747	66	1	1.05	0.95", "14	1	0.0	0.0	0	0	1	1.0022397129520626	-3.4712322278813694	22	1	1.05	0.95", "15	1	6.07	2.9561	0	0	1	1.0004839547345485	-3.7414720381052304	22	1	1.05	0.95", "16	1	0.0	0.0	0	0	1	1.016949737037724	-2.071535147736141	22	1	1.05	0.95", "17	1	0.0	0.0	0	0	1	1.015709039394145	-2.0915390045533004	22	1	1.05	0.95", "18	1	3.09	1.504	0	0	1	1.010768659694761	-2.2480278059122187	22	1	1.05	0.95", "19	1	15.0	7.4447	0	0	1	1.019321222824595	-2.2208389886407085	66	1	1.05	0.95", "20	1	0.0	0.0	0	0	1	1.0314262828710168	-1.5468497248543704	66	1	1.05	0.95", "21	1	0.0	0.0	0	0	1	1.0109635288110257	-3.320387251275851	22	1	1.05	0.95", "22	1	9.92	4.9125	0	0	1	1.0017227974429035	-2.928847999424379	22	1	1.05	0.95", "23	1	0.0	0.0	0	0	1	0.9742869183994061	-5.447723113181647	22	1	1.05	0.95", "24	3	1900.0	234.0678	0	0	1	1.0470835474971156	0.0	230	1	1.05	0.95", "25	1	24.3	11.8812	0	0	1	1.0150894779867858	-2.2901660515753433	66	1	1.05	0.95", "26	1	0.0	0.0	0	0	1	1.0131931162855303	-2.1598998741912774	22	1	1.05	0.95", "27	1	0.0	0.0	0	0	1	1.0198819811988222	-2.164937260283591	66	1	1.05	0.95", "28	1	3.45	1.6367	0	0	1	1.0025617220443173	-3.6944183331600167	22	1	1.05	0.95", "29	1	8.91	4.4553	0	0	1	0.9806558873171056	-5.33314290873868	22	1	1.05	0.95", "30	1	0.0	0.0	0	0	1	1.0465749711668877	0.6851677460175745	230	1	1.05	0.95", "31	1	2.57	1.2847	0	0	1	0.9750124263061519	-5.490343656182953	22	1	1.05	0.95", "32	1	24.5	10.4451	0	0	1	1.0102188797970064	-3.1798538524915485	66	1	1.05	0.95", "33	1	12.5	7.0727	0	0	1	0.9986682063527001	-3.5188870100572704	22	1	1.05	0.95", "34	1	7.95	3.1416	0	0	1	1.0101585764836976	-2.2583359081631316	22	1	1.05	0.95", "35	1	13.0	6.3335	0	0	1	1.0155872277282678	-2.1034729232039435	22	1	1.05	0.95", "36	1	11.7	5.657	0	0	1	1.0281203286968292	-2.372392814567118	66	1	1.05	0.95", "37	1	5.57	2.7184	0	0	1	1.0247263978197825	-0.9491748401262335	22	1	1.05	0.95", "38	1	5.74	2.9114	0	0	1	1.0003580815933786	-3.5084255635124477	22	1	1.05	0.95", "39	1	8.05	3.7108	0	0	1	1.0292196821839363	-0.856559090596288	22	1	1.05	0.95", "40	1	3.9	1.9357	0	0	1	1.0144564159095966	-2.1708842270507582	22	1	1.05	0.95", "41	1	1.65	0.919	0	0	1	1.0470088998913136	0.4602978709245258	230	1	1.05	0.95", "42	1	0.0	0.0	0	0	1	1.0132231625092423	-2.196555376046719	22	1	1.05	0.95", "43	1	4.38	2.0979	0	0	1	1.0143480611001823	-2.3132005744303368	22	1	1.05	0.95", "44	1	4.56	1.4989	0	0	1	1.0146780537052538	-3.2337227368855808	22	1	1.05	0.95", "45	1	0.0	0.0	0	0	1	0.9926852934097634	-5.081915136272162	22	1	1.05	0.95", "46	1	0.0	0.0	0	0	1	1.0217252669115617	-1.6253249495634243	66	1	1.05	0.95", "47	1	0.817	0.3945	0	0	1	1.0041609335175152	-3.6638048566483015	22	1	1.05	0.95", "48	1	0.0	0.0	0	0	1	1.0397427985367855	-0.19768757413674617	230	1	1.05	0.95", "49	1	42.9	20.848	0	0	1	1.0199056679148326	-2.2396845435080484	66	1	1.05	0.95", "50	1	16.2	8.1006	0	0	1	1.0092864227850877	-3.2695738636364475	66	1	1.05	0.95", "51	1	3.61	1.8034	0	0	1	0.9756575857222924	-5.451113088371486	22	1	1.05	0.95", "52	1	5.05	1.66	0	0	1	1.0003324760467558	-3.577233172934755	22	1	1.05	0.95", "53	1	0.762	0.3243	0	0	1	1.0007994342483146	-3.722788536029164	22	1	1.05	0.95", "54	1	0.0	0.0	0	0	1	1.0150238654311765	-2.1543922993283733	22	1	1.05	0.95", "55	1	8.07	2.6518	0	0	1	1.007876712663856	-3.400090100549882	22	1	1.05	0.95", "56	1	0.0	0.0	0	0	1	1.0138769449332654	-2.1454104848536746	22	1	1.05	0.95", "57	1	2.59	1.2849	0	0	1	1.0131916471361944	-2.159821546163444	22	1	1.05	0.95", "58	1	0.0	0.0	0	0	1	1.010694556137263	-2.7400552489605086	22	1	1.05	0.95", "59	1	0.0	0.0	0	0	1	0.9866318960022763	-5.209140182823183	22	1	1.05	0.95", "60	1	1.14	0.7864	0	0	1	1.0163752828532424	-2.0824473895367634	22	1	1.05	0.95", "61	1	12.9	6.1425	0	0	1	1.018706588619005	-2.2825474456894295	66	1	1.05	0.95", "62	1	4.77	2.4446	0	0	1	0.9762411925372378	-5.406950779468498	22	1	1.05	0.95", "63	1	8.77	4.4273	0	0	1	1.0099579980591622	-2.26274380342601	22	1	1.05	0.95", "64	1	0.0	0.0	0	0	1	1.0131984791524145	-3.25003257743904	22	1	1.05	0.95", "65	1	2.27	1.1003	0	0	1	1.0052794619747119	-3.639218668868015	22	1	1.05	0.95", "66	1	0.0	0.0	0	0	1	1.035104583082	-0.6324183754472611	66	1	1.05	0.95", "67	1	3.42	1.6786	0	0	1	1.0317342682625306	-0.7992135764115487	223	1	1.05	0.95", "68	1	25.9	12.529	0	0	1	1.0155160914935768	-2.247706257292519	66	1	1.05	0.95", "69	1	0.0	0.0	0	0	1	1.0017045735213128	-3.5032187153696763	22	1	1.05	0.95", "70	1	8.56	4.0362	0	0	1	1.0040415897202468	-3.4721743008070702	22	1	1.05	0.95", "71	1	6.28	3.1265	0	0	1	0.9832739769711522	-5.285264462274612	22	1	1.05	0.95", "72	1	4.29	2.1297	0	0	1	1.0058417299529938	-3.322216243999598	22	1	1.05	0.95", "73	1	0.848	0.4273	0	0	1	1.0317577510330693	-0.7999944688050417	22	1	1.05	0.95", "74	1	0.0	0.0	0	0	1	1.002693065970735	-3.4478720197901214	22	1	1.05	0.95", "75	1	27.9	13.8427	0	0	1	1.0000411400898985	-3.750999294210664	22	1	1.05	0.95", "76	1	0.0	0.0	0	0	1	0.9960990261861812	-3.048556362725481	22	1	1.05	0.95", "77	1	0.743	0.5184	0	0	1	1.0145738143429386	-2.1597268792131854	22	1	1.05	0.95", "78	1	120.0	57.0821	0	0	1	1.0047002378731988	-3.716949944797853	66	1	1.05	0.95", "79	1	0.0	0.0	0	0	1	0.9943924523000315	-5.035470121905695	22	1	1.05	0.95", "80	1	10.1	4.8509	0	0	1	0.9924329246657199	-3.9199732434467554	22	1	1.05	0.95", "81	1	0.0	0.0	0	0	1	0.9905259329753233	-3.6675875937639106	22	1	1.05	0.95", "82	1	4.38	1.8644	0	0	1	1.014313949781507	-3.2427138543892577	22	1	1.05	0.95", "83	1	6.49	3.2301	0	0	1	0.9976690223345195	-3.7975705235257897	22	1	1.05	0.95", "84	1	0.0	0.0	0	0	1	1.0222541019473124	-1.9852082541814127	66	1	1.05	0.95", "85	1	2.46	1.2233	0	0	1	1.015905873530394	-2.140967319759012	22	1	1.05	0.95", "86	1	2.48	1.26	0	0	1	1.011562974157431	-2.2311744116357137	22	1	1.05	0.95", "87	1	0.0	0.0	0	0	1	1.0084279314720044	-3.38411838955544	22	1	1.05	0.95", "88	1	16.0	7.7156	0	0	1	0.9907300777685931	-3.6632253857780888	22	1	1.05	0.95", "89	1	2.19	1.3601	0	0	1	1.015702515656055	-2.0901938465368364	22	1	1.05	0.95", "90	1	26.4	12.7903	0	0	1	1.0103597917523344	-3.16746546806798	66	1	1.05	0.95", "91	1	10.2	4.8354	0	0	1	0.9995284307189569	-3.4600840020844026	22	1	1.05	0.95", "92	1	0.0	0.0	0	0	1	1.0497316689091498	1.7178471295306368	230	1	1.05	0.95", "93	1	11.0	5.4261	0	0	1	1.0021859544459244	-3.482562969430192	22	1	1.05	0.95", "94	1	0.765	0.368	0	0	1	1.003952840865782	-3.6683782099820377	22	1	1.05	0.95", "95	1	16.3	5.3686	0	0	1	0.9954629674809009	-3.6971827291562382	22	1	1.05	0.95", "96	1	0.862	0.3802	0	0	1	1.0318901454223282	-0.7972826474351167	22	1	1.05	0.95", "97	1	3.8	2.5546	0	0	1	1.0016913530057672	-3.702850558016414	22	1	1.05	0.95", "98	1	2.58	1.2455	0	0	1	0.9955400426181368	-3.850855795049957	22	1	1.05	0.95", "99	1	0.0	0.0	0	0	1	1.0075914059741666	-3.284608782036414	22	1	1.05	0.95", "100	1	4.35	2.1667	0	0	1	1.0010035806704403	-3.556349401274197	22	1	1.05	0.95", "101	1	0.827	0.4049	0	0	1	1.0154144202181634	-2.107136176379279	22	1	1.05	0.95", "102	1	0.0	0.0	0	0	1	1.0118474034639575	-3.1938264161414334	22	1	1.05	0.95", "103	1	3.31	1.877	0	0	1	1.0002290395228473	-3.7313278817894426	22	1	1.05	0.95", "104	1	5.05	1.6584	0	0	1	0.9999998499041194	-3.587584291405311	22	1	1.05	0.95", "105	1	247.0	13.1662	0	0	1	1.0302301723524254	-1.752479860438736	66	1	1.05	0.95", "106	1	12.9	6.987	0	0	1	1.0091903543706597	-3.2768682391498203	66	1	1.05	0.95", "107	1	3.36	1.6479	0	0	1	1.000335165761457	-3.741055061149553	22	1	1.05	0.95", "108	1	14.1	6.8998	0	0	1	0.9994924654910988	-3.459309614244432	22	1	1.05	0.95", "109	1	0.0	0.0	0	0	1	1.0176358469661855	-2.0600634934877875	22	1	1.05	0.95", "110	1	3.3	1.508	0	0	1	0.9950042968207646	-3.8633031612686275	22	1	1.05	0.95", "111	1	0.0	0.0	0	0	1	1.0017053984554498	-3.503263151712545	22	1	1.05	0.95", "112	1	88.5	4.4022	0	0	1	1.0285537746613602	-2.32571255664549	66	1	1.05	0.95", "113	1	0.0	0.0	0	0	1	1.0171463334211348	-2.1150791691857758	22	1	1.05	0.95", "114	1	2.51	1.139	0	0	1	1.0257558465467402	-0.927470499971504	22	1	1.05	0.95", "115	1	0.0	0.0	0	0	1	1.0136465594150665	-2.9432033247766767	66	1	1.05	0.95", "116	1	0.0	0.0	0	0	1	1.0220937805357602	-1.9459447803663155	66	1	1.05	0.95", "117	1	0.0	0.0	0	0	1	1.0146703057061974	-2.1663802765436415	22	1	1.05	0.95", "118	1	11.3	5.4141	0	0	1	1.019353685922958	-2.217604008460052	66	1	1.05	0.95", "119	1	0.547	0.3161	0	0	1	1.016583535035278	-2.079511060380723	22	1	1.05	0.95", "120	1	5.02	2.5119	0	0	1	1.018672556780871	-2.2858524559744606	66	1	1.05	0.95", "121	1	0.0	0.0	0	0	1	1.0017036760944986	-3.503170403484459	22	1	1.05	0.95", "122	1	4.7	2.4093	0	0	1	0.9760701420455313	-5.410511015695649	22	1	1.05	0.95", "123	1	175.0	81.3182	0	0	1	1.0470024477442186	0.5679793462819862	230	1	1.05	0.95", "124	1	0.0	0.0	0	0	1	1.0445275408890533	0.46729740297980177	230	1	1.05	0.95", "125	1	3.6	1.79	0	0	1	0.9900278209111747	-3.678241396368547	22	1	1.05	0.95", "126	1	2.32	1.1503	0	0	1	1.0177269710074899	-2.063913851991996	22	1	1.05	0.95", "127	1	9.34	4.5094	0	0	1	1.0120541186941923	-2.3624058849531946	22	1	1.05	0.95", "128	1	105.0	51.8522	0	0	1	1.0252313792403975	-2.142724368147601	66	1	1.05	0.95", "129	1	0.0	0.0	0	0	1	1.0017052192603852	-3.5032534778914024	22	1	1.05	0.95", "130	1	0.0	0.0	0	0	1	1.0153053847879012	-2.292336676347726	22	1	1.05	0.95", "131	1	4.54	2.3267	0	0	1	0.9742868725889232	-5.447720575551449	22	1	1.05	0.95", "132	1	3.82	1.882	0	0	1	1.0147142112004992	-2.1276256587817493	22	1	1.05	0.95", "133	1	4.45	2.1716	0	0	1	1.003241991679355	-3.680439716488317	22	1	1.05	0.95", "134	1	13.4	5.3034	0	0	1	1.028173184617727	-2.367142877247744	66	1	1.05	0.95", "135	1	10.8	5.3041	0	0	1	1.0073023254455655	-3.373598056103863	22	1	1.05	0.95", "136	1	0.0	0.0	0	0	1	1.0150895802824382	-2.290166404245225	66	1	1.05	0.95", "137	1	3.21	1.6021	0	0	1	1.0162420220113786	-2.100176112786942	22	1	1.05	0.95", "138	1	2.52	1.2511	0	0	1	1.0105196681672477	-2.2532498364309057	22	1	1.05	0.95", "139	1	0.0	0.0	0	0	1	1.0339557523942184	-0.7528973165407975	22	1	1.05	0.95", "140	1	0.0	0.0	0	0	1	0.9926836838709917	-5.08182743634505	22	1	1.05	0.95", "141	1	0.0	0.0	0	0	1	1.0140833809235537	-2.1780709382854457	22	1	1.05	0.95", "142	1	9.86	4.866	0	0	1	1.003939000734296	-3.4454420899630294	22	1	1.05	0.95", "143	1	1.61	0.7789	0	0	1	1.0171754347673885	-2.0681525898873985	22	1	1.05	0.95", "144	1	0.0	0.0	0	0	1	0.9962196235023028	-3.046003928862946	22	1	1.05	0.95", "145	1	0.0	0.0	0	0	1	0.9876402003041292	-5.186384846352534	22	1	1.05	0.95", "146	1	0.784	0.3782	0	0	1	1.0049763153668734	-3.645876955126458	22	1	1.05	0.95", "147	1	2.95	1.4533	0	0	1	1.0100859867910035	-2.260104259821366	22	1	1.05	0.95", "148	1	0.0	0.0	0	0	1	0.9822106796367233	-5.30968337193437	22	1	1.05	0.95", "149	1	0.0	0.0	0	0	1	1.0211747447392796	-1.6915923439764329	66	1	1.05	0.95", "150	1	3.25	1.5715	0	0	1	1.0033168819459766	-3.376429675790674	22	1	1.05	0.95", "151	1	0.0	0.0	0	0	1	1.0252550130418108	-1.2960357630995536	66	1	1.05	0.95", "152	1	9.59	4.7993	0	0	1	1.0007760294387542	-3.731823219028276	22	1	1.05	0.95", "153	1	5.38	2.5919	0	0	1	1.014845145967365	-2.118848154248525	22	1	1.05	0.95", "154	1	2.47	1.3259	0	0	1	1.0162295956847214	-2.0890520392132337	22	1	1.05	0.95", "155	1	4.67	2.2997	0	0	1	1.004770864772843	-3.3451303103768653	22	1	1.05	0.95", "156	1	4.79	2.44	0	0	1	1.0060015833316112	-2.835474446291662	22	1	1.05	0.95", "157	1	0.0	0.0	0	0	1	1.006233892623041	-3.618469526219167	22	1	1.05	0.95", "158	1	0.0	0.0	0	0	1	1.0159864711723894	-2.137291531824135	22	1	1.05	0.95", "159	1	0.0	0.0	0	0	1	0.9947914491733929	-3.0762114554011806	22	1	1.05	0.95", "160	1	10.3	6.064	0	0	1	1.0157090376995361	-2.0915389145187198	22	1	1.05	0.95", "161	1	2.35	1.1626	0	0	1	1.0006677082017903	-3.7255847305011063	22	1	1.05	0.95", "162	1	4.36	2.1807	0	0	1	0.9947175874974765	-3.077776372453301	22	1	1.05	0.95", "163	1	0.0	0.0	0	0	1	1.0490467439493465	1.161902880227998	230	1	1.05	0.95", "164	1	14.5	7.2534	0	0	1	0.9944415785084146	-3.864629797375267	22	1	1.05	0.95", "165	1	0.0	0.0	0	0	1	1.0168128762678783	-3.1746675905989834	22	1	1.05	0.95", "166	1	0.0	0.0	0	0	1	1.0110691800687124	-3.1188640176865285	66	1	1.05	0.95", "167	1	0.0	0.0	0	0	1	1.0134838227646106	-2.153668683978873	22	1	1.05	0.95", "168	1	0.0	0.0	0	0	1	1.0240717100227668	-1.402224528203187	66	1	1.05	0.95", "169	1	24.6	12.2973	0	0	1	1.0107603777843956	-3.223995525330227	66	1	1.05	0.95", "170	1	3.88	1.9389	0	0	1	1.01521165906293	-2.1521288834867978	22	1	1.05	0.95", "171	1	0.0	0.0	0	0	1	1.0473479113778086	0.9096060186765248	230	1	1.05	0.95", "172	1	1.56	0.7669	0	0	1	1.0296564896813656	-0.8467600031833314	22	1	1.05	0.95", "173	1	2.4	1.1777	0	0	1	1.0021626867818496	-3.7054786034393805	22	1	1.05	0.95", "174	1	112.0	56.5455	0	0	1	1.009513192875858	-3.184892717772294	66	1	1.05	0.95", "175	1	0.0	0.0	0	0	1	1.0027227115381883	-3.3892864663307525	22	1	1.05	0.95", "176	1	0.0	0.0	0	0	1	1.0154144430280272	-2.1071373900185053	22	1	1.05	0.95", "177	1	0.0	0.0	0	0	1	1.015531242349914	-2.2161105621097836	66	1	1.05	0.95", "178	1	29.4	14.0764	0	0	1	1.0096106207159066	-2.8068445262732777	66	1	1.05	0.95", "179	1	0.0	0.0	0	0	1	0.9999999831521093	-3.5875914885583198	22	1	1	0.95", "180	1	123.0	60.2394	0	0	1	1.045425635621036	0.6609876762680146	230	1	1.05	0.95", "181	1	7.63	3.7866	0	0	1	0.9747484911803055	-5.4708566469671736	22	1	1.05	0.95", "182	1	12.9	6.6253	0	0	1	0.9696518160229312	-5.544711347657365	22	1	1.05	0.95", "183	1	0.0	0.0	0	0	1	1.0119160128474596	-2.222442985251821	22	1	1.05	0.95", "184	1	25.1	12.5267	0	0	1	1.0092481298532492	-3.273238612900325	66	1	1.05	0.95", "185	1	0.0	0.0	0	0	1	1.0156933401317922	-2.1429040008296374	22	1	1.05	0.95", "186	1	0.0	0.0	0	0	1	1.0100915281115574	-3.3182157558614342	22	1	1.05	0.95", "187	1	12.5	5.1147	0	0	1	0.9758474675672868	-5.472282086353156	22	1	1.05	0.95", "188	1	0.0	0.0	0	0	1	1.0110691800687124	-3.1188640176865285	66	1	1.05	0.95", "189	1	0.0	0.0	0	0	1	1.0452052830257204	0.4211361110834393	230	1	1.05	0.95", "190	1	2.56	1.2609	0	0	1	1.0166862898468987	-2.124702722270478	22	1	1.05	0.95", "191	1	16.0	7.9016	0	0	1	0.9987682768032443	-2.991985498575482	22	1	1.05	0.95", "192	1	3.41	1.5924	0	0	1	1.0009718033273522	-3.7189751314476784	22	1	1.05	0.95", "193	1	17.6	8.8221	0	0	1	0.9791635211293432	-5.366406835552541	22	1	1.05	0.95", "194	1	27.6	13.7716	0	0	1	1.0171009090987497	-2.0899007706410875	66	1	1.05	0.95", "195	1	22.1	11.0388	0	0	1	1.0118338084683303	-2.581107971377223	66	1	1.05	0.95", "196	1	12.6	6.2227	0	0	1	0.9765837238218514	-5.4310644159329	22	1	1.05	0.95", "197	1	0.0	0.0	0	0	1	1.0180261670104653	-2.057668382573241	22	1	1.05	0.95", "198	1	12.2	4.8092	0	0	1	0.9926831106066859	-5.081796299318689	22	1	1.05	0.95", "199	1	0.0	0.0	0	0	1	1.0346360054700947	-0.6817980299501213	66	1	1.05	0.95", "200	1	35.0	17.5934	0	0	1	1.0119828519174079	-3.1045598505509533	66	1	1.05	0.95", "201	1	5.34	2.2768	0	0	1	0.9911823004318456	-3.6532971488094392	22	1	1.05	0.95", "202	1	0.0	0.0	0	0	1	1.010404184216204	-2.251817294349881	22	1	1.05	0.95", "203	1	3.43	1.6948	0	0	1	0.9991370940877246	-3.467444643699668	22	1	1.05	0.95", "204	1	0.0	0.0	0	0	1	1.0130175200752751	-2.182581244324115	22	1	1.05	0.95", "205	1	4.73	2.4171	0	0	1	1.0165166359632045	-2.0807398315867425	22	1	1.05	0.95", "206	1	178.0	87.9117	0	0	1	1.0452797238141518	0.6398606506266254	230	1	1.05	0.95", "207	1	0.0	0.0	0	0	1	1.0317344040181355	-0.7992206719218148	22	1	1.05	0.95", "208	1	14.1	6.6452	0	0	1	1.0082417928166314	-3.3888072390279502	22	1	1.05	0.95"};
			for (int i = 0; i < fixedrList3.size(); i++){
			  String[] arr = fixedrList3.get(i);
			  String str = String.join(",", arr);
			  //System.out.println(str);
			  //System.out.println(busBase[i]);
			  assertEquals(str, busBase[i]);
			}
			//Check that the base generator input data hasn't changed. 
			String[] genBase = {"30	143.10531131368188	63.71479730928525	400	-400	1	100	1	245	143	0	0	0	0	0	0	0	0	0	0	0", "92	141.37128869521203	14.09833290606645	400	-400	1	100	1	250	120	0	0	0	0	0	0	0	0	0	0	0", "92	141.37128869521203	14.09833290606645	400	-400	1	100	1	250	120	0	0	0	0	0	0	0	0	0	0	0", "24	392.9989442883181	341.61414425016085	400	-400	1	100	1	400	0	0	0	0	0	0	0	0	0	0	0	0", "92	119.99998476606959	14.09833290606645	400	-400	1	100	1	120	80	0	0	0	0	0	0	0	0	0	0	0", "189	143.154401094758	76.84494335782307	400	-400	1	100	1	400	80	0	0	0	0	0	0	0	0	0	0	0", "92	141.37128869521203	14.09833290606645	400	-400	1	0	1	250	120	0	0	0	0	0	0	0	0	0	0	0", "189	143.15438763925837	76.84494335782307	400	-400	1	100	1	170	25	0	0	0	0	0	0	0	0	0	0	0", "92	89.99999362981966	14.09833290606645	400	-400	1	100	1	90	70	0	0	0	0	0	0	0	0	0	0	0", "92	114.99998763406487	14.09833290606645	400	-400	1	100	1	115	80	0	0	0	0	0	0	0	0	0	0	0", "92	141.37129109831844	14.09833290606645	400	-400	1	100	1	292	122	0	0	0	0	0	0	0	0	0	0	0", "92	141.37129109831844	14.09833290606645	400	-400	1	100	1	292	122	0	0	0	0	0	0	0	0	0	0	0", "92	141.37128916694462	14.09833290606645	400	-400	1	100	1	270	120	0	0	0	0	0	0	0	0	0	0	0", "163	139.9998867494065	43.09942144535257	400	-400	1	100	1	140	120	0	0	0	0	0	0	0	0	0	0	0", "92	114.99998763406487	14.09833290606645	400	-400	1	100	1	115	80	0	0	0	0	0	0	0	0	0	0	0", "92	141.37128869521203	14.09833290606645	400	-400	1	100	1	250	120	0	0	0	0	0	0	0	0	0	0	0", "92	141.37128916694462	14.09833290606645	400	-400	1	100	1	270	120	0	0	0	0	0	0	0	0	0	0	0", "189	143.15440109475782	76.84494335782307	400	-400	1	100	1	400	80	0	0	0	0	0	0	0	0	0	0	0", "163	142.13123395220367	43.09942144535257	400	-400	1	100	1	260	120	0	0	0	0	0	0	0	0	0	0	0", "92	141.37128869521203	14.09833290606645	400	-400	1	100	1	250	120	0	0	0	0	0	0	0	0	0	0	0", "30	143.10531131368188	63.71479730928525	400	-400	1	100	1	245	143	0	0	0	0	0	0	0	0	0	0	0", "189	143.15438763925837	76.84494335782307	400	-400	1	100	1	170	25	0	0	0	0	0	0	0	0	0	0	0", "30	142.792837609134	63.71479730928525	400	-400	1	100	1	400	80	0	0	0	0	0	0	0	0	0	0	0", "92	89.99999362981966	14.09833290606645	400	-400	1	100	1	90	70	0	0	0	0	0	0	0	0	0	0	0", "189	143.15438763925837	76.84494335782307	400	-400	1	100	1	170	25	0	0	0	0	0	0	0	0	0	0	0", "163	139.9998867494065	43.09942144535257	400	-400	1	100	1	140	120	0	0	0	0	0	0	0	0	0	0	0", "30	143.11073669616653	63.71479730928525	400	-400	1	100	1	300	143	0	0	0	0	0	0	0	0	0	0	0", "92	141.37129025255223	14.09833290606645	400	-400	1	100	1	250	122	0	0	0	0	0	0	0	0	0	0	0", "163	142.13123395220367	43.09942144535257	400	-400	1	100	1	260	120	0	0	0	0	0	0	0	0	0	0	0"};
			for (int i = 0; i < fixedrList5.size(); i++){
			  String[] arr = fixedrList5.get(i);
			  String str = String.join(",", arr);
			  //System.out.println(str);
			  //System.out.println(genBase[i]);
			  assertEquals(str, genBase[i]);
			}
			//Now check that this input creates the correct output. 
			//Check that the branch output data hasn't changed. 
			String[] branchBaseOut = {"1	0.04913282553648912	0.04774875485262697	0.02456641276824456	0.023874377426313487	0.03425630648207475", "2	0.00037134720665688015	0.0021727708825772796	0.00018567360332844007	-0.0010863854412886398	0.0011021379287624936", "3	0.006720456330775448	0.11011125076222328	0.003360228165387724	0.05505562538111164	0.05515807302135172", "4	0.0	0.0	0.0	0.0	0.0", "5	1.630482679149054	27.85627738345812	0.815241339574527	13.92813869172906	13.9519771307794", "6	0.0701674069766991	0.07127643359104585	0.03508370348834955	0.035638216795522926	0.05000948656828145", "7	0.1745854144167538	2.859500081862137	0.0872927072083769	1.4297500409310686	1.4324123694921653", "8	0.013252929377863154	0.2167266985049423	0.006626464688931577	0.10836334925247115	0.10856576576198732", "9	0.0004063575601103153	0.0003512282841984238	0.00020317878005515766	-0.0001756141420992119	0.0002685552895959123", "10	0.0002958540506927676	0.0009727824231768878	0.0001479270253463838	0.0004863912115884439	0.0005083884494540608", "11	0.14367422269006624	2.3502365894612325	0.07183711134503312	1.1751182947306162	1.1773120135193516", "12	0.005810184405864227	0.0026996742790608153	0.0029050922029321136	0.0013498371395304076	0.00320337650156715", "13	0.02568464210418142	0.4154916753933726	0.01284232105209071	0.2077458376966863	0.20814239906924994", "14	0.002596615709284933	0.001761016651220526	0.0012983078546424665	0.000880508325610263	0.0015687250227159994", "15	0.0005068972924986426	0.00026185730063654766	0.0002534486462493213	-0.00013092865031827383	0.0002852692197903209", "16	0.00060620190695726	0.0010847229467507713	0.00030310095347863	-0.0005423614733753856	0.0006213100319498904", "17	0.011662546176028243	0.011206704491155683	0.005831273088014122	0.005603352245577842	0.008087107159857596", "18	1.9236709830872226e-09	0.002307847164747777	9.618354915436113e-10	-0.0011539235823738885	0.0011539235823742895", "19	0.0	0.0	0.0	0.0	0.0", "20	0.04993472556465406	0.8176057580632516	0.02496736278232703	0.4088028790316258	0.4095646018747841", "21	0.0004315195773720859	0.00039650884565034517	0.00021575978868604295	-0.00019825442282517258	0.0002930138266082135", "22	0.0	0.0	0.0	0.0	0.0", "23	0.08380601227193196	0.08578486925601769	0.04190300613596598	0.042892434628008846	0.05996351283529689", "24	0.005418061465718438	0.08856742652709926	0.002709030732859219	0.04428371326354963	0.04436649758454973", "25	0.04529001739470573	0.7436463778126168	0.022645008697352864	0.3718231889063084	0.37251212091334623", "26	0.02269069891136155	0.021847638729070695	0.011345349455680775	0.010923819364535348	0.015749501064494503", "27	5.866532829459842e-12	0.0007341193161127383	4.3752814076803085e-12	-0.00036705965810942534	0.0003670596581094254", "28	0.17405450650394982	0.17475125470569353	0.08702725325197491	0.08737562735284676	0.12332170556672868", "29	0.0001292686935717713	0.0020404365238824718	6.463434678588564e-05	-0.0010202182619412359	0.0010222636160907002", "30	0.002246304628271467	0.001925803907157908	0.0011231523141357336	-0.000962901953578954	0.0014794091026334194", "31	0.707863548602063	12.116677736432806	0.3539317743010315	6.058338868216403	6.0686685148392705", "32	2.280421403710875e-05	0.00024066187055149513	1.1402107018554375e-05	0.00012033093527574756	0.00012086993848264636", "33	0.003369147449721055	0.0017628444101371876	0.0016845737248605275	0.0008814222050685938	0.0019012347929907168", "34	0.0	0.0	0.0	0.0	0.0", "35	0.08624001242378654	1.4078703150903138	0.04312000621189327	0.7039351575451569	0.7052545930115156", "36	0.0014491139870274061	0.00024114493769333833	0.0007245569935137031	0.00012057246884666917	0.000734520630815362", "37	0.008932960640141374	0.14548021929418375	0.004466480320070687	0.07274010964709188	0.07287710887460154", "38	0.022586543522876923	0.3692571546530843	0.011293271761438461	0.18462857732654214	0.1849736455625521", "39	0.0003176345175059936	0.00024594504232222647	0.0001588172587529968	0.00012297252116111324	0.00020086105306538841", "40	0.00028280073058795097	0.004627374532084616	0.00014140036529397548	0.002313687266042308	0.0023180040613319896", "41	0.0015425100986048434	0.0009652555871846324	0.0007712550493024217	-0.0004826277935923162	0.0009098153319340516", "42	0.019892883156369834	0.01834806390361532	0.009946441578184917	0.00917403195180766	0.0135312439310299", "43	0.0046791241961496866	0.004278135382123072	0.0023395620980748433	-0.002139067691061536	0.003170041229651705", "44	0.15173464098845457	0.1468978334838642	0.07586732049422729	0.0734489167419321	0.10559637157372916", "45	0.0	0.0	0.0	-0.0	0.0", "46	0.19153030233255208	3.1487937829399826	0.09576515116627604	1.5743968914699913	1.5773067349276966", "47	0.0027217824189023077	0.02228072461014108	0.0013608912094511538	0.01114036230507054	0.011223176786106417", "48	0.007230260227950147	0.006488272583031307	0.0036151301139750736	0.0032441362915156535	0.0048573229271789585", "49	1.6990496145071564e-10	0.0020168610067128187	8.53294389682936e-11	-0.0010084305033564093	0.001008430503356413", "50	0.022774438295801502	0.02285186725744026	0.011387219147900751	0.01142593362872013	0.016131358256831166", "51	0.05846403870843986	0.057535467184499645	0.02923201935421993	0.028767733592249822	0.04101333260733816", "52	0.035784351558028504	0.03071512607265614	0.017892175779014252	0.01535756303632807	0.023579327736853197", "53	0.05538740807020304	6.644675959977789	0.02769370403510152	3.3223379799888946	3.3224533999019266", "54	0.006006577704716687	0.0007417815008539108	0.0030032888523583434	0.0003708907504269554	0.0030261037456525124", "55	0.017360305029116674	0.017114372587142768	0.008680152514558337	0.008557186293571384	0.012188949295935206", "56	1.126124698784961e-10	0.0018308981526604972	5.7039255005602366e-11	-0.0009154490763404741	0.0009154490763404759", "57	0.2539583840274915	0.25112190850637717	0.12697919201374575	0.12556095425318858	0.17857566586025944", "58	0.006796510217239593	0.004392706672271984	0.0033982551086197965	0.002196353336135992	0.004046245884819192", "59	0.006508389902318168	0.001424223972491978	0.003254194951159084	0.000712111986245989	0.0033311992226680892", "60	0.0013808103573760278	0.0008780788674014328	0.0006904051786880139	-0.0004390394337007164	0.0008181778138665666", "61	0.07122877064477073	0.06108209249550445	0.03561438532238537	0.030541046247752224	0.04691630790885701", "62	0.07629377317711494	0.07658259100687825	0.03814688658855747	0.03829129550343913	0.05405005335549599", "63	0.0007526356367932863	0.06034646096807261	0.00037631781839664313	0.030173230484036306	0.030175577093789287", "64	2.1772722719347805e-10	0.0034699991891336645	1.0886361359673903e-10	-0.0017349995945668323	0.0017349995945668357", "65	0.21765153215056898	3.5543094727451745	0.10882576607528449	1.7771547363725873	1.7804836433883329", "66	0.055467450741033986	0.9063574686419855	0.027733725370516993	0.45317873432099276	0.45402656834562466", "67	0.016973284334016014	0.01599412443660153	0.008486642167008007	0.007997062218300766	0.01166087901464602", "68	0.034488881578013775	0.03332689772489417	0.017244440789006887	0.016663448862447083	0.02398001805914569", "69	0.04950769821721224	3.9695361633637845	0.02475384910860612	1.9847680816818922	1.9849224395700982", "70	0.21204129789873605	3.624125313986724	0.10602064894936802	1.812062656993362	1.8151615495248592", "71	0.017580930043056497	0.017648082542525145	0.008790465021528249	0.008824041271262573	0.012455359475007408", "72	0.350034667993409	0.35517254809968435	0.1750173339967045	0.17758627404984217	0.24933501946220804", "73	0.0633269022313101	1.0348019887740918	0.03166345111565505	0.5174009943870459	0.5183689449892397", "74	0.010024082476888907	0.17132470864277138	0.0050120412384444535	0.08566235432138569	0.08580885446886291", "75	0.001094525700874982	0.0006521992441310687	0.000547262850437491	0.00032609962206553433	0.0006370538367989412", "76	0.00026418677735495777	0.00024048709228585707	0.00013209338867747888	-0.00012024354614292854	0.00017862579243022573", "77	0.11737835060063162	14.143129129740231	0.05868917530031581	7.0715645648701155	7.0718081007917695", "78	0.001279596716101672	0.0006413853480700116	0.000639798358050836	0.0003206926740350058	0.0007156715239160129", "79	0.0004418343978409567	0.00020069608981332365	0.00022091719892047834	-0.00010034804490666183	0.0002426399367281891", "80	0.015886603203561833	0.0155744039491168	0.007943301601780917	0.0077872019745584	0.011123693403246131", "81	0.16132459954917522	2.7613096761635347	0.08066229977458761	1.3806548380817674	1.38300910644996", "82	0.0015874609994730449	0.0005023341261880532	0.0007937304997365224	-0.0002511670630940266	0.0008325220716565228", "83	0.0006095252845321752	0.00027417798590456677	0.0003047626422660876	-0.00013708899295228338	0.0003341760914692709", "84	0.002445026707849962	0.1960426819803942	0.001222513353924981	0.0980213409901971	0.09802896423209326", "85	0.0001934878611109525	0.003152308505057988	9.674393055547625e-05	0.001576154252528994	0.0015791205197401986", "86	0.00011495063240973824	0.00160065478421334	5.747531620486912e-05	-0.00080032739210667	0.0008023885259206497", "87	0.0	0.0	0.0	0.0	0.0", "88	8.308493044140164e-05	0.0012689374776675422	4.154246522070082e-05	0.0006344687388337711	0.0006358273012178143", "89	0.26829538540040687	0.2722926492738509	0.13414769270020344	0.13614632463692544	0.19113195747684714", "90	0.023921801150338595	0.022333064806490555	0.011960900575169298	0.011166532403245277	0.016363208379832236", "91	0.04315276865027684	0.03956449919667282	0.02157638432513842	0.01978224959833641	0.029272474437890623", "92	0.005567838404253678	0.0035434976897850134	0.002783919202126839	0.0017717488448925067	0.0032998939518336747", "93	0.006820722747299612	0.10988691234940617	0.003410361373649806	0.05494345617470309	0.055049195644626826", "94	0.0038215298976105316	0.0031676456730620473	0.0019107649488052658	0.0015838228365310236	0.002481837518271486", "95	0.00183987467043778	0.04589687357236727	0.00091993733521889	0.022948436786183635	0.022966868215544667", "96	0.00871593117920888	0.006845092340101111	0.00435796558960444	0.0034225461700505555	0.005541271186858129", "97	0.26427307564086533	0.2622998656448381	0.13213653782043266	0.13114993282241905	0.18617295589450059", "98	0.016754259533007243	0.014890681572588882	0.008377129766503622	0.007445340786294441	0.011207560062248186", "99	0.007299975242180068	0.007026073269949862	0.003649987621090034	0.003513036634974931	0.005065948680433555", "100	0.014147490045420597	0.011841303113888202	0.007073745022710298	0.005920651556944101	0.009224531614399905", "101	0.002063395415875746	0.0020571205241779467	0.001031697707937873	0.0010285602620889733	0.0014568240708173375", "102	0.1130149066608439	0.11092382870447892	0.05650745333042195	0.05546191435223946	0.0791777508237318", "103	0.3232640002787548	5.51860586650082	0.1616320001393774	2.75930293325041	2.764032847292769", "104	0.0017987452424650385	0.0010724530852033176	0.0008993726212325193	0.0005362265426016588	0.0010470959921674713", "105	0.288640614969367	4.737795562959512	0.1443203074846835	2.368897781479756	2.373289921238487", "106	0.0800822618181769	6.42101018181998	0.04004113090908845	3.21050509090999	3.2107547758935815", "107	0.1104271150814995	0.10872462173616881	0.05521355754074975	0.05436231086808441	0.07748417760565013", "108	0.023271241076859184	0.022292370296684183	0.011635620538429592	0.011146185148342092	0.016112886416637037", "109	0.0051011463772283605	0.4090108356518627	0.0025505731886141803	0.20450541782593135	0.20452132246724114", "110	0.0013987468967862071	0.022656347452503667	0.0006993734483931036	0.011328173726251833	0.011349741987924641", "111	0.24035721439486224	3.933354917482802	0.12017860719743112	1.966677458741401	1.970345940272786", "112	0.15728353602332845	2.688606414087502	0.07864176801166423	1.344303207043751	1.34660151497914", "113	0.042448123370622426	0.0387708864570353	0.021224061685311213	0.01938544322851765	0.0287446726122942", "114	0.010851737799329797	0.009819422442726378	0.0054258688996648985	0.004909711221363189	0.007317466610346123", "115	0.030481848899839292	0.12460391684120253	0.015240924449919646	0.06230195842060127	0.06413906610740827", "116	0.07117671558984284	1.1650100771301766	0.03558835779492142	0.5825050385650883	0.583591167825777", "117	0.018492694361555095	0.30243626456578454	0.009246347180777548	0.15121813228289227	0.151500555996715", "118	0.019531157646420994	0.019602119538729035	0.009765578823210497	0.009801059769364517	0.013835725581081499", "119	0.0	0.0	0.0	0.0	0.0", "120	7.012661592133813e-09	0.0019609790007780216	3.5063307960669064e-09	-0.0009804895003890108	0.0009804895003952803", "121	0.0005820814181700662	0.0004129218714969962	0.0002910407090850331	-0.0002064609357484981	0.0003568344326643161", "122	0.22165128068079554	0.22597601643020404	0.11082564034039777	0.11298800821510202	0.1582675347545262", "123	0.045375361087408805	0.04421026548854101	0.022687680543704403	0.022105132744270506	0.031675980523024116", "124	0.003293252911646505	0.05376774902428849	0.0016466264558232524	0.026883874512144246	0.026934254908382672", "125	0.110429096274828	0.45176877861578646	0.055214548137414	0.22588438930789323	0.23253473645676828", "126	0.42297201573159526	0.4231290108186929	0.21148600786579763	0.21156450540934646	0.2991418918709207", "127	0.0017963103283187287	0.0004505080259438188	0.0008981551641593644	-0.0002252540129719094	0.0009259708792754143", "128	0.0002526120029311407	0.00017293197472612043	0.00012630600146557036	-8.646598736306021e-05	0.00015306721718542332", "129	0.005989480553872717	0.004200867876696268	0.0029947402769363585	0.002100433938348134	0.0036579081529843683", "130	0.05267501855365708	4.223492478626113	0.02633750927682854	2.1117462393130566	2.111910472450891", "131	0.003597989654345213	0.002660557959565324	0.0017989948271726064	0.001330278979782662	0.0022374147027865434", "132	0.04156813426830652	0.041646536040468085	0.02078406713415326	0.020823268020234043	0.02942084189277206", "133	0.02056075562218851	0.0202422635510624	0.010280377811094255	0.0101211317755312	0.014426485239187903", "134	0.1507908653696859	18.085048232249306	0.07539543268484294	9.042524116124653	9.042838429495777", "135	0.00013511925431886063	0.0007565986430341232	6.755962715943031e-05	-0.0003782993215170616	0.00038428463393972746", "136	4.55027556573731e-12	0.0014940897136939317	2.320180809773284e-12	-0.0007470448568487672	0.0007470448568487672", "137	0.0008205565625174494	0.00036375505324603985	0.0004102782812587247	-0.00018187752662301993	0.00044878469533075714", "138	0.0	0.0	0.0	0.0	0.0", "139	0.003592826574895902	0.0021436900421893945	0.001796413287447951	0.0010718450210946973	0.00209187773269975", "140	0.008799433515594757	0.00805002214447903	0.004399716757797378	0.004025011072239515	0.005963071497181109", "141	0.021205986685398415	0.021040033348356246	0.010602993342699207	0.010520016674178123	0.014936338863667678", "142	0.0033322988446933266	0.0032466399558295223	0.0016661494223466633	0.0016233199779147611	0.0023262032689090823", "143	0.00011723057327894804	0.001699561960818774	5.861528663947402e-05	-0.000849780980409387	0.0008518001329498409", "144	2.845210141724379e-11	0.0010742336618668896	1.4226050708621895e-11	-0.0005371168309334448	0.000537116830933445", "145	0.10460319501175164	0.10181044188669652	0.05230159750587582	0.05090522094334826	0.07298492050388038", "146	0.015333738241210426	0.014162696640101835	0.007666869120605213	0.0070813483200509175	0.010436779970008839", "147	0.03848132034760354	0.037469599762189	0.01924066017380177	0.0187347998810945	0.02685508760939685", "148	0.0007910838339952875	0.012819252796996139	0.00039554191699764374	0.0064096263984980695	0.006421819366536675", "149	0.00028149666707100174	0.004057929171709329	0.00014074833353550087	-0.0020289645858546645	0.0020338405503001976", "150	0.15753857406684801	2.692310072760563	0.07876928703342401	1.3461550363802814	1.348457630981319", "151	0.008883364672804817	0.007927077240147185	0.004441682336402408	0.003963538620073592	0.005952997595356816", "152	0.04958714234314243	0.04892076031327264	0.024793571171571216	0.02446038015663632	0.03482859986917268", "153	2.181632882298983e-11	0.0009777119796936674	1.0908164411494915e-11	-0.0004888559898468337	0.0004888559898468338", "154	0.056866842270025586	0.057256801987279005	0.028433421135012793	0.028628400993639502	0.04034903692646965", "155	0.008057859999594186	0.007303098954391629	0.004028929999797093	0.0036515494771958146	0.005437470967984475", "156	0.03793049181695096	0.03757586102339161	0.01896524590847548	0.018787930511695805	0.02669582149478184", "157	0.08243478758211076	0.08270064129397081	0.04121739379105538	0.041350320646985406	0.05838426644683855", "158	0.0009768837327568392	0.0007525737332914062	0.0004884418663784196	0.0003762868666457031	0.0006165770534501548", "159	0.024467452384238086	0.023070737242137263	0.012233726192119043	0.011535368621068631	0.016814540902673328", "160	0.21291766099314913	0.21642676751032042	0.10645883049657456	0.10821338375516021	0.15180124839552528", "161	0.0014194523871875475	0.0005486206682849826	0.0007097261935937738	0.0002743103341424913	0.0007608925215104115", "162	1.2526042048044417e-05	9.171477956426699e-05	6.263021024022208e-06	4.5857389782133495e-05	4.6283103072048506e-05", "163	0.005881568663285819	0.0056642584008628205	0.0029407843316429094	0.0028321292004314103	0.004082789278565904", "164	0.0265533841179888	2.1290551229741723	0.0132766920589944	1.0645275614870862	1.0646103511227343", "165	9.125379277563996e-09	0.011923602510171933	4.562689638781998e-09	-0.005961801255406012	0.005961801255407758", "166	0.0	0.0	0.0	0.0	0.0", "167	0.05697185630367585	0.04920560519689943	0.028485928151837925	0.024602802598449713	0.03763968648076065", "168	0.16565488674200424	2.8373481231904236	0.08282744337100212	1.4186740615952118	1.4210898910408276", "169	0.12636038505267067	0.12523510644519575	0.06318019252633533	0.06261755322259788	0.08895332876992113", "170	0.0004370670287805467	2.241634077471133e-05	0.00021853351439027335	1.1208170387355665e-05	0.00021882074854820283", "171	0.5876267538840239	10.04382924789941	0.29381337694201193	5.021914623949705	5.030502240404031", "172	0.0013258837793195255	0.001271806979315393	0.0006629418896597628	0.0006359034896576965	0.0009186213568301649", "173	8.871371830998509e-05	0.005260615175212424	4.4356859154992545e-05	-0.002630307587606212	0.00263068157259158", "174	0.07994745086022803	0.08148369541971867	0.039973725430114015	0.040741847709859336	0.05707711344817215", "175	0.21845692157535268	26.207679789640224	0.10922846078767634	13.103839894820112	13.104295129678864", "176	0.08786756947137064	1.4367796640725885	0.04393378473568532	0.7183898320362943	0.7197319835982952", "177	0.0014785874618734596	0.0026377336577005916	0.0007392937309367298	-0.0013188668288502958	0.0015119407835110425", "178	0.015721199290718246	0.005250226880005648	0.007860599645359123	0.002625113440002824	0.008287354665845032", "179	0.4350844768786466	0.4385932182286236	0.2175422384393233	0.2192966091143118	0.3088942023965919", "180	0.11221066369566657	0.1119635067744511	0.056105331847833284	0.05598175338722555	0.07925758622405578", "181	0.0024007876278684392	0.0009037317330697903	0.0012003938139342196	0.00045186586653489513	0.0012826254597000926", "182	0.09286586740614311	0.09007067812647485	0.046432933703071555	0.04503533906323742	0.06468538549637466", "183	0.017121659447722593	0.013493463504253178	0.008560829723861296	0.006746731752126589	0.01089982545255199", "184	0.0013553756872184408	0.02186637511052325	0.0006776878436092204	0.010933187555261625	0.010954170481232406", "185	0.0	0.0	0.0	0.0	0.0", "186	0.0007730646381443051	0.0007284914665284248	0.0003865323190721526	0.0003642457332642124	0.000531114100630439", "187	0.025745150117458593	0.025011028279624092	0.012872575058729296	0.012505514139812046	0.017946895902736456", "188	0.006461169650895826	0.004975294009341269	0.003230584825447913	0.0024876470046706345	0.004077384692944866", "189	0.1360163296173873	0.12213340103763848	0.06800816480869365	0.06106670051881924	0.09140159950953591", "190	0.0009074013819212468	0.0008478027080993655	0.0004537006909606234	0.00042390135404968277	0.0006209159966882006", "191	0.007932060210247727	0.007282992622458906	0.003966030105123863	0.003641496311229453	0.005384226052038168", "192	0.0010471220693806238	0.003026317394462019	0.0005235610346903119	-0.0015131586972310096	0.0016011762551486443", "193	0.13831532980781702	0.13905768350901582	0.06915766490390851	0.06952884175450791	0.09806652053929873", "194	0.1561451879622382	2.671298835521924	0.0780725939811191	1.335649417760962	1.3379292571343737", "195	0.02898371142472911	0.028712480556333908	0.014491855712364554	0.014356240278166954	0.0203989096991103", "196	0.0018549265493215117	0.001212347592534968	0.0009274632746607558	0.000606173796267484	0.0011079868217292955", "197	0.09701927617804529	1.5886616717221695	0.048509638089022644	0.7943308358610848	0.7958106946927125", "198	0.004329306956565304	0.0025797638164286596	0.002164653478282652	0.0012898819082143298	0.0025198253943834728", "199	0.01620056779392165	0.015722759237087658	0.008100283896960825	0.007861379618543829	0.011287864666012752", "200	0.009118061211600192	0.008440428288627455	0.004559030605800096	-0.0042202141443137275	0.006212484807907996", "201	3.5801505543350526e-12	0.00020014295723666924	1.7900752771675263e-12	-0.00010007148417741017	0.00010007148417741019", "202	0.004525564352533706	0.36286056520281207	0.002262782176266853	0.18143028260140603	0.18144439266068071", "203	0.0015753985457775954	0.0014000544678220983	0.0007876992728887977	0.0007000272339110492	0.0010538065632395232", "204	0.018262565010996212	0.011586971634543364	0.009131282505498106	0.005793485817271682	0.010814101817078656", "205	0.010760217398392058	0.1758868997510561	0.005380108699196029	0.08794344987552805	0.08810786540158988", "206	0.00491868904053927	0.004055629515976378	0.002459344520269635	0.002027814757988189	0.003187539515377806", "207	0.004034924917924343	0.06523425815929862	0.0020174624589621715	0.03261712907964931	0.032679462421708015", "208	4.013328795540512e-09	0.005871912092414462	2.0068510322906982e-09	-0.002935956046207231	0.002935956046207917", "209	0.0006581545498129948	0.0005458341609023698	0.0003290772749064974	-0.0002729170804511849	0.00042752261421109063", "210	0.08496639296697595	1.3935119400517948	0.04248319648348797	0.6967559700258974	0.6980499292673726", "211	0.28465037036929175	0.285711405304383	0.14232518518464587	0.1428557026521915	0.2016536885804351", "212	0.0685594779819283	0.06836120925883549	0.03427973899096415	0.03418060462941774	0.04840882396961531", "213	0.023954912566043873	0.023661142733022444	0.011977456283021937	0.011830571366511222	0.016835138189804585", "214	0.05899876226031964	0.053627175322310094	0.02949938113015982	0.026813587661155047	0.03986454527929392", "215	0.004123315000107652	0.06740488228506791	0.002061657500053826	0.033702441142533957	0.03376544047711349", "216	0.0005566643487653344	0.00047618450533915	0.0002783321743826672	-0.000238092252669575	0.00036627410511508486", "217	0.008350521111097464	0.6695462872868951	0.004175260555548732	0.33477314364344757	0.3347991793682044", "218	4.458960947775566e-09	0.0019265446776206624	2.229480473887783e-09	-0.0009632723388103312	0.0009632723388129113", "219	0.002781481148048215	0.0024762541353062772	0.0013907405740241074	0.0012381270676531386	0.0018620198656008652"};
			for (int i = 0; i < fixedrList2.size(); i++){
			  String[] arr = fixedrList2.get(i);
			  String str = String.join(",", arr);
			  //System.out.println(str);
			  //System.out.println(branchBaseOut[i]);
			  assertEquals(str, branchBaseOut[i]);
			}
			//Check that the bus output data hasn't changed. 
			String[] busBaseOut = {"1	1.0427668646556403	0.202458630313752	0.0	0.0	0.0	0.0", "2	1.0175322347647784	-2.0621043616258965	0.0	0.0	2.85	1.2801", "3	1.0279569538736146	-1.4051242266590986	0.0	0.0	65.3	9.0612", "4	1.0140887267616712	-2.1409524077700284	0.0	0.0	2.57	1.2447", "5	1.0148506165848994	-2.1348215487548754	0.0	0.0	0.0	0.0", "6	0.9750124812841793	-5.490346702399187	0.0	0.0	0.0	0.0", "7	1.0005662553171843	-3.5041441247207583	0.0	0.0	7.09	3.8263", "8	0.9591266393534287	-5.8295523198864565	0.0	0.0	25.6	11.9915", "9	1.0250842916618883	-0.941631872379373	0.0	0.0	6.52	3.1859", "10	1.0141487238530391	-2.152370658846765	0.0	0.0	2.47	1.2317", "11	1.0184959091802916	-2.303927007860102	0.0	0.0	27.5	13.1169", "12	1.0225506272193576	-1.955555344450829	0.0	0.0	0.0	0.0", "13	1.018683156636038	-2.284789557972747	0.0	0.0	4.03	2.0487", "14	1.0022397129520626	-3.4712322278813694	0.0	0.0	0.0	0.0", "15	1.0004839547345485	-3.7414720381052304	0.0	0.0	6.07	2.9561", "16	1.016949737037724	-2.071535147736141	0.0	0.0	0.0	0.0", "17	1.015709039394145	-2.0915390045533004	0.0	0.0	0.0	0.0", "18	1.010768659694761	-2.2480278059122187	0.0	0.0	3.09	1.504", "19	1.019321222824595	-2.2208389886407085	0.0	0.0	15.0	7.4447", "20	1.0314262828710168	-1.5468497248543704	0.0	0.0	0.0	0.0", "21	1.0109635288110257	-3.320387251275851	0.0	0.0	0.0	0.0", "22	1.0017227974429035	-2.928847999424379	0.0	0.0	9.92	4.9125", "23	0.9742869183994061	-5.447723113181647	0.0	0.0	0.0	0.0", "24	1.0470835474971156	0.0	392.9989442883181	341.61414425016085	1900.0	234.0678", "25	1.0150894779867858	-2.2901660515753433	0.0	0.0	24.3	11.8812", "26	1.0131931162855303	-2.1598998741912774	0.0	0.0	0.0	0.0", "27	1.0198819811988222	-2.164937260283591	0.0	0.0	0.0	0.0", "28	1.0025617220443173	-3.6944183331600167	0.0	0.0	3.45	1.6367", "29	0.9806558873171056	-5.33314290873868	0.0	0.0	8.91	4.4553", "30	1.0465749711668877	0.6851677460175745	572.1141969326643	254.859189237141	0.0	0.0", "31	0.9750124263061519	-5.490343656182953	0.0	0.0	2.57	1.2847", "32	1.0102188797970064	-3.1798538524915485	0.0	0.0	24.5	10.4451", "33	0.9986682063527001	-3.5188870100572704	0.0	0.0	12.5	7.0727", "34	1.0101585764836976	-2.2583359081631316	0.0	0.0	7.95	3.1416", "35	1.0155872277282678	-2.1034729232039435	0.0	0.0	13.0	6.3335", "36	1.0281203286968292	-2.372392814567118	0.0	0.0	11.7	5.657", "37	1.0247263978197825	-0.9491748401262335	0.0	0.0	5.57	2.7184", "38	1.0003580815933786	-3.5084255635124477	0.0	0.0	5.74	2.9114", "39	1.0292196821839363	-0.856559090596288	0.0	0.0	8.05	3.7108", "40	1.0144564159095966	-2.1708842270507582	0.0	0.0	3.9	1.9357", "41	1.0470088998913136	0.4602978709245258	0.0	0.0	1.65	0.919", "42	1.0132231625092423	-2.196555376046719	0.0	0.0	0.0	0.0", "43	1.0143480611001823	-2.3132005744303368	0.0	0.0	4.38	2.0979", "44	1.0146780537052538	-3.2337227368855808	0.0	0.0	4.56	1.4989", "45	0.9926852934097634	-5.081915136272162	0.0	0.0	0.0	0.0", "46	1.0217252669115617	-1.6253249495634243	0.0	0.0	0.0	0.0", "47	1.0041609335175152	-3.6638048566483015	0.0	0.0	0.817	0.3945", "48	1.0397427985367855	-0.19768757413674617	0.0	0.0	0.0	0.0", "49	1.0199056679148326	-2.2396845435080484	0.0	0.0	42.9	20.848", "50	1.0092864227850877	-3.2695738636364475	0.0	0.0	16.2	8.1006", "51	0.9756575857222924	-5.451113088371486	0.0	0.0	3.61	1.8034", "52	1.0003324760467558	-3.577233172934755	0.0	0.0	5.05	1.66", "53	1.0007994342483146	-3.722788536029164	0.0	0.0	0.762	0.3243", "54	1.0150238654311765	-2.1543922993283733	0.0	0.0	0.0	0.0", "55	1.007876712663856	-3.400090100549882	0.0	0.0	8.07	2.6518", "56	1.0138769449332654	-2.1454104848536746	0.0	0.0	0.0	0.0", "57	1.0131916471361944	-2.159821546163444	0.0	0.0	2.59	1.2849", "58	1.010694556137263	-2.7400552489605086	0.0	0.0	0.0	0.0", "59	0.9866318960022763	-5.209140182823183	0.0	0.0	0.0	0.0", "60	1.0163752828532424	-2.0824473895367634	0.0	0.0	1.14	0.7864", "61	1.018706588619005	-2.2825474456894295	0.0	0.0	12.9	6.1425", "62	0.9762411925372378	-5.406950779468498	0.0	0.0	4.77	2.4446", "63	1.0099579980591622	-2.26274380342601	0.0	0.0	8.77	4.4273", "64	1.0131984791524145	-3.25003257743904	0.0	0.0	0.0	0.0", "65	1.0052794619747119	-3.639218668868015	0.0	0.0	2.27	1.1003", "66	1.035104583082	-0.6324183754472611	0.0	0.0	0.0	0.0", "67	1.0317342682625306	-0.7992135764115487	0.0	0.0	3.42	1.6786", "68	1.0155160914935768	-2.247706257292519	0.0	0.0	25.9	12.529", "69	1.0017045735213128	-3.5032187153696763	0.0	0.0	0.0	0.0", "70	1.0040415897202468	-3.4721743008070702	0.0	0.0	8.56	4.0362", "71	0.9832739769711522	-5.285264462274612	0.0	0.0	6.28	3.1265", "72	1.0058417299529938	-3.322216243999598	0.0	0.0	4.29	2.1297", "73	1.0317577510330693	-0.7999944688050417	0.0	0.0	0.848	0.4273", "74	1.002693065970735	-3.4478720197901214	0.0	0.0	0.0	0.0", "75	1.0000411400898985	-3.750999294210664	0.0	0.0	27.9	13.8427", "76	0.9960990261861812	-3.048556362725481	0.0	0.0	0.0	0.0", "77	1.0145738143429386	-2.1597268792131854	0.0	0.0	0.743	0.5184", "78	1.0047002378731988	-3.716949944797853	0.0	0.0	120.0	57.0821", "79	0.9943924523000315	-5.035470121905695	0.0	0.0	0.0	0.0", "80	0.9924329246657199	-3.9199732434467554	0.0	0.0	10.1	4.8509", "81	0.9905259329753233	-3.6675875937639106	0.0	0.0	0.0	0.0", "82	1.014313949781507	-3.2427138543892577	0.0	0.0	4.38	1.8644", "83	0.9976690223345195	-3.7975705235257897	0.0	0.0	6.49	3.2301", "84	1.0222541019473124	-1.9852082541814127	0.0	0.0	0.0	0.0", "85	1.015905873530394	-2.140967319759012	0.0	0.0	2.46	1.2233", "86	1.011562974157431	-2.2311744116357137	0.0	0.0	2.48	1.26", "87	1.0084279314720044	-3.38411838955544	0.0	0.0	0.0	0.0", "88	0.9907300777685931	-3.6632253857780888	0.0	0.0	16.0	7.7156", "89	1.015702515656055	-2.0901938465368364	0.0	0.0	2.19	1.3601", "90	1.0103597917523344	-3.16746546806798	0.0	0.0	26.4	12.7903", "91	0.9995284307189569	-3.4600840020844026	0.0	0.0	10.2	4.8354", "92	1.0497316689091498	1.7178471295306368	1943.7128415529776	211.47499359099672	0.0	0.0", "93	1.0021859544459244	-3.482562969430192	0.0	0.0	11.0	5.4261", "94	1.003952840865782	-3.6683782099820377	0.0	0.0	0.765	0.368", "95	0.9954629674809009	-3.6971827291562382	0.0	0.0	16.3	5.3686", "96	1.0318901454223282	-0.7972826474351167	0.0	0.0	0.862	0.3802", "97	1.0016913530057672	-3.702850558016414	0.0	0.0	3.8	2.5546", "98	0.9955400426181368	-3.850855795049957	0.0	0.0	2.58	1.2455", "99	1.0075914059741666	-3.284608782036414	0.0	0.0	0.0	0.0", "100	1.0010035806704403	-3.556349401274197	0.0	0.0	4.35	2.1667", "101	1.0154144202181634	-2.107136176379279	0.0	0.0	0.827	0.4049", "102	1.0118474034639575	-3.1938264161414334	0.0	0.0	0.0	0.0", "103	1.0002290395228473	-3.7313278817894426	0.0	0.0	3.31	1.877", "104	0.9999998499041194	-3.587584291405311	0.0	0.0	5.05	1.6584", "105	1.0302301723524254	-1.752479860438736	0.0	0.0	247.0	13.1662", "106	1.0091903543706597	-3.2768682391498203	0.0	0.0	12.9	6.987", "107	1.000335165761457	-3.741055061149553	0.0	0.0	3.36	1.6479", "108	0.9994924654910988	-3.459309614244432	0.0	0.0	14.1	6.8998", "109	1.0176358469661855	-2.0600634934877875	0.0	0.0	0.0	0.0", "110	0.9950042968207646	-3.8633031612686275	0.0	0.0	3.3	1.508", "111	1.0017053984554498	-3.503263151712545	0.0	0.0	0.0	0.0", "112	1.0285537746613602	-2.32571255664549	0.0	0.0	88.5	4.4022", "113	1.0171463334211348	-2.1150791691857758	0.0	0.0	0.0	0.0", "114	1.0257558465467402	-0.927470499971504	0.0	0.0	2.51	1.139", "115	1.0136465594150665	-2.9432033247766767	0.0	0.0	0.0	0.0", "116	1.0220937805357602	-1.9459447803663155	0.0	0.0	0.0	0.0", "117	1.0146703057061974	-2.1663802765436415	0.0	0.0	0.0	0.0", "118	1.019353685922958	-2.217604008460052	0.0	0.0	11.3	5.4141", "119	1.016583535035278	-2.079511060380723	0.0	0.0	0.547	0.3161", "120	1.018672556780871	-2.2858524559744606	0.0	0.0	5.02	2.5119", "121	1.0017036760944986	-3.503170403484459	0.0	0.0	0.0	0.0", "122	0.9760701420455313	-5.410511015695649	0.0	0.0	4.7	2.4093", "123	1.0470024477442186	0.5679793462819862	0.0	0.0	175.0	81.3182", "124	1.0445275408890533	0.46729740297980177	0.0	0.0	0.0	0.0", "125	0.9900278209111747	-3.678241396368547	0.0	0.0	3.6	1.79", "126	1.0177269710074899	-2.063913851991996	0.0	0.0	2.32	1.1503", "127	1.0120541186941923	-2.3624058849531946	0.0	0.0	9.34	4.5094", "128	1.0252313792403975	-2.142724368147601	0.0	0.0	105.0	51.8522", "129	1.0017052192603852	-3.5032534778914024	0.0	0.0	0.0	0.0", "130	1.0153053847879012	-2.292336676347726	0.0	0.0	0.0	0.0", "131	0.9742868725889232	-5.447720575551449	0.0	0.0	4.54	2.3267", "132	1.0147142112004992	-2.1276256587817493	0.0	0.0	3.82	1.882", "133	1.003241991679355	-3.680439716488317	0.0	0.0	4.45	2.1716", "134	1.028173184617727	-2.367142877247744	0.0	0.0	13.4	5.3034", "135	1.0073023254455655	-3.373598056103863	0.0	0.0	10.8	5.3041", "136	1.0150895802824382	-2.290166404245225	0.0	0.0	0.0	0.0", "137	1.0162420220113786	-2.100176112786942	0.0	0.0	3.21	1.6021", "138	1.0105196681672477	-2.2532498364309057	0.0	0.0	2.52	1.2511", "139	1.0339557523942184	-0.7528973165407975	0.0	0.0	0.0	0.0", "140	0.9926836838709917	-5.08182743634505	0.0	0.0	0.0	0.0", "141	1.0140833809235537	-2.1780709382854457	0.0	0.0	0.0	0.0", "142	1.003939000734296	-3.4454420899630294	0.0	0.0	9.86	4.866", "143	1.0171754347673885	-2.0681525898873985	0.0	0.0	1.61	0.7789", "144	0.9962196235023028	-3.046003928862946	0.0	0.0	0.0	0.0", "145	0.9876402003041292	-5.186384846352534	0.0	0.0	0.0	0.0", "146	1.0049763153668734	-3.645876955126458	0.0	0.0	0.784	0.3782", "147	1.0100859867910035	-2.260104259821366	0.0	0.0	2.95	1.4533", "148	0.9822106796367233	-5.30968337193437	0.0	0.0	0.0	0.0", "149	1.0211747447392796	-1.6915923439764329	0.0	0.0	0.0	0.0", "150	1.0033168819459766	-3.376429675790674	0.0	0.0	3.25	1.5715", "151	1.0252550130418108	-1.2960357630995536	0.0	0.0	0.0	0.0", "152	1.0007760294387542	-3.731823219028276	0.0	0.0	9.59	4.7993", "153	1.014845145967365	-2.118848154248525	0.0	0.0	5.38	2.5919", "154	1.0162295956847214	-2.0890520392132337	0.0	0.0	2.47	1.3259", "155	1.004770864772843	-3.3451303103768653	0.0	0.0	4.67	2.2997", "156	1.0060015833316112	-2.835474446291662	0.0	0.0	4.79	2.44", "157	1.006233892623041	-3.618469526219167	0.0	0.0	0.0	0.0", "158	1.0159864711723894	-2.137291531824135	0.0	0.0	0.0	0.0", "159	0.9947914491733929	-3.0762114554011806	0.0	0.0	0.0	0.0", "160	1.0157090376995361	-2.0915389145187198	0.0	0.0	10.3	6.064", "161	1.0006677082017903	-3.7255847305011063	0.0	0.0	2.35	1.1626", "162	0.9947175874974765	-3.077776372453301	0.0	0.0	4.36	2.1807", "163	1.0490467439493465	1.161902880227998	564.2622414032203	172.39768578141027	0.0	0.0", "164	0.9944415785084146	-3.864629797375267	0.0	0.0	14.5	7.2534", "165	1.0168128762678783	-3.1746675905989834	0.0	0.0	0.0	0.0", "166	1.0110691800687124	-3.1188640176865285	0.0	0.0	0.0	0.0", "167	1.0134838227646106	-2.153668683978873	0.0	0.0	0.0	0.0", "168	1.0240717100227668	-1.402224528203187	0.0	0.0	0.0	0.0", "169	1.0107603777843956	-3.223995525330227	0.0	0.0	24.6	12.2973", "170	1.01521165906293	-2.1521288834867978	0.0	0.0	3.88	1.9389", "171	1.0473479113778086	0.9096060186765248	0.0	0.0	0.0	0.0", "172	1.0296564896813656	-0.8467600031833314	0.0	0.0	1.56	0.7669", "173	1.0021626867818496	-3.7054786034393805	0.0	0.0	2.4	1.1777", "174	1.009513192875858	-3.184892717772294	0.0	0.0	112.0	56.5455", "175	1.0027227115381883	-3.3892864663307525	0.0	0.0	0.0	0.0", "176	1.0154144430280272	-2.1071373900185053	0.0	0.0	0.0	0.0", "177	1.015531242349914	-2.2161105621097836	0.0	0.0	0.0	0.0", "178	1.0096106207159066	-2.8068445262732777	0.0	0.0	29.4	14.0764", "179	0.9999999831521093	-3.5875914885583198	0.0	0.0	0.0	0.0", "180	1.045425635621036	0.6609876762680146	0.0	0.0	123.0	60.2394", "181	0.9747484911803055	-5.4708566469671736	0.0	0.0	7.63	3.7866", "182	0.9696518160229312	-5.544711347657365	0.0	0.0	12.9	6.6253", "183	1.0119160128474596	-2.222442985251821	0.0	0.0	0.0	0.0", "184	1.0092481298532492	-3.273238612900325	0.0	0.0	25.1	12.5267", "185	1.0156933401317922	-2.1429040008296374	0.0	0.0	0.0	0.0", "186	1.0100915281115574	-3.3182157558614342	0.0	0.0	0.0	0.0", "187	0.9758474675672868	-5.472282086353156	0.0	0.0	12.5	5.1147", "188	1.0110691800687124	-3.1188640176865285	0.0	0.0	0.0	0.0", "189	1.0452052830257204	0.4211361110834393	715.771965107291	384.22471678911535	0.0	0.0", "190	1.0166862898468987	-2.124702722270478	0.0	0.0	2.56	1.2609", "191	0.9987682768032443	-2.991985498575482	0.0	0.0	16.0	7.9016", "192	1.0009718033273522	-3.7189751314476784	0.0	0.0	3.41	1.5924", "193	0.9791635211293432	-5.366406835552541	0.0	0.0	17.6	8.8221", "194	1.0171009090987497	-2.0899007706410875	0.0	0.0	27.6	13.7716", "195	1.0118338084683303	-2.581107971377223	0.0	0.0	22.1	11.0388", "196	0.9765837238218514	-5.4310644159329	0.0	0.0	12.6	6.2227", "197	1.0180261670104653	-2.057668382573241	0.0	0.0	0.0	0.0", "198	0.9926831106066859	-5.081796299318689	0.0	0.0	12.2	4.8092", "199	1.0346360054700947	-0.6817980299501213	0.0	0.0	0.0	0.0", "200	1.0119828519174079	-3.1045598505509533	0.0	0.0	35.0	17.5934", "201	0.9911823004318456	-3.6532971488094392	0.0	0.0	5.34	2.2768", "202	1.010404184216204	-2.251817294349881	0.0	0.0	0.0	0.0", "203	0.9991370940877246	-3.467444643699668	0.0	0.0	3.43	1.6948", "204	1.0130175200752751	-2.182581244324115	0.0	0.0	0.0	0.0", "205	1.0165166359632045	-2.0807398315867425	0.0	0.0	4.73	2.4171", "206	1.0452797238141518	0.6398606506266254	0.0	0.0	178.0	87.9117", "207	1.0317344040181355	-0.7992206719218148	0.0	0.0	0.0	0.0", "208	1.0082417928166314	-3.3888072390279502	0.0	0.0	14.1	6.6452"};
			for (int i = 0; i < fixedrList4.size(); i++){
			  String[] arr = fixedrList4.get(i);
			  String str = String.join(",", arr);
			  //System.out.println(str);
			  //System.out.println(busBaseOut[i]);
			  assertEquals(str, busBaseOut[i]);
			}
			//Check that the generator output data hasn't changed. 
			String[] genBaseOut = {"1	143.10531131368188	63.71479730928525", "2	141.37128869521203	14.09833290606645", "3	141.37128869521203	14.09833290606645", "4	392.9989442883181	341.61414425016085", "5	119.99998476606959	14.09833290606645", "6	143.154401094758	76.84494335782307", "7	141.37128869521203	14.09833290606645", "8	143.15438763925837	76.84494335782307", "9	89.99999362981966	14.09833290606645", "10	114.99998763406487	14.09833290606645", "11	141.37129109831844	14.09833290606645", "12	141.37129109831844	14.09833290606645", "13	141.37128916694462	14.09833290606645", "14	139.9998867494065	43.09942144535257", "15	114.99998763406487	14.09833290606645", "16	141.37128869521203	14.09833290606645", "17	141.37128916694462	14.09833290606645", "18	143.15440109475782	76.84494335782307", "19	142.13123395220367	43.09942144535257", "20	141.37128869521203	14.09833290606645", "21	143.10531131368188	63.71479730928525", "22	143.15438763925837	76.84494335782307", "23	142.792837609134	63.71479730928525", "24	89.99999362981966	14.09833290606645", "25	143.15438763925837	76.84494335782307", "26	139.9998867494065	43.09942144535257", "27	143.11073669616653	63.71479730928525", "28	141.37129025255223	14.09833290606645", "29	142.13123395220367	43.09942144535257"};
			for (int i = 0; i < fixedrList6.size(); i++){
			  String[] arr = fixedrList6.get(i);
			  String str = String.join(",", arr);
			  //System.out.println(str);
			  //System.out.println(genBaseOut[i]);
			  assertEquals(str, genBaseOut[i]);
			}
		    
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
