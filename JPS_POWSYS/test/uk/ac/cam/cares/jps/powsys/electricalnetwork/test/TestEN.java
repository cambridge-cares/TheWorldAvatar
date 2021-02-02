package uk.ac.cam.cares.jps.powsys.electricalnetwork.test;

import java.io.File;
import java.io.FileInputStream;
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
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;


public class TestEN extends TestCase {
	
	public static String ELECTRICAL_NETWORK = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	public static String SGELECTRICAL_NETWORK = "http://localhost:8080/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
	String dataPath = QueryBroker.getLocalDataPath();
	String baseUrl=dataPath+"/JPS_POWSYS_EN";
	
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
		   List<String[]>buslist=b.extractOWLinArray(b.readModelGreedy(ELECTRICAL_NETWORK),ELECTRICAL_NETWORK,busInfo,"bus",baseUrl);
//		     List<String[]>buslist=  b.extractOWLinArray(b.readModelGreedy(iriofnetwork),iriofnetwork,genInfocost,"generatorcost",baseUrl);
	      System.out.println(buslist.size());
	      assertEquals(208, buslist.size());
	}
	
		
	public void testgeninfogathering () throws IOException {
		
		
		ENAgent b= new ENAgent ();
		
	String busmapurl=baseUrl+"/mappingforbus.csv";
	OntModel model = ENAgent.readModelGreedy(ELECTRICAL_NETWORK);
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
		jo.put("electricalnetwork", SGELECTRICAL_NETWORK);
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

		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationOPF", jo.toString());
		System.out.println(resultStart);
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
}
