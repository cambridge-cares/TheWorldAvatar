package uk.ac.cam.cares.jps.powsys.electricalnetwork;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONObject;

import com.opencsv.CSVReader;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.PythonHelper;
import uk.ac.cam.cares.jps.powsys.nuclear.IriMapper;
import uk.ac.cam.cares.jps.powsys.nuclear.IriMapper.IriMapping;
import uk.ac.cam.cares.jps.powsys.nuclear.LandlotsKB;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearAgent;

	@WebServlet("/ENAgent")
	public class ENAgent extends HttpServlet{
		private static final long serialVersionUID = -4199209974912271432L;
		OntModel jenaOwlModel2 = null;
		private DatatypeProperty numval = null;
		private OntClass scalarvalueclass = null;
		
		
		public void initOWLClasses(OntModel jenaOwlModel) {

			scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
			
			numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
			
		}
		
		
		protected void doGet(HttpServletRequest request, HttpServletResponse response)
				throws ServletException, IOException {
						
			JSONObject joforEN = AgentCaller.readJsonParameter(request);
			
			String iriofnetwork=joforEN.getString("electricalnetwork");
						
			String genInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
					+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
					+ "SELECT ?entity ?BusNumbervalue ?activepowervalue ?reactivepowervalue ?Qmaxvalue ?Qminvalue ?Vgvalue ?mBasevalue ?Statusvalue ?Pmaxvalue ?Pminvalue ?Pc1value ?Pc2value ?Qc1minvalue ?Qc2minvalue ?Qc1maxvalue ?Qc2maxvalue ?Rampagcvalue ?Ramp10value ?Ramp30value ?Rampqvalue ?apfvalue "
					
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
					
					+ "?model   j5:hasModelVariable ?Qc1max ."
					+ "?Qc1max  a  j3:QC1Max  ."
					+ "?Qc1max  j2:hasValue ?vQc1max ."
					+ "?vQc1max   j2:numericalValue ?Qc1maxvalue ." //qc1max
					
					+ "?model   j5:hasModelVariable ?qc1min ."
					+ "?qc1min  a  j3:QC1Min  ."
					+ "?qc1min  j2:hasValue ?vqc1min ."
					+ "?vqc1min   j2:numericalValue ?Qc1minvalue ." //qc1min
					
					+ "?model   j5:hasModelVariable ?Qc2max ."
					+ "?Qc2max  a  j3:QC2Max  ."
					+ "?Qc2max  j2:hasValue ?vQc2max ."
					+ "?vQc2max   j2:numericalValue ?Qc2maxvalue ." //qc2max
					
					+ "?model   j5:hasModelVariable ?qc2min ."
					+ "?qc2min  a  j3:QC2Min  ."
					+ "?qc2min  j2:hasValue ?vqc2min ."
					+ "?vqc2min   j2:numericalValue ?Qc2minvalue ." //qc2min
					
					+ "?model   j5:hasModelVariable ?ramp10 ."
					+ "?ramp10  a  j3:Ramp10  ."
					+ "?ramp10  j2:hasValue ?vramp10 ."
					+ "?vramp10   j2:numericalValue ?Ramp10value ." //ramp10
					
					+ "?model   j5:hasModelVariable ?rampagc ."
					+ "?rampagc  a  j3:Rampagc  ."
					+ "?rampagc  j2:hasValue ?vrampagc ."
					+ "?vrampagc   j2:numericalValue ?Rampagcvalue ." //rampagc
					
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
			
			String genInfocost= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
					+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
					+ "SELECT ?entity ?formatvalue ?startupcostvalue ?shutdowncostvalue ?gencostnvalue ?gencostn-1value ?gencostn-2value ?gencostcvalue "
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
					
					+ "?model   j5:hasModelVariable ?gencostn-1 ."
					+ "?gencostn-1  a  j3:genCostcn-1  ."
					+ "?gencostn-1  j2:hasValue ?vgencostn-1 ."
					+ "?vgencostn-1   j2:numericalValue ?gencostn-1value ." 
					
					+ "?model   j5:hasModelVariable ?gencostn-2 ."
					+ "?gencostn-2  a  j3:genCostcn-2  ."
					+ "?gencostn-2  j2:hasValue ?vgencostn-2 ."
					+ "?vgencostn-2   j2:numericalValue ?gencostn-2value ." 
					
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
					+ "SELECT ?entity ?BusNumber1value ?BusNumber2value ?resistancevalue ?reactancevalue ?susceptancevalue ?rateAvalue ?rateBvalue ?rateCvalue ?ratiovalue ?statusvalue ?angleminvalue ?anglemaxvalue "
					
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
					
					+ "?model   j5:hasModelVariable ?stat ."
					+ "?stat  a  j3:BranchStatus ."
					+ "?stat  j2:hasValue ?vstat ."
					+ "?vstat   j2:numericalValue ?statusvalue ." //status
					
					+ "?model   j5:hasModelVariable ?angmax ."
					+ "?angmax  a  j3:AngleMax  ."
					+ "?angmax  j2:hasValue ?vangmax ."
					+ "?vangmax   j2:numericalValue ?anglemaxvalue ." //anglemax
					
					+ "?model   j5:hasModelVariable ?angmin ."
					+ "?angmin  a  j3:AngleMin  ."
					+ "?angmin  j2:hasValue ?vangmin ."
					+ "?vangmin   j2:numericalValue ?angleminvalue ." //anglemin
										
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

					+ "?model   j5:hasModelVariable ?Pd ."
					+ "?Pd  a  j3:PdBus  ."
					+ "?Pd  j2:hasValue ?vpd ."
					+ "?vpd   j2:numericalValue ?activepowervalue ."  //pd
					
					+ "?model   j5:hasModelVariable ?Gd ."
					+ "?Gd  a  j3:GdBus  ."
					+ "?Gd  j2:hasValue ?vgd ."
					+ "?vgd   j2:numericalValue ?reactivepowervalue ." //Gd

					+ "?model   j5:hasModelVariable ?type ."
					+ "?type  a  j3:BusType  ."
					+ "?type  j2:hasValue ?vtype ."
					+ "?vtype   j2:numericalValue ?typevalue ." //type
					

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

					+ "?model   j5:hasModelVariable ?vminvar ."
					+ "?vminvar  a  j3:VmMin  ."
					+ "?vminvar  j2:hasValue ?vvminvar ."
					+ "?vvminvar   j2:numericalValue ?VMinvalue ." //Vmin
					
					+ "?model   j5:hasModelVariable ?vmaxvar ."
					+ "?vmaxvar  a  j3:VmMax  ."
					+ "?vmaxvar  j2:hasValue ?vvmaxvar ."
					+ "?vvmaxvar   j2:numericalValue ?VMaxvalue ." //Vmax
													
					+ "}";
			
			ArrayList<String[]>buslist=extractOWLinArray(iriofnetwork,busInfo,13,"bus");
			createNewTSV(buslist, "C:/JPS_DATA/workingdir/JPS_POWSYS/bus.txt","C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENBus.csv");
			
			ArrayList<String[]>gencostlist=extractOWLinArray(iriofnetwork,genInfocost,8,"generatorcost");
			createNewTSV(gencostlist, "C:/JPS_DATA/workingdir/JPS_POWSYS/genCost.txt","C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENGenCost.csv");
			
			ArrayList<String[]>genlist=extractOWLinArray(iriofnetwork,genInfo,22,"generator");
			createNewTSV(genlist, "C:/JPS_DATA/workingdir/JPS_POWSYS/gen.txt","C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENGen.csv");
			
			ArrayList<String[]>branchlist=extractOWLinArray(iriofnetwork,branchInfo,13,"branch");
			createNewTSV(branchlist, "C:/JPS_DATA/workingdir/JPS_POWSYS/branch.txt","C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENBranch.csv");
		
		
		runModel();
		
		try {
			doConversion(iriofnetwork);
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		}
		
		
		
		public ArrayList<String[]> extractOWLinArray(String iriofnetwork, String busInfo, int numberofvar,String context) throws IOException {
			
			
			String electricalnodeInfo= "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "SELECT ?component "
					
					+ "WHERE {?entity  a  j2:CompositeSystem  ." 
					+ "?entity   j2:hasSubsystem ?component ."								
					+ "}";
			
			jenaOwlModel2 = ModelFactory.createOntologyModel();	
			jenaOwlModel2.read(iriofnetwork, null); 
			ResultSet rs_electricalnode = NuclearAgent.queryFromOWLFile(electricalnodeInfo,jenaOwlModel2); 	
			
			ArrayList<String> totalnodeelectricresult= new ArrayList<String>();
			
			while(rs_electricalnode.hasNext()) {
				QuerySolution qs_p = rs_electricalnode.nextSolution();
				Resource iriofnode = qs_p.getResource("component");
			    String stiriofnode = iriofnode.toString();  
			    
			    totalnodeelectricresult.add(stiriofnode);
			}
			
			int numberofiri=totalnodeelectricresult.size();
			   
			ArrayList<String[]> totalnodebusresult= new ArrayList<String[]>();
			IriMapper mapper= new IriMapper();
			int index=1;
			for(int t=0;t<numberofiri;t++) {
				OntModel jenaOwlModel3  = ModelFactory.createOntologyModel();	
				
				jenaOwlModel3.read(totalnodeelectricresult.get(t), null); 
				ResultSet rs_busnode = NuclearAgent.queryFromOWLFile(busInfo,jenaOwlModel3);
				
				while(rs_busnode.hasNext()) {
			    	QuerySolution qs_p = rs_busnode.nextSolution();
			    	
					Iterator<String> varNames = qs_p.varNames();
					
					String [] queryresult= new String[numberofvar];
					int counter=0;
					for (; varNames.hasNext();) {

						String varName = varNames.next();
						if(qs_p.get(varName).isResource()) {
							queryresult[counter] = qs_p.getResource(varName).toString();
							System.out.println("index included= "+index);
							mapper.add(queryresult[counter], ""+index, context);
							index++;
						}
						else {							
					    queryresult[counter] = qs_p.getLiteral(varName).getString();
					    //System.out.println("prop= "+queryresult[counter]);
						}
						counter++;	
					}
				    totalnodebusresult.add(queryresult);
				    
			    }
			}
			int number=totalnodebusresult.size();
			if(context.contains("bus")) {
		    mapper.serialize("C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENBus.csv");
			}
			else if(context.contains("generator")) {
				mapper.serialize("C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENGen.csv");
			}
			else if(context.contains("branch")) {
				mapper.serialize("C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENBranch.csv");
			}
			System.out.println("amount= "+number);

			
			return totalnodebusresult;

		}
		
		public void createNewTSV(ArrayList<String[]>componentlist, String tsvFileout,String mapdir) throws IOException {
			try (BufferedWriter bw = new BufferedWriter(new FileWriter(tsvFileout))) {

				int line=componentlist.size();
		    	IriMapper map2=new IriMapper();
		    	List<IriMapping> original=map2.deserialize(mapdir);
				for(int x=0;x<line;x++) {
					int element=componentlist.get(x).length;
					for (int e=0;e<element;e++) {
						
						if(mapdir.contentEquals("C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENBus.csv")&&e==0) {
							String content=componentlist.get(x)[e];
							String content2=null;
							int a=0;
							while(a<original.size()) {
							if(original.get(a).iri.contentEquals(content)) {
								content2=original.get(a).id+"\t";
							}
								
								a++;	
							}
							bw.write(content2);
						}
						else if(!mapdir.contentEquals("C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENBus.csv")&&e==0) {

						}
						
						else if(e==element-1) {
							String content=componentlist.get(x)[e]+"\n";
							bw.write(content);
						}
						else {
						//String content = "This is the content to write into file\n";
						String content=componentlist.get(x)[e]+"\t";
						bw.write(content);
						}
	
					}
							
				}
							
				// no need to close it.
				//bw.close();

				System.out.println("Done");

			} catch (IOException e) {

				e.printStackTrace();

			}
		}
		
		public void runModel() throws IOException {
			
			String result = PythonHelper.callPython("PyPower-PF-OPF-JA-8.py",null, this);
		}
		
		
		public ArrayList<String[]> readResult(String outputfiledir,int colnum) throws IOException {
			ArrayList<String[]> entryinstance= new ArrayList<String[]> ();
			 CSVReader reader = new CSVReader(new FileReader(outputfiledir), '\t');
		        String[] record;
		        int entry=0;
		        while ((record = reader.readNext()) != null) {
		        	int element=0;
		        	String[]entityline=new String[colnum];
		        	System.out.println ("entry of "+entry);
		            for (String value : record) {
		            	
		                System.out.println("element value of "+element+" is= "+value); // Debug only
		                entityline[element]=value;
		                element++;
		            }
		            entryinstance.add(entityline);
		            entry++;
		        }   
			return entryinstance;
		}
		
		public void doConversion(String iriofnetwork) throws URISyntaxException, IOException {

						
			
			String genoutputInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
					+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
					+ "SELECT ?entity ?vpg ?vqg "
					
					+ "WHERE {?entity  a  j1:PowerGenerator  ." 
					+ "?entity   j2:isModeledBy ?model ."
					+ "?model   j5:hasModelVariable ?Pg ."
					+ "?Pg  a  j3:Pg  ."
					+ "?Pg  j2:hasValue ?vpg ."//pg
					
					+ "?model   j5:hasModelVariable ?Qg ."
					+ "?Qg  a  j3:Qg  ."
					+ "?Qg  j2:hasValue ?vqg ."//qg
										
					+ "}";
			
			String busoutputInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
					+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
					+ "SELECT ?BusNumbervalue ?vpdbus ?vgdbus ?vpdgen ?vgdgen ?vVM ?vVA  "
					
					+ "WHERE {?entity  a  j1:BusNode  ." 
					+ "?entity   j2:isModeledBy ?model ."
					+ "?model   j5:hasModelVariable ?num ."
					+ "?num  a  j3:BusNumber  ."
					+ "?num  j2:hasValue ?vnum ."
					+ "?vnum   j2:numericalValue ?BusNumbervalue ."  //number

					+ "?model   j5:hasModelVariable ?Pd ."
					+ "?Pd  a  j3:PdBus  ."
					+ "?Pd  j2:hasValue ?vpdbus ."  //pd
					
					+ "?model   j5:hasModelVariable ?Gd ."
					+ "?Gd  a  j3:GdBus  ."
					+ "?Gd  j2:hasValue ?vgdbus ." //Gd

					+ "?model   j5:hasModelVariable ?Pdgen ."
					+ "?Pdgen  a  j3:PdGen  ."
					+ "?Pdgen  j2:hasValue ?vpdgen ."  //pdgen
					
					+ "?model   j5:hasModelVariable ?Gdgen ."
					+ "?Gdgen  a  j3:GdGen  ."
					+ "?Gdgen  j2:hasValue ?vgdgen ." //Gd
					
					+ "?model   j5:hasModelVariable ?VM ."
					+ "?VM  a  j3:Vm  ."
					+ "?VM  j2:hasValue ?vVM ."//Vm
					
					+ "?model   j5:hasModelVariable ?VA ."
					+ "?VA  a  j3:Va  ."
					+ "?VA  j2:hasValue ?vVA ."//Va												
					+ "}";
			
			String branchoutputInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
					+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
					+ "SELECT ?entity ?vploss ?vqloss ?vpave ?vqave ?vsave "
					
					+ "WHERE {?entity  a  j1:UndergroundCable  ." 
					+ "?entity   j2:isModeledBy ?model ."
					+ "?model   j5:hasModelVariable ?ploss ."
					+ "?ploss  a  j3:PLoss  ."
					+ "?ploss  j2:hasValue ?vploss ."  //ploss
					
					+ "?model   j5:hasModelVariable ?qloss ."
					+ "?qloss  a  j3:QLoss  ."
					+ "?qloss  j2:hasValue ?vqloss ."  //qloss
					
					+ "?model   j5:hasModelVariable ?pave ."
					+ "?pave  a  j3:PAverage  ."
					+ "?pave  j2:hasValue ?vpave ."  //pave
					
					+ "?model   j5:hasModelVariable ?qave ."
					+ "?qave  a  j3:QAverage  ."
					+ "?qave  j2:hasValue ?vqave ."  //qave
					
					+ "?model   j5:hasModelVariable ?save ."
					+ "?save  a  j3:SAverage  ."
					+ "?save  j2:hasValue ?vsave ."  //save
					
					+ "}";	
			
			ArrayList<String[]>branchoutputlist=extractOWLinArray(iriofnetwork,branchoutputInfo,6,"output");
			ArrayList<String[]>genoutputlist=extractOWLinArray(iriofnetwork,genoutputInfo,3,"output");
			ArrayList<String[]>busoutputlist=extractOWLinArray(iriofnetwork,busoutputInfo,7,"output");
			
			ArrayList<String[]> resultfrommodelgen=readResult("C:/JPS_DATA/workingdir/JPS_POWSYS/outputGenPF.txt", 3);
			ArrayList<String[]> resultfrommodelbus=readResult("C:/JPS_DATA/workingdir/JPS_POWSYS/outputBusPF.txt", 7);
			ArrayList<String[]> resultfrommodelbranch=readResult("C:/JPS_DATA/workingdir/JPS_POWSYS/outputBranchPF.txt", 6);
			
			int amountofgen=genoutputlist.size();
			for (int a=0;a<amountofgen;a++) {
				String filePath= genoutputlist.get(a)[1].replaceAll("http://www.theworldavatar.com/kb", "C:/TOMCAT/webapps/ROOT/kb").split("#")[0]; //update the file locally
				
				FileInputStream inFile = new FileInputStream(filePath);
				Reader in = new InputStreamReader(inFile, "UTF-8");

				OntModel jenaOwlModel2 = ModelFactory.createOntologyModel();
				jenaOwlModel2.read(in, null);

				initOWLClasses(jenaOwlModel2);
			
			
			Individual vpout=jenaOwlModel2.getIndividual(genoutputlist.get(a)[1]);
			vpout.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelgen.get(a)[1]));  //TODO:checking for the mapping position !!!!
			
			Individual vqout=jenaOwlModel2.getIndividual(genoutputlist.get(a)[2]);
			vqout.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelgen.get(a)[2]));  //TODO:checking for the mapping position !!!!
			
			/** save the updated model file*/ 
			LandlotsKB ins2 = new LandlotsKB();
			ins2.savefile(jenaOwlModel2, filePath);
			}
			
			int amountofbus=busoutputlist.size();
			for (int a=0;a<amountofbus;a++) {
				String filePath= busoutputlist.get(a)[1].replaceAll("http://www.theworldavatar.com/kb", "C:/TOMCAT/webapps/ROOT/kb").split("#")[0]; //update the file locally
				
				FileInputStream inFile = new FileInputStream(filePath);
				Reader in = new InputStreamReader(inFile, "UTF-8");

				OntModel jenaOwlModel2 = ModelFactory.createOntologyModel();
				jenaOwlModel2.read(in, null);

				initOWLClasses(jenaOwlModel2);
			
			
			Individual vpdbusout=jenaOwlModel2.getIndividual(busoutputlist.get(a)[1]);
			vpdbusout.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbus.get(a)[5]));  //TODO:checking for the mapping position !!!!
			
			Individual vgdbusout=jenaOwlModel2.getIndividual(busoutputlist.get(a)[2]);
			vgdbusout.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbus.get(a)[6]));  //TODO:checking for the mapping position !!!!
			
			Individual vpdgenout=jenaOwlModel2.getIndividual(busoutputlist.get(a)[3]);
			vpdgenout.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbus.get(a)[3]));  //TODO:checking for the mapping position !!!!
			
			Individual vgdgenout=jenaOwlModel2.getIndividual(busoutputlist.get(a)[4]);
			vgdgenout.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbus.get(a)[4]));  //TODO:checking for the mapping position !!!!
			
			Individual vVmout=jenaOwlModel2.getIndividual(busoutputlist.get(a)[5]);
			vVmout.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbus.get(a)[1]));  //TODO:checking for the mapping position !!!!
			
			Individual vVaout=jenaOwlModel2.getIndividual(busoutputlist.get(a)[6]);
			vVaout.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbus.get(a)[2]));  //TODO:checking for the mapping position !!!!
			
			/** save the updated model file*/ 
			LandlotsKB ins2 = new LandlotsKB();
			ins2.savefile(jenaOwlModel2, filePath);
			}
			

			int amountofbranch=branchoutputlist.size();
			for (int a=0;a<amountofbranch;a++) {
				String filePath= branchoutputlist.get(a)[1].replaceAll("http://www.theworldavatar.com/kb", "C:/TOMCAT/webapps/ROOT/kb").split("#")[0]; //update the file locally
				
				FileInputStream inFile = new FileInputStream(filePath);
				Reader in = new InputStreamReader(inFile, "UTF-8");

				OntModel jenaOwlModel2 = ModelFactory.createOntologyModel();
				jenaOwlModel2.read(in, null);

				initOWLClasses(jenaOwlModel2);
			
			
			Individual vploss=jenaOwlModel2.getIndividual(branchoutputlist.get(a)[1]);
			vploss.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbranch.get(a)[1]));  //TODO:checking for the mapping position !!!!
			
			Individual vqloss=jenaOwlModel2.getIndividual(branchoutputlist.get(a)[2]);
			vqloss.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbranch.get(a)[2]));  //TODO:checking for the mapping position !!!!
			
			Individual vpave=jenaOwlModel2.getIndividual(branchoutputlist.get(a)[3]);
			vpave.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbranch.get(a)[3]));  //TODO:checking for the mapping position !!!!
			
			Individual vqave=jenaOwlModel2.getIndividual(branchoutputlist.get(a)[4]);
			vqave.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbranch.get(a)[4]));  //TODO:checking for the mapping position !!!!
			
			Individual vsave=jenaOwlModel2.getIndividual(branchoutputlist.get(a)[5]);
			vsave.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(resultfrommodelbranch.get(a)[5]));  //TODO:checking for the mapping position !!!!
			
			
			/** save the updated model file*/ 
			LandlotsKB ins2 = new LandlotsKB();
			ins2.savefile(jenaOwlModel2, filePath);
			}
		}
		
		
	}
