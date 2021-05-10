package uk.ac.cam.cares.jps.powsys.electricalnetwork;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.powsys.nuclear.IriMapper;
import uk.ac.cam.cares.jps.powsys.nuclear.IriMapper.IriMapping;
import uk.ac.cam.cares.jps.powsys.util.Util;

@WebServlet(urlPatterns = { "/ENAgent/startsimulationPF", "/ENAgent/startsimulationOPF" })
public class ENAgent extends JPSAgent{
	//currently on OPF is running
	
	private static final long serialVersionUID = -4199209974912271432L;
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(ENAgent.class);
    }
    Logger logger = LoggerFactory.getLogger(ENAgent.class);

	public DatatypeProperty getNumericalValueProperty(OntModel jenaOwlModel) {
		return jenaOwlModel.getDatatypeProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		requestParams = processRequestParameters(requestParams, null);
		return requestParams;
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		if (!validateInput(requestParams)) {
			throw new JSONException("");
		}
		String iriofnetwork = requestParams.getString("electricalnetwork");
		String modeltype = "OPF";
		String baseUrl = QueryBroker.getLocalDataPath() + "/JPS_POWSYS_EN";
		JSONObject result;
		try {
			result = startSimulation(iriofnetwork, baseUrl, modeltype);
			return result;
		} catch (IOException e) {
			throw new JPSRuntimeException("");
			
		}
	}
	/** main function for ENAgent
	 * 
	 * @param iriofnetwork
	 * @param baseUrl
	 * @param modeltype
	 * @return
	 * @throws IOException
	 */
	public JSONObject startSimulation(String iriofnetwork, String baseUrl, String modeltype) throws IOException {
		
		JSONObject resjo=new JSONObject();
		resjo.put("electricalnetwork", iriofnetwork);
		
		logger.info("starting simulation for electrical network = " + iriofnetwork + ", modeltype = " + modeltype + ", local data path=" + baseUrl);
		
		OntModel model = Util.readModelGreedy(iriofnetwork);	
		
		List<String[]> buslist = generateInput(model, iriofnetwork, baseUrl, modeltype);
		
		logger.info("running PyPower simulation");
		try {
			String[] fileNames = {"/baseMVA.txt", "/bus.txt", "/gen.txt", "/branch.txt", "/outputBusOPF.txt", "/outputBranchOPF.txt", "/outputGenOPF.txt", "/areas.txt", "/genCost.txt", "/outputStatus.txt"};
			runPythonScript("PyPower-PF-OPF-JA-9-Java-2.py", baseUrl, fileNames);
		} catch (Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		String fileName = baseUrl+"/outputStatus.txt";
		Path path = Paths.get(fileName);
		BufferedReader input = new BufferedReader(new FileReader(fileName));
	    String last, line;
	    last = "";
	    while ((line = input.readLine()) != null) { 
	        last = line;
	    }
		if (last.contains("Converged")) {
			resjo.put("status", "converged");
			try {
				logger.info("converting PyPower results to OWL files");
				doConversion(model, iriofnetwork, baseUrl, modeltype, buslist);
			} catch (URISyntaxException e) {
				logger.error(e.getMessage(), e);
				throw new JPSRuntimeException(e.getMessage(), e);
			}
			
		}else {
			resjo.put("status", "Diverged");
			//return null;
		}
		

		
		
		logger.info("finished simulation for electrical network = " + iriofnetwork + ", modeltype = " + modeltype + ", local data path=" + baseUrl);
		return resjo;
	}

	public List<String[]> generateInput(OntModel model, String iriofnetwork, String baseUrl, String modeltype) throws IOException {
		
		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
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
				+ "?vnum   j2:numericalValue ?BusNumbervalue ." // number

				+ "?model   j5:hasModelVariable ?Pg ." 
				+ "?Pg  a  j3:Pg  ." 
				+ "?Pg  j2:hasValue ?vpg ."
				+ "?vpg   j2:numericalValue ?activepowervalue ." // pg

				+ "?model   j5:hasModelVariable ?Qg ." 
				+ "?Qg  a  j3:Qg  ." 
				+ "?Qg  j2:hasValue ?vqg ."
				+ "?vqg   j2:numericalValue ?reactivepowervalue ." // qg

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

				+ "?model   j5:hasModelVariable ?stat ." 
				+ "?stat  a  j3:Status ." 
				+ "?stat  j2:hasValue ?vstat ."
				+ "?vstat   j2:numericalValue ?Statusvalue ." // status

				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ." 
				+ "?pmax  j2:hasValue ?vpmax ."
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax

				+ "?model   j5:hasModelVariable ?pmin ." 
				+ "?pmin  a  j3:PMin  ." 
				+ "?pmin  j2:hasValue ?vpmin ."
				+ "?vpmin   j2:numericalValue ?Pminvalue ." // pmin

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

				+ "}";
		String batteryInfo ="PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
				
				+ "SELECT DISTINCT ?entity ?V_ActivePowerInjection_of_VRB "
				+ "WHERE {?entity  a  j1:VanadiumRedoxBattery ."

				+ "?entity   j9:hasActivePowerInjection ?cap ."
				+"?cap j2:hasValue ?vcap ."
				+"?vcap  j2:numericalValue ?V_ActivePowerInjection_of_VRB ."
				+ " {?class rdfs:subClassOf j1:Battery ."
				+ "} "
				+ "UNION { ?class rdfs:subClassOf j1:EnergyStorageSystem . } ."
				+ "}";
		String genInfocost = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
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

		String branchInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
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
				+ "?vnum1   j2:numericalValue ?BusNumber1value ." // number 1

				+ "?model   j5:hasModelVariable ?num2 ." 
				+ "?num2  a  j3:BusTo  ." 
				+ "?num2  j2:hasValue ?vnum2 ."
				+ "?vnum2   j2:numericalValue ?BusNumber2value ." // number 2

				+ "?model   j5:hasModelVariable ?res ." 
				+ "?res  a  j3:R  ." 
				+ "?res  j2:hasValue ?vres ."
				+ "?vres   j2:numericalValue ?resistancevalue ." // resistance

				+ "?model   j5:hasModelVariable ?rea ." 
				+ "?rea  a  j3:X  ." 
				+ "?rea  j2:hasValue ?vrea ."
				+ "?vrea   j2:numericalValue ?reactancevalue ." // reactance

				+ "?model   j5:hasModelVariable ?sus ." 
				+ "?sus  a  j3:B  ." 
				+ "?sus  j2:hasValue ?vsus ."
				+ "?vsus   j2:numericalValue ?susceptancevalue ." // susceptance

				+ "?model   j5:hasModelVariable ?ratea ." 
				+ "?ratea  a  j3:RateA  ." 
				+ "?ratea  j2:hasValue ?vratea ."
				+ "?vratea   j2:numericalValue ?rateAvalue ." // rateA

				+ "?model   j5:hasModelVariable ?rateb ." 
				+ "?rateb  a  j3:RateB  ." 
				+ "?rateb  j2:hasValue ?vrateb ."
				+ "?vrateb   j2:numericalValue ?rateBvalue ." // rateB

				+ "?model   j5:hasModelVariable ?ratec ." 
				+ "?ratec  a  j3:RateC  ." 
				+ "?ratec  j2:hasValue ?vratec ."
				+ "?vratec   j2:numericalValue ?rateCvalue ." // rateC

				+ "?model   j5:hasModelVariable ?ratio ." 
				+ "?ratio  a  j3:RatioCoefficient  ."
				+ "?ratio  j2:hasValue ?vratio ." 
				+ "?vratio   j2:numericalValue ?ratiovalue ." // ratio

				+ "?model   j5:hasModelVariable ?ang ." 
				+ "?ang  a  j3:Angle  ." 
				+ "?ang  j2:hasValue ?vang ."
				+ "?vang   j2:numericalValue ?anglevalue ." // angle

				+ "?model   j5:hasModelVariable ?stat ." 
				+ "?stat  a  j3:BranchStatus ." 
				+ "?stat  j2:hasValue ?vstat ."
				+ "?vstat   j2:numericalValue ?statusvalue ." // status

				+ "?model   j5:hasModelVariable ?angmin ." 
				+ "?angmin  a  j3:AngleMin  ."
				+ "?angmin  j2:hasValue ?vangmin ." 
				+ "?vangmin   j2:numericalValue ?angleminvalue ." // anglemin

				+ "?model   j5:hasModelVariable ?angmax ." 
				+ "?angmax  a  j3:AngleMax  ."
				+ "?angmax  j2:hasValue ?vangmax ." 
				+ "?vangmax   j2:numericalValue ?anglemaxvalue ." // anglemax

				+ "}";

		String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
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
				+ "?vnum   j2:numericalValue ?BusNumbervalue ." // number

				+ "?model   j5:hasModelVariable ?type ." 
				+ "?type  a  j3:BusType  ." 
				+ "?type  j2:hasValue ?vtype ."
				+ "?vtype   j2:numericalValue ?typevalue ." // type

				+ "?model   j5:hasModelVariable ?Pd ." 
				+ "?Pd  a  j3:PdBus  ." 
				+ "?Pd  j2:hasValue ?vpd ."
				+ "?vpd   j2:numericalValue ?activepowervalue ." // pd

				+ "?model   j5:hasModelVariable ?Gd ." 
				+ "?Gd  a  j3:GdBus  ." 
				+ "?Gd  j2:hasValue ?vgd ."
				+ "?vgd   j2:numericalValue ?reactivepowervalue ." // Gd

				+ "?model   j5:hasModelVariable ?Gsvar ." 
				+ "?Gsvar  a  j3:Gs  ." 
				+ "?Gsvar  j2:hasValue ?vGsvar ."
				+ "?vGsvar   j2:numericalValue ?Gsvalue ." // Gs

				+ "?model   j5:hasModelVariable ?Bsvar ." 
				+ "?Bsvar  a  j3:Bs  ." 
				+ "?Bsvar  j2:hasValue ?vBsvar ."
				+ "?vBsvar   j2:numericalValue ?Bsvalue ." // Bs

				+ "?model   j5:hasModelVariable ?areavar ." 
				+ "?areavar  a  j3:Area  ."
				+ "?areavar  j2:hasValue ?vareavar ." 
				+ "?vareavar   j2:numericalValue ?areavalue ." // area

				+ "?model   j5:hasModelVariable ?VM ." 
				+ "?VM  a  j3:Vm  ." 
				+ "?VM  j2:hasValue ?vVM ."
				+ "?vVM   j2:numericalValue ?VoltMagvalue ." // Vm

				+ "?model   j5:hasModelVariable ?VA ." 
				+ "?VA  a  j3:Va  ." 
				+ "?VA  j2:hasValue ?vVA ."
				+ "?vVA   j2:numericalValue ?VoltAnglevalue ." // Va

				+ "?model   j5:hasModelVariable ?BKV ." 
				+ "?BKV  a  j3:baseKV  ." 
				+ "?BKV  j2:hasValue ?vBKV ."
				+ "?vBKV   j2:numericalValue ?BaseKVvalue ." // Base KV

				+ "?model   j5:hasModelVariable ?zvar ." 
				+ "?zvar  a  j3:Zone  ." 
				+ "?zvar  j2:hasValue ?vzvar ."
				+ "?vzvar   j2:numericalValue ?Zonevalue ." // Zone

				+ "?model   j5:hasModelVariable ?vmaxvar ." 
				+ "?vmaxvar  a  j3:VmMax  ."
				+ "?vmaxvar  j2:hasValue ?vvmaxvar ." 
				+ "?vvmaxvar   j2:numericalValue ?VMaxvalue ." // Vmax

				+ "?model   j5:hasModelVariable ?vminvar ." 
				+ "?vminvar  a  j3:VmMin  ."
				+ "?vminvar  j2:hasValue ?vvminvar ." 
				+ "?vvminvar   j2:numericalValue ?VMinvalue ." // Vmin

				+ "}";
		
		QueryBroker broker = new QueryBroker();

		List<String[]> buslist = extractOWLinArray(model, iriofnetwork, busInfo, "bus", baseUrl);
		String content = createNewTSV(buslist, baseUrl + "/mappingforbus.csv", baseUrl + "/mappingforbus.csv");
		broker.putLocal(baseUrl + "/bus.txt", content);

		List<String[]> genlist = extractOWLinArray(model, iriofnetwork, genInfo, "generator", baseUrl);
		content = createNewTSV(genlist, baseUrl + "/mappingforgenerator.csv", baseUrl + "/mappingforbus.csv");
		//only add if battery is available. 
		
		List<String[]> batterylist = extractOWLinArray(model, iriofnetwork, batteryInfo, "battery", baseUrl);
		if (!batterylist.isEmpty()) {
			content += createDummyValueTSV(batterylist); 
			
		}
		broker.putLocal(baseUrl + "/gen.txt", content);
		
		List<String[]> gencostlist = extractOWLinArray(model, iriofnetwork, genInfocost, "generatorcost", baseUrl);
		content = createNewTSV(gencostlist, baseUrl + "/mappingforgeneratorcost.csv", baseUrl + "/mappingforbus.csv");
		
		if (!batterylist.isEmpty()) {
			String[] dummyarray = new String[7];
			Arrays.fill(dummyarray, "0");
			for (int i = 0; i < batterylist.size(); i++) {
				String message = String.join("\t", dummyarray)+"\n";
				content += message;
			}
			
		}
		broker.putLocal(baseUrl + "/genCost.txt", content);

		List<String[]> branchlist = extractOWLinArray(model, iriofnetwork, branchInfo, "branch", baseUrl);
		content = createNewTSV(branchlist, baseUrl + "/mappingforbranch.csv", baseUrl + "/mappingforbus.csv");
		broker.putLocal(baseUrl + "/branch.txt", content);

		String resourceDir = Util.getResourceDir(this);
		File file = new File(resourceDir + "/baseMVA.txt");
		broker.putLocal(baseUrl + "/baseMVA.txt", file);
		
		return buslist;
	}

	
	public List<String[]> extractOWLinArray(OntModel model, String iriofnetwork, String busInfo, String context, String baseUrl)
			throws IOException {

		ResultSet resultSet = JenaHelper.query(model, busInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		
		String keysConcat = MiscUtil.concat(keys, ", ");
		StringBuffer b = new StringBuffer("context = ").append(context)
				.append(", keys = ").append(keys.length).append(", ").append(keysConcat);
		logger.info(b.toString());
		
//		if ("generator".equals(context)) {
//			for (String[] current : resultList) {
//				System.out.println(MiscUtil.concat(current, ", "));
//			}
//		}
		
		if (!context.toLowerCase().contains("output")) {
			/*
			 * special case 1. for bus, the mapper contains bus number instead of iri case
			 * 2. for gencost no need mapper as it just read from the gen mapper
			 */
			IriMapper mapper = new IriMapper();
			for (int i = 0; i < resultList.size(); i++) {
				String[] current = resultList.get(i);
				String id = "" + (i + 1);
				mapper.add(current[0], id, context);
				// current[0]=id;// ??? no need to be there because it is not written in the tsv
			}

			String csv = mapper.serialize();
			QueryBroker broker = new QueryBroker();
			broker.putLocal(baseUrl + "/mappingfor" + context + ".csv", csv);
		}

		return resultList;
	}
	public String createDummyValueTSV(List<String[]> activePower) throws IOException {
		String content = "";
		for (int i = 0; i < activePower.size(); i++) {
			String[] dummyarray = new String[21];
			Arrays.fill(dummyarray, "0");
			dummyarray[3] = activePower.get(i)[1].toString();
			String message = String.join("\t", dummyarray)+"\n";
			content += message;
	    }
		
		
		return content;
	}
	public String createNewTSV(List<String[]> componentlist, String mapdir, String mapdirbus) throws IOException {
		StringWriter writer = new StringWriter();
//			try (BufferedWriter bw = new BufferedWriter(new FileWriter(tsvFileout))) {
		try (BufferedWriter bw = new BufferedWriter(writer)) {
			int line = componentlist.size();
			IriMapper map2 = new IriMapper();
			List<IriMapping> original = map2.deserialize2(mapdir);
			List<IriMapping> originalforbus = map2.deserialize2(mapdirbus);// because the gen and branch input also
																			// depends on the bus number
																			// =baseUrl+"/mappingforbus.csv"
			for (int x = 0; x < line; x++) {
				int element = componentlist.get(x).length;
				for (int e = 0; e < element; e++) {

					if (mapdir.contains("bus") && e == 0) { // first condition is when the map is bus, it takes the 1st
															// element of busnumber and use the mapped id to be written
															// in tsv
						String content = componentlist.get(x)[e];
						String content2 =map2.getIDFromMap(original, content)+"\t";
						bw.write(content2);
					} else if (!mapdir.contains("bus") && e == 0) {
						// condition when the map is not bus, it ignores the 1st element ( entities real iri is not to be
						// written in tsv)
					}

					else if (e == element - 1) {// condition for every type in the last property, it will add the \n to
												// move to new row of the tsv
						String content = componentlist.get(x)[e] + "\n";
						bw.write(content);
					} else {// the rest element index (not the first and not the last

						if (e == 1 && mapdir.contains("mappingforgenerator.csv")&& !mapdir.contains("cost.csv")) { // condition that original bus
																					// number extracted from gen owl
																					// file need to be mapped to the
																					// mapped bus number

							String ori = componentlist.get(x)[e].replace(".", "x").split("x")[0]; // remove the decimal
																									// values queried
																									// from the kb in
																									// gen in the form
																									// of string
							String content2 =map2.getIDFromMap(originalforbus, ori)+"\t";
							bw.write(content2);
						}

						else if ((mapdir.contains("branch") && e == 1) || (mapdir.contains("branch") && e == 2)) {// condition that original 2 bus numbers
																												//	 extracted from branch owl file need to 
																												//	 be mapped to the mapped bus number

							String content = componentlist.get(x)[e];
							String content2 =map2.getIDFromMap(originalforbus, content)+"\t";
							bw.write(content2);

						} else if (mapdir.contains("bus") && e == 7) {
							double pu = Double.valueOf(componentlist.get(x)[e]);

							if (pu > 1.20000) {
								String basekv = componentlist.get(x)[9];
								pu = Double.valueOf(componentlist.get(x)[e]) / Double.valueOf(basekv);
							}
							String content = pu + "\t";
							bw.write(content);

						} else { // the rest normal condition
							String content = componentlist.get(x)[e] + "\t";
							bw.write(content);
						}
					}
				}
			}

			System.out.println("Done");

		} catch (IOException e) {

			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
		}

		return writer.toString();

	}
	 /** run Python Script in folder
		 * 
		 * @param script
		 * @param folder
		 * @return
		 * @throws Exception
		 */
	public String runPythonScript(String script, String folder, String[] fileNames) throws Exception {
			String result = "";
				String path = AgentLocator.getCurrentJpsAppDirectory(this);
				String command = "python " + path+ "/python/model/" +script 
						+ " " +folder +fileNames[0] + " " +folder 
						+fileNames[1] + " " +folder +fileNames[2] + " " 
						+folder +fileNames[3] +" 1" + " " +folder 
						+fileNames[4] + " " +folder 
						+fileNames[5] + " " +folder 
						+fileNames[6] + " 1" + " 1" 
						+ " " +folder +fileNames[7] + " " +folder 
						+fileNames[8] + " " +folder +fileNames[9];
				System.out.println(command);
				result = CommandHelper.executeSingleCommand( path, command);
			
			
				return result;
		}
	/** reads the result from the csv file produced and returns as List<String[]>
	 * 
	 * @param baseUrl String
	 * @param filename name of the file. 
	 * @return
	 * @throws IOException
	 */
	public ArrayList<String[]> readResult(String outputFile) throws IOException {
        String csv = new QueryBroker().readFileLocal(outputFile);
        ArrayList<String[]> simulationResult = new ArrayList<String[]> (fromCsvToArray(csv));
		
		return simulationResult;
	}
	/** cannot use MatrixConverter as they're separated by \t
	 * 
	 * @param s
	 * @return
	 */
	public List<String[]> fromCsvToArray(String s) {
		
		List<String[]> result = new ArrayList<String[]>();
		
		StringTokenizer tokenizer = new StringTokenizer(s, "\n");
		
		while (tokenizer.hasMoreTokens()) {
			String[] row = tokenizer.nextToken().split("\t");
			result.add(row);
		}
		
		return result;
	}
	/** Start loading output from csv and store in KG
	 * 
	 * @param model
	 * @param iriofnetwork
	 * @param baseUrl
	 * @param modeltype
	 * @param buslist
	 * @throws URISyntaxException
	 * @throws IOException
	 */
	public void doConversion(OntModel model, String iriofnetwork, String baseUrl, String modeltype, List<String[]> buslist)
			throws URISyntaxException, IOException {

		String genoutputInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
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
				+ "?Pg  a  j3:Pg  ." + "?Pg  j2:hasValue ?vpg ."// pg

				+ "?model   j5:hasModelVariable ?Qg ." 
				+ "?Qg  a  j3:Qg  ." 
				+ "?Qg  j2:hasValue ?vqg ."// qg

				+ "}";

		String busoutputInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
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
				+ "?num  a  j3:BusNumber  ." + "?num  j2:hasValue ?vnum ."
				+ "?vnum   j2:numericalValue ?BusNumbervalue ." // number

				+ "?model   j5:hasModelVariable ?Pd ." 
				+ "?Pd  a  j3:PdBus  ." 
				+ "?Pd  j2:hasValue ?vpdbus ." // pd
				+ "?vpdbus  a  j5:ModelVariableSpecification  ."

				+ "?model   j5:hasModelVariable ?Gd ." 
				+ "?Gd  a  j3:GdBus  ." 
				+ "?Gd  j2:hasValue ?vgdbus ." // Gd
				+ "?vgdbus  a  j5:ModelVariableSpecification  ."

				+ "?model   j5:hasModelVariable ?Pdgen ." 
				+ "?Pdgen  a  j3:PdGen  ." 
				+ "?Pdgen  j2:hasValue ?vpdgen ." // pdgen
				+ "?vpdgen  a  j5:ModelVariableSpecification  ."

				+ "?model   j5:hasModelVariable ?Gdgen ." 
				+ "?Gdgen  a  j3:GdGen  ." 
				+ "?Gdgen  j2:hasValue ?vgdgen ." // Gd
				+ "?vgdgen  a  j5:ModelVariableSpecification  ."

				+ "?model   j5:hasModelVariable ?VM ." 
				+ "?VM  a  j3:Vm  ." 
				+ "?VM  j2:hasValue ?vVM ."// Vm
				+ "?vVM  a  j5:ModelVariableSpecification  ."

				+ "?model   j5:hasModelVariable ?VA ." 
				+ "?VA  a  j3:Va  ." 
				+ "?VA  j2:hasValue ?vVA ."// Va
				+ "?vVA  a  j5:ModelVariableSpecification  ." 
				+ "}";

		String branchoutputInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
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
				+ "?ploss  j2:hasValue ?vploss ." // ploss

				+ "?model   j5:hasModelVariable ?qloss ." 
				+ "?qloss  a  j3:QLoss  ." 
				+ "?qloss  j2:hasValue ?vqloss ." // qloss

				+ "?model   j5:hasModelVariable ?pave ." 
				+ "?pave  a  j3:PAverage  ." 
				+ "?pave  j2:hasValue ?vpave ." // pave

				+ "?model   j5:hasModelVariable ?qave ." 
				+ "?qave  a  j3:QAverage  ." 
				+ "?qave  j2:hasValue ?vqave ." // qave

				+ "?model   j5:hasModelVariable ?save ." 
				+ "?save  a  j3:SAverage  ." 
				+ "?save  j2:hasValue ?vsave ." // save

				+ "}";


		
		QueryBroker broker = new QueryBroker();

		logger.info("extractOWLinArray for bus entities");
		List<String[]> busoutputlist = extractOWLinArray(model, iriofnetwork, busoutputInfo, "output", baseUrl);
		ArrayList<String[]> resultfrommodelbus = readResult(baseUrl + "/outputBus" + modeltype.toUpperCase() + ".txt");

		int amountofbus = busoutputlist.size();
		IriMapper map2 = new IriMapper();
		List<IriMapping> originalforbus = map2.deserialize2(baseUrl + "/mappingforbus.csv");
		for (int a = 0; a < amountofbus; a++) {
			
			String currentIri = busoutputlist.get(a)[1];
			OntModel jenaOwlModel = JenaHelper.createModel(currentIri);
			DatatypeProperty numval = getNumericalValueProperty(jenaOwlModel);

			// mapping from output tab to correct owl file
			String keymapper = busoutputlist.get(a)[0];
			int amod = Integer.valueOf(map2.getIDFromMap(originalforbus, keymapper));
			Individual vpdbusout = jenaOwlModel.getIndividual(busoutputlist.get(a)[1]);
			vpdbusout.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbus.get(amod - 1)[5])));

			Individual vgdbusout = jenaOwlModel.getIndividual(busoutputlist.get(a)[2]);
			vgdbusout.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbus.get(amod - 1)[6])));

			Individual vpdgenout = jenaOwlModel.getIndividual(busoutputlist.get(a)[3]);
			vpdgenout.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbus.get(amod - 1)[3])));

			Individual vgdgenout = jenaOwlModel.getIndividual(busoutputlist.get(a)[4]);
			vgdgenout.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbus.get(amod - 1)[4])));

			Individual vVmout = jenaOwlModel.getIndividual(busoutputlist.get(a)[5]);
			double basekv = Double.valueOf(buslist.get(amod - 1)[9]);
			//System.out.println("basekv= " + basekv);
			//System.out.println("pukv= " + resultfrommodelbus.get(amod - 1)[1]);
			double originalv = basekv * Double.valueOf(resultfrommodelbus.get(amod - 1)[1]);
			
			//vVmout.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(originalv)); //if vm is in kv
			vVmout.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbus.get(amod - 1)[1]))); //if vm is in pu
			

			Individual vVaout = jenaOwlModel.getIndividual(busoutputlist.get(a)[6]);
			vVaout.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbus.get(amod - 1)[2])));
			
			String content = JenaHelper.writeToString(jenaOwlModel);
//			broker.put(currentIri, content); tempchange
			broker.putOld(currentIri, content);
		}
		
		
		
		logger.info("extractOWLinArray for generator entities");
		List<String[]> genoutputlist = extractOWLinArray(model, iriofnetwork, genoutputInfo, "output", baseUrl);
		ArrayList<String[]> resultfrommodelgen = readResult(baseUrl + "/outputGen" + modeltype.toUpperCase() + ".txt");

		IriMapper map3 = new IriMapper();
		List<IriMapping> originalforgen = map3.deserialize2(baseUrl + "/mappingforgenerator.csv");
		int amountofgen = genoutputlist.size();
		logger.info("amount of gen in output="+amountofgen);
		for (int a = 0; a < amountofgen; a++) {
			
			String currentIri = genoutputlist.get(a)[1];
			OntModel jenaOwlModel = JenaHelper.createModel(currentIri);
			DatatypeProperty numval = getNumericalValueProperty(jenaOwlModel);

			// mapping from output tab to correct owl file
			String keymapper = genoutputlist.get(a)[0];

			int amod = Integer.valueOf(map3.getIDFromMap(originalforgen, keymapper));

			Individual vpout = jenaOwlModel.getIndividual(genoutputlist.get(a)[1]);
			vpout.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelgen.get(amod - 1)[1])));

			Individual vqout = jenaOwlModel.getIndividual(genoutputlist.get(a)[2]);
			vqout.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelgen.get(amod - 1)[2])));
			//logger.info("initial Pout= "+jenaOwlModel.createTypedLiteral(resultfrommodelgen.get(amod - 1)[1]));
			updateGeneratorEmission(jenaOwlModel); //add new functionality for updating the emission

			String content = JenaHelper.writeToString(jenaOwlModel);
//			broker.put(currentIri, content); tempchange
			broker.putOld(currentIri, content);
		}
		
		
		logger.info("extractOWLinArray for branch entities");
		List<String[]> branchoutputlist = extractOWLinArray(model, iriofnetwork, branchoutputInfo, "output", baseUrl);
		ArrayList<String[]> resultfrommodelbranch = readResult(
				baseUrl + "/outputBranch" + modeltype.toUpperCase() + ".txt");

		IriMapper map = new IriMapper();
		List<IriMapping> originalforbranch = map.deserialize2(baseUrl + "/mappingforbranch.csv");
		int amountofbranch = branchoutputlist.size();
		for (int a = 0; a < amountofbranch; a++) {
			
			String currentIri = branchoutputlist.get(a)[1];
			OntModel jenaOwlModel = JenaHelper.createModel(currentIri);
			DatatypeProperty numval = getNumericalValueProperty(jenaOwlModel);

			// mapping from output tab to correct owl file
			String keymapper = branchoutputlist.get(a)[0];
			int amod = Integer.valueOf(map.getIDFromMap(originalforbranch, keymapper));

			Individual vploss = jenaOwlModel.getIndividual(branchoutputlist.get(a)[1]);
			vploss.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbranch.get(amod - 1)[1])));

			Individual vqloss = jenaOwlModel.getIndividual(branchoutputlist.get(a)[2]);
			vqloss.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbranch.get(amod - 1)[2])));

			Individual vpave = jenaOwlModel.getIndividual(branchoutputlist.get(a)[3]);
			vpave.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbranch.get(amod - 1)[3])));

			Individual vqave = jenaOwlModel.getIndividual(branchoutputlist.get(a)[4]);
			vqave.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbranch.get(amod - 1)[4])));

			Individual vsave = jenaOwlModel.getIndividual(branchoutputlist.get(a)[5]);
			vsave.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(resultfrommodelbranch.get(amod - 1)[5])));
			
			String content = JenaHelper.writeToString(jenaOwlModel);
//			broker.put(currentIri, content); tempchange
			broker.putOld(currentIri, content);
		}
	}
	/** Update the carbon emission value in each generator
	 * 
	 * @param model
	 */
	public void updateGeneratorEmission(OntModel model) {
		String genInfo = new SelectBuilder()
			.addPrefix("j1", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
			.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
			.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
			.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#")
			.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")
			.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#")
			.addPrefix("j10", "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#")
			.addVar("?entity").addVar("?Pvalue").addVar("?valueemm").addVar("?emissionfactor")
			.addWhere("?entity", "a", "j1:PowerGenerator")
			.addWhere("?entity", "j2:isModeledBy", "?model")
			.addWhere("?model", "j5:hasModelVariable", "?p")
	        .addWhere("?p","a","j3:Pg")
	        .addWhere("?p","j2:hasValue", "?vp")
	        .addWhere("?vp","j2:numericalValue", "?Pvalue")
	        
			.addWhere("?entity", "j6:realizes", "?genprocess")
	        .addWhere( "?genprocess", "j9:hasEmission", "?emm")
	        .addWhere("?emm", "a", "j9:Actual_CO2_Emission")
	        .addWhere("?emm","j2:hasValue", "?valueemm")	
	        
			.addWhere("?genprocess", "j10:usesGenerationTechnology", "?tech")
	        .addWhere("?tech","j10:hasEmissionFactor","?emmfac")
	        .addWhere("?emmfac","j2:hasValue", "?valueemmfac")
	        .addWhere("?valueemmfac","j2:numericalValue", "?emissionfactor")
			.buildString();
		List<String[]> resultListfromquery = Util.queryResult(model, genInfo);
		DatatypeProperty numval = getNumericalValueProperty(model);
		for (int c = 0; c < resultListfromquery.size(); c++) {
		double pvalue=	Double.valueOf(resultListfromquery.get(c)[1]);
		double emissionf=	Double.valueOf(resultListfromquery.get(c)[3]);
		double actualem=pvalue*emissionf;
		Individual vemissioninstance = model.getIndividual(resultListfromquery.get(c)[2]);
		vemissioninstance.setPropertyValue(numval, model.createTypedLiteral(new Double(actualem)));
		}
		
		
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
    	if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean w = InputValidator.checkIfValidIRI(ENIRI);
	        return w;
        } catch (JSONException ex) {
        	ex.printStackTrace();
        }
        return false;
    }
}
