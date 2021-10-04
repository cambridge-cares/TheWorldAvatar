package uk.ac.cam.cares.jps.bio;


import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import com.cmclinnovations.mods.api.MoDSAPI;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;

//import com.cmclinnovations.modsapi.MoDSAPI;

import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

/**
 * Servlet implementation class DoSimulation2
 */
@WebServlet("/DoSimulation2")
public class DoSimulation2 extends  JPSHttpServlet{
	private static final long serialVersionUID = 1L;
	public static String root=AgentLocator.getProperty("absdir.root");
	String APPWSim = root+"/Sim_BIODIESELPLANT/BD_WWHR_Sim"; // THIS SIMULATION NEED TO BE EXIST 
    public DoSimulation2() {
        super();
     }

 /***
  * receives request and calls callReq
  * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
  */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub

		JSONObject jo = AgentCaller.readJsonParameter(request);
		String iriString =  jo.optString("PLANTIRI", "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant2/BiodieselPlant2.owl");
		JSONObject result = callReq(iriString);
		placeinOWLFiles(result, iriString);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
 
	}
	
	// The main function that does the simulation which requires two inputs 
	// SimulationIRI find the simulation individuals in the owl file where the inputs, model name and outputs
	// The editStack stores the user inputs
	public JSONObject callReq(String iriString) throws IOException {
		String heaterInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>"
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#>"
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>"
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>"
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>"
				+ "SELECT ?entity ?vmolarFinvalue ?vT1invalue ?vT2invalue " 
				+ "WHERE {?entity  a  j1:ShellTubeApparatus  ."
				+ "?entity   j4:realizes  ?proc ." 
				+ "?proc  j5:hasInput ?input ."
				+ "?input a j3:RawMaterial ."
				+ "?input  j6:refersToGeneralizedAmount ?genAmountin ." 
				+ "?genAmountin  j2:hasSubsystem ?matAmountin ."
				+ "?matAmountin  j2:hasProperty ?molarFin ."
				+ "?molarFin j2:hasValue ?vmolarFin ."
				+ "?vmolarFin  j2:numericalValue ?vmolarFinvalue ."
				
				
				+ "?matAmountin  j7:refersToMaterial ?matin ."
				+ "?matin j9:has_temperature ?Tin ."
				+ "?Tin  j2:hasValue ?vTin ."
				+ "?vTin  j2:numericalValue ?vT1invalue ."
				
				+ "?proc  j5:hasOutput ?ouput ."
				+ "?ouput j6:refersToGeneralizedAmount ?genAmountout ."
				+ "?genAmountout j2:hasSubsystem ?matAmountout ."
				+ "?matAmountout  j7:refersToMaterial ?matout ."
				+ "?matout j9:has_temperature ?Tout ."
				+ "?Tout  j2:hasValue ?vTout ."
				+ "?vTout  j2:numericalValue ?vT2invalue ."
				+ "}";
		OntModel model = readModelGreedy(iriString);
		List<String[]> heatList = getResultList(model, heaterInfo);
		String[] inputs = {heatList.get(0)[1],heatList.get(0)[2], heatList.get(0)[3]};
		
		Double input1 = null,input2 = null,input3 = null;
		
		
		Double[] inputs_num = {input1,input2,input3};
		inputs_num = new Double[]{31.3,30.0,70.0};
		for(int i = 0; i < inputs_num.length ; i++){
			inputs_num[i] = Double.parseDouble(inputs[i]); // convert string inputs to doubles
		}
		JSONObject result = doSimulation(inputs_num);
		return result;
	}
	public OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
	public List<String[]> getResultList( OntModel model, String info){
	   ResultSet resultSet = JenaHelper.query(model, info);
	   String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
	   String[] keys = JenaResultSetFormatter.getKeys(result);
	   List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
	   return resultList;
   }
	public JSONObject doSimulation(Double[] inputs) throws IOException
	{
		// First use hardcoded informations instead of reading info from owl files
		// 1. Write to APIN.csv 
		/*  FOIL, TOIL, FMEOH, TMEOH, FREWATER, PBOILER
		 *	33,30.0,180.0,30.0,233.135,4.0
		 *	The output needed to be collected are 
		 *	25 - ValueOfHeatDutyOfR-301     			in R-301
		 *	112- V_Angle_LoadPoint_R-602001 			in R-301
		 *	113- V_ActualVoltage_LoadPoint_R-602001		in R-301
		 *	23 - ValueOfHeatDutyOfR-302					in R-302
		 *	102- V_Angle_LoadPoint_R-602002				in R-302
		 *	103- V_ActualVoltage_LoadPoint_R-602002		in R-302
		 */
		ArrayList<Double> xRow = new ArrayList<Double>(Arrays.asList(inputs));                                      // extra arraylist to collect the x-value required as input to the pr aspen plus model
	/*
		FileWriter filewriter = new FileWriter(APINCSV);
		filewriter.append("FOIL, TOIL, FMEOH, TMEOH, FREWATER, PBOILER");
		filewriter.append("33,30.0,180.0,30.0,233.135,4.0");
 */
		List<Double> xData = new ArrayList<>(1);                                    // arraylist to
 
		String simDir = APPWSim;	
		String modelName = "HDMR_Alg_1";
		FileWriter fileWriter = null;
		xData.addAll(xRow); 
		try {
			System.load(root + "/MoDS_Java_API_0.1.dll");      //the MoDS API at use is version 0.1  D:\MoDS_API\MoDS_Java_API_v0.1
		} catch (Error e) {
			e.printStackTrace();
		} 
	
 		List<Double> yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData);   // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam"  and  the input xData that was collected before
 		ArrayList<String[]> result = new ArrayList<String[]>();
 		//List y = yData.get(0);
 		
 		JSONObject simResult=new JSONObject();
 		simResult.put("V_molarF_601039",String.valueOf(yData.get(0)));
 		simResult.put("ValueOfHeatDutyOfE-601001",yData.get(4));
 		simResult.put("ValueOfHeatDutyOfE-601002",yData.get(5));
 		simResult.put("ValueOfHeatDutyOfE-601003",yData.get(6));
 		simResult.put("ValueOfHeatDutyOfE-601004",yData.get(7));
 		
 		return simResult;
	}
	public void placeinOWLFiles(JSONObject simResult, String iriString) {
		String Prefix = "http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant2/";
		Iterator<String> keys = simResult.keys();
		//store in T the molarF individually. 
		String i = "http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant2/T-601002.owl";
		OntModel jenaOwlModel = JenaHelper.createModel(i);//OBJECT 
		Individual vH = jenaOwlModel.getIndividual(i+ "#V_molarF_601039");
		DatatypeProperty numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		vH.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(simResult.get("V_molarF_601039").toString()));

		QueryBroker broker = new QueryBroker();
		String content = JenaHelper.writeToString(jenaOwlModel);
		broker.putOld(i, content);
		String[] d = irisToBeUsed(iriString);
		for (String j: d) {
			jenaOwlModel = JenaHelper.createModel(Prefix + j);//OBJECT 
			String[] reactore = j.split(".owl");
			String reactor = reactore[0];
			vH = jenaOwlModel.getIndividual(Prefix + j+ "#ValueOfHeatDutyOf"+reactor);
			vH.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(simResult.get("ValueOfHeatDutyOf" + reactor).toString()) );
			content = JenaHelper.writeToString(jenaOwlModel);
			broker.putOld(Prefix + j, content);
		}
		
	}
	private String[] irisToBeUsed(String iriString) {
		OntModel model =  readModelGreedy(iriString); 
		String info = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#> "
					+"PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+"PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#> "
					+"PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
					+"PREFIX j5:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+"PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#> "
					+"PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
					+"PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#> "
					+"PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
					+"SELECT Distinct ?entity "
					+"WHERE {"
					  +"{"
					    +"?entity  a  j1:ShellTubeApparatus ."
						+"?entity   j4:realizes  ?proc ."
						+"?proc  j5:hasInput ?input ."
						+"?input a j3:RawMaterial ."
						+"?proc  j5:hasOutput ?ouput ."
					  	+"?ouput  a  j3:ProcessStream ."
						+"?proc  j7:hasHeatDuty ?HD ."
						+"?HD  j2:hasValue ?vHD ."
					  +"}UNION { "
					    +"?entity   j4:realizes  ?proc ."
						+"?proc  j5:hasInput ?input ."
						+"?input  a  j3:ProcessStream ."
						+"?proc  j5:hasOutput ?ouput ."
					  	+"?ouput  a  j3:ProcessStream ."
						+"?proc  j7:hasHeatDuty ?HD ."
						+"?HD  j2:hasValue ?vHD . }"
						+"}";
		 List<String[]> a = getResultList(model, info);
		 String[] ans = new String[a.size()];
		 for (int i =0; i < a.size(); i++) {
			 ans[i] = a.get(i)[0].split("#")[1]+".owl";
		 }
		 return ans;
	}

}
