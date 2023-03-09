package uk.ac.cam.cares.jps.bio;


import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
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
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

 
@WebServlet("/DoSimulation")
public class DoSimulation extends JPSHttpServlet {
	public static String root=AgentLocator.getProperty("absdir.root");
	String Sim4 = root+"/Sim_BIODIESELPLANT/APPWSIM"; // THIS SIMULATION NEED TO BE EXIST 
	private static final long serialVersionUID = 1L;  
//	public static String APPWSim = new String("C:\\Users\\LONG01\\RO\\APPWSim"); // output CSV file from the pr aspen plus model

    /**
     * @see HttpServlet#HttpServlet()
     */
    public DoSimulation() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		JSONObject jo = AgentCaller.readJsonParameter(request);
		String iriString =  jo.optString("PLANTIRI", "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant3/BiodieselPlant3.owl");
		JSONObject result = callReq(iriString);

		placeinOWLFiles(result, iriString);
		
	}
	/** calls doSimulation via iriString and 
	 * 
	 * @param:iriString, string where biodiesel plant iri is called.  
	 * @return: returns JSON Object of six results from doSimulation
	 */
	public JSONObject callReq(String iriString) throws IOException {
		String reactorInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>"
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#>"
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>"
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>"
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>"
				+ "SELECT ?entity ?vmolarFinvalue ?vTinvalue ?loadiri " 
				+ "WHERE {?entity  a  j1:StirredTank  ."
				+ "?entity   j2:hasElectricalRepresentation  ?loadiri ."
				+ "?entity   j4:realizes  ?proc ."

				+ "?proc  j5:hasInput ?input ."
				//+ "?input a j3:RawMaterial ." not sure why this is not a raw material
				+ "?input  j6:refersToGeneralizedAmount ?genAmountin ." 
				+ "?genAmountin  j2:hasSubsystem ?matAmountin ."
				+ "?matAmountin  j2:hasProperty ?molarFin ."
				+ "?molarFin j2:hasValue ?vmolarFin ."
				+ "?vmolarFin  j2:numericalValue ?vmolarFinvalue ."
				
				+ "?matAmountin  j7:refersToMaterial ?matin ."
				+ "?matin  j8:thermodynamicBehavior ?singphasein ."
				+ "?singphasein  j9:has_temperature ?Tin ."
				+ "?Tin  j2:hasValue ?vTin ."
				+ "?vTin  j2:numericalValue ?vTinvalue ."
				+ "}";
	   String pumpInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/machine.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>"
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#>"
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>"
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>"
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>"
				+ "SELECT ?entity ?vmolarFinvalue ?vPinvalue " 
				+ "WHERE {?entity  a  j1:Pump  ."
				+ "?entity   j4:realizes  ?proc ." 
				+ "?proc  j5:hasInput ?input ."
				+ "?input  j6:refersToGeneralizedAmount ?genAmountin ." 
				+ "?genAmountin  j2:hasSubsystem ?matAmountin ."
				+ "?matAmountin  j2:hasProperty ?molarFin ."
				+ "?molarFin j2:hasValue ?vmolarFin ."
				+ "?vmolarFin  j2:numericalValue ?vmolarFinvalue ."
				
				+ "?matAmountin  j7:refersToMaterial ?matin ."
				+ "?matin  j8:thermodynamicBehavior ?singphasein ."
				+ "?singphasein  j9:has_pressure ?Pin ."
				+ "?Pin  j2:hasValue ?vPin ."
				+ "?vPin  j2:numericalValue ?vPinvalue ."
		 
				+ "}";
		
		String heaterInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>"
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#>"
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>"
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>"
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>"
				+ "SELECT ?entity ?vmolarFinvalue ?vTinvalue " 
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
				+ "?matin  j8:thermodynamicBehavior ?singphasein ."
				+ "?singphasein  j9:has_temperature ?Tin ."
				+ "?Tin  j2:hasValue ?vTin ."
				+ "?vTin  j2:numericalValue ?vTinvalue ."
				+ "}";
		OntModel model = readModelGreedy(iriString);
		List<String[]> pumpList = getResultList(model, pumpInfo);
		List<String[]> heatList = getResultList(model, heaterInfo);
		List<String[]> reactList = getResultList(model, reactorInfo);

		String[] inputs = {heatList.get(0)[1],heatList.get(0)[2], reactList.get(0)[1],reactList.get(0)[2],pumpList.get(0)[1], pumpList.get(0)[2]};
		Double input1 = null,input2 = null,input3 = null,input4 = null,input5 = null,input6 = null;
		
		
		Double[] inputs_num = {input1,input2,input3,input4,input5,input6};
		inputs_num = new Double[]{33.0,30.0,180.0,30.0,233.135,4.0};
		for(int i = 0; i < inputs_num.length ; i++){
			inputs_num[i] = Double.parseDouble(inputs[i]); // convert string inputs to doubles
		}
		JSONObject result = doSimulation(inputs_num);
		return result;
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
 
	}
	public OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
	public List<String[]> getResultList(OntModel model, String info){
	   ResultSet resultSet = JenaHelper.query(model, info);
	   String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
	   String[] keys = JenaResultSetFormatter.getKeys(result);
	   List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
	   return resultList;
   }
	// The main function that does the simulation which requires two inputs 
	// SimulationIRI find the simulation individuals in the owl file where the inputs, model name and outputs
	// The editStack stores the user inputs
	/**
	 * @param inputs double array
	 * @return JSONObject of six doubles. 
	 * @throws IOException
	 */
	@SuppressWarnings("resource")
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
		ArrayList<Double> xRow = new ArrayList<Double>(Arrays.asList(inputs));                                   // extra arraylist to collect the x-value required as input to the pr aspen plus model
	/*
		FileWriter filewriter = new FileWriter(APINCSV);
		filewriter.append("FOIL, TOIL, FMEOH, TMEOH, FREWATER, PBOILER");
		filewriter.append("33,30.0,180.0,30.0,233.135,4.0");
		FOIL and TOIL in E-301 (V_molarF_3-1,V_Temperature_3-1 --->http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/E-301.owl#V_molarF_3-1,http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/E-301.owl#V_Temperature_3-1)
		FREWATER and  PBOILER in P-302 (V_molarF_Utility_FW-301,ValueOfOutletPressureOfP-302--->http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/P-302.owl#V_molarF_Utility_FW-301,http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/P-302.owl#ValueOfOutletPressureOfP-302)
		FMEOH and TMEOH in reactor 301 most probably (http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/R-301.owl#V_molarF_3-4, http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/R-301.owl#V_Temperature_3-4)
		
		input IRI=http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/R-301.owl
		input IRI=http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/E-301.owl
		input IRI=http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/P-302.owl
 */
		List<Double> xData = new ArrayList<>(1);                                    //Rather than header, insert empty Array List
 
		String simDir = Sim4;	
		String modelName = "Polynomial_Alg_1";
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
 			simResult.put("ValueOfHeatDutyOfR-301",yData.get(25)); //R-301
 			simResult.put("V_theta_R-301load",yData.get(112));//R-301 load
 			simResult.put("V_ActualV_R-301",yData.get(113));//R-301 load
 			simResult.put("ValueOfHeatDutyOfR-302",yData.get(23));//R-302
 			simResult.put("V_theta_R-302load",yData.get(102));//R-302 load
 			simResult.put("V_ActualV_R-302",yData.get(103));//R-302 load
 			
 			
 		
 		System.out.println("ans:");
 		System.out.println(yData.get(25));
 		System.out.println(yData.get(112));
 		
 		
 		return simResult;
	}
	/**Dump in owl files the simulation result
	 * @param simResult JSONObject containing six values
	 * @return null
	 */
	public void placeinOWLFiles(JSONObject simResult, String iriString) {
		String[] d = irisToBeUsed(iriString);
		QueryBroker broker = new QueryBroker();
		for (String i: d) {
			OntModel jenaOwlModel = JenaHelper.createModel(i);//OBJECT 
			String reactor = i.substring(i.length()-5);
			DatatypeProperty numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
			Individual vH = jenaOwlModel.getIndividual(i.split("#")[0]+ "#ValueOfHeatDutyOf"+reactor);
			vH.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(simResult.get("ValueOfHeatDutyOf" + reactor).toString()) );
			String content = JenaHelper.writeToString(jenaOwlModel);
			broker.putOld(i, content);
			
			//store in loadfile rather than load
			String newM = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/" +reactor + "load.owl";
			jenaOwlModel = JenaHelper.createModel(newM +"#"+reactor+"load");//OBJECT 
			Individual vAngle = jenaOwlModel.getIndividual(i.split("#")[0]+  "#V_theta_"+reactor+"load");
			vH.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(simResult.get("V_theta_" + reactor+"load").toString()) );
			
			Individual vVolt = jenaOwlModel.getIndividual(newM+ "#V_ActualV_"+reactor+"load");
			vVolt.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(simResult.get("V_ActualV_" + reactor).toString()) );
			content = JenaHelper.writeToString(jenaOwlModel);
			broker.putOld(newM, content);
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
					    +"?entity  a  j1:StirredTank ."
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
			 ans[i] = a.get(i)[0];
		 }
		 return ans;
	}

}
