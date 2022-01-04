package uk.ac.cam.cares.bio.test;

import java.io.IOException;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.bio.DoSimulation;

public class Test_DoSimulation extends TestCase{
	protected int value1, value2;
	   
	   // assigning the values
	   protected void setUp(){
	      value1 = 3;
	      value2 = 3;
	   }

	   // test method to add two values
	   public void testAdd(){
	      double result = value1 + value2;
	      assertTrue(result == 6);
	   }
	   public void testSimulation() throws IOException{
		   Double[] inputs_num = new Double[]{33.0,30.0,180.0,30.0,233.135,4.0};
//		   System.out.println(new DoSimulation().doSimulation(inputs_num));
		   new DoSimulation().callReq("http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant3/BiodieselPlant3.owl");
	   }
	   public void testSimulationAgent() throws IOException{
		   JSONObject jo = new JSONObject();
		   jo.put("PLANTIRI", "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant3/BiodieselPlant3.owl");
			String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_BIODIESELPLANT3/DoSimulation", jo.toString());
	   }
	   public List<String[]> getResultList(OntModel model, String info){
		   ResultSet resultSet = JenaHelper.query(model, info);
		   String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		   String[] keys = JenaResultSetFormatter.getKeys(result);
		   List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		   return resultList;
	   }
	   
	   public void testBiodieselPlant() {
		   String iriofnetwork = "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant3/BiodieselPlant3.owl";
		   DoSimulation a = new DoSimulation();
		   OntModel model = a.readModelGreedy(iriofnetwork);
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
			List<String[]> pumpList = getResultList(model,pumpInfo);
			List<String[]> heatList = getResultList(model,heaterInfo);
			List<String[]> reactList = getResultList(model,reactorInfo);
		   
		   System.out.println();
	   }
	   public void testDumpOWL() {
		   JSONObject jo = new JSONObject();
		   jo.put("V_Angle_LoadPoint_R-301",-0.123456);
		   jo.put("ValueOfHeatDutyOfR-302",0.43186899431558023);
		   jo.put("V_ActualV_R-302",2.966610277813966);
		   jo.put("ValueOfHeatDutyOfR-301",1.1612685888155425);
		   jo.put("V_ActualV_R-301",3.3821878615224006);
		   jo.put("V_Angle_LoadPoint_R-302",-0.48014259831225964);
		   new DoSimulation().placeinOWLFiles(jo, "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant3/BiodieselPlant3.owl#BiodieselPlant3");
	   }
	   

}
