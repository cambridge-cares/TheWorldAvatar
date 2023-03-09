package uk.ac.cam.cares.bio.test;

import java.io.IOException;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.bio.DoSimulation2;

public class Test_DoSimulation2 extends TestCase{
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
	   public void testBiodieselPlant() throws IOException {
		   String iriofnetwork = "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant2/BiodieselPlant2.owl";
		   DoSimulation2 n = new DoSimulation2();
		   JSONObject heaterList = n.callReq(iriofnetwork);
		   System.out.println(heaterList.toString());
	   }
	   public void testSimulationAgentCall() {
		   JSONObject jo = new JSONObject();
		   jo.put("PLANTIRI", "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant2/BiodieselPlant2.owl");
			String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_BIODIESELPLANT3/DoSimulation2", jo.toString());
	   
	   }
	   public void testDumpOWL() {
		   JSONObject jo = new JSONObject();
		   jo.put("ValueOfHeatDutyOfE-601001",97278.71561607323);
		   jo.put("ValueOfHeatDutyOfE-601004",196623.23770461947);
		   jo.put("V_molarF_601039",90.00933660142013);
		   jo.put("ValueOfHeatDutyOfE-601002",290142.05932762893);
		   jo.put("ValueOfHeatDutyOfE-601003",103803.70706111955);
		   new DoSimulation2().placeinOWLFiles(jo, "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant2/BiodieselPlant2.owl");
	   }
}

