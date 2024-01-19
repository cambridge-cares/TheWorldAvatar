package uk.ac.cam.cares.bio.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.bio.SimulationCoordination;

public class Test_SimCoord extends TestCase{
	public void testSimulationAgentCall() {
		   JSONObject jo = new JSONObject();
		   jo.put("PLANTIRI", "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant2/BiodieselPlant3.owl");
			String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_BIODIESELPLANT3/SimCoord", jo.toString());
	   
	   }
	public void testSimulationDirectCall() {
		   JSONObject jo = new JSONObject();
		   SimulationCoordination a = new SimulationCoordination();
		   a.helperRequest();
	   
	   }
	public void testSimulationSelection() {
		JSONObject jo = new JSONObject().put("componentIRI", "http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant2/R-601004.owl#R-601004");
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_BIODIESELPLANT3/DoModelSelection", jo.toString());
		   System.out.println(resultStart);
	}
}
