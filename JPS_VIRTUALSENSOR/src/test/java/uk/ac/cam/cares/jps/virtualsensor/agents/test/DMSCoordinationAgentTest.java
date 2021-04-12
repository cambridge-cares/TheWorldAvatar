package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;

public class DMSCoordinationAgentTest extends TestCase {
	public void testAgentCall() {
		JSONObject jo = new JSONObject();
		jo.put(DispSimSparql.SimKey, "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim6");
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/DMSCoordinationAgent",jo.toString());
	}
}
