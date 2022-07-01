package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;

public class DMSCoordinationAgentTest {
	@Test
	public void testAgentCall() {
		JSONObject jo = new JSONObject();
		jo.put(DispSimSparql.SimKey, "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim1");
		AgentCaller.executeGetWithURLAndJSON("http://localhost:8081/JPS_VIRTUALSENSOR/DMSCoordinationAgent", jo.toString());
	}
}
