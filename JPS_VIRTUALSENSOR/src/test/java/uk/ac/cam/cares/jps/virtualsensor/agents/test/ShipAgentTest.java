package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

public class ShipAgentTest{
	@Test
	public void testShipAgent() {
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship2";
    	JSONObject request = new JSONObject();
    	request.put(ShipSparql.shipKey,ship_iri);
    	AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/ShipAgent", request.toString());
    }
}
