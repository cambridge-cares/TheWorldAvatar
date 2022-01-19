package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.virtualsensor.sparql.ShipSparql;

public class SpeedLoadMapAgentTest extends TestCase{
    public void testSpeedLoadMapAgent() {
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship1";
    	JSONObject request = new JSONObject();
    	request.put(ShipSparql.shipKey,ship_iri);
    	AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/SpeedLoadMapAgent", request.toString());
    }
}
