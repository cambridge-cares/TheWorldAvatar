package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class SpeedLoadMapAgentTest extends TestCase{
    public void testSpeedLoadMapAgent() {
    	String ship_iri = "http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#ship1";
    	JSONObject request = new JSONObject();
    	request.put("shipIRI",ship_iri);
    	AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/SpeedLoadMapAgent", request.toString());
    }
}
