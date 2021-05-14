package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.virtualsensor.sparql.SensorSparql;

public class SensorUpdaterAgentTest extends TestCase {
	/**
	 * to run this test, you must have a sensor called 
	 * http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#virtualsensor1
	 * in your endpoint
	 */
	public void testAgentCall() {
		String stationIRI = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#virtualsensor1";
		JSONObject request = new JSONObject();
		request.put(SensorSparql.keyAirStationIRI, stationIRI);
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/SensorUpdaterAgent", request.toString());
	}
}
