package uk.ac.cam.cares.jps.dispersion.agents.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;

public class InterpolationPyAgentTest extends TestCase {

	/**
	 * in order to run this test you must have at least one completed Episode simulation stored in the metadata endpoint
	 */
	public void testInterpolationPyAgent() {
    	JSONObject request = new JSONObject();
    	request.put(Region.keyCity, Region.SINGAPORE_IRI);
		String resultlocation = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/episode/results/latest", request.toString());
		
		JSONObject requestParams = new JSONObject();
		requestParams.put("filepath", resultlocation);
		requestParams.put("x", "364629.312");
		requestParams.put("y", "131795.703");
		
		AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/InterpolationPyAgent", requestParams.toString());
	}
}
