package uk.ac.cam.cares.jps.dispersion.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class AQMeshCoordinationAgentTest extends TestCase {

	public void testcallcoordination() {
		JSONObject jo = new JSONObject();
		jo.put("city", "http://dbpedia.org/resource/Singapore");
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/AQMeshCoordinationAgent", jo.toString());
		System.out.println("result= "+resultStart);
		System.out.println("done");
	}
}
