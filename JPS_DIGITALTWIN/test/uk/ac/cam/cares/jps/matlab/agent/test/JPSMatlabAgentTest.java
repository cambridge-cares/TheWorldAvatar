package uk.ac.cam.cares.jps.matlab.agent.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class JPSMatlabAgentTest extends TestCase {
	
	public void testFCQueryAgent(){
		JSONObject jo = new JSONObject();
		try {
			String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DIGITALTWIN/JPSMatlabAgent", jo.toString());
			System.out.println(resultStart);
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
