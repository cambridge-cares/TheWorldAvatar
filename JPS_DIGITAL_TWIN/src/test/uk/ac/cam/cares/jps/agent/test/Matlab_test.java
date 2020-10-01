package uk.ac.cam.cares.jps.agent.test;

import java.net.URI;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class Matlab_test extends TestCase {
	
	public void testFCQueryAgent(){
		JSONObject jo = new JSONObject();
		try {
	        URI uri = AgentCaller.createURIWithURLandJSON("ElChemoAgent/test", jo.toString());
	        System.out.println(uri);
			String resultStart = AgentCaller.executeGetWithJsonParameter("ElChemoAgent/test", jo.toString());
			System.out.println(resultStart);
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
