package uk.ac.cam.cares.jps.ship.test;


import java.io.IOException;

import org.json.JSONObject;

import junit.framework.TestCase;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;


public class TestSpeedLoadMap extends TestCase {

	public void testcallspeedloadmapWrapper() throws IOException {
		
		
			JSONObject jo = new JSONObject();
			jo.put("speed",8.0); //ori=11
			jo.put("type","passenger");
			String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SLMAgent",jo.toString());
			System.out.println("result= "+result);
			
			JSONObject joresult = new JSONObject(result);
//			assertEquals(joresult.getJSONObject("mixture").getJSONObject("massflux").get("value"), 13.392369102033804); //mass flux
//			assertEquals(joresult.getJSONArray("pollutants").getJSONObject(4).get("value"), 0.006041259571151734); //no2
			
		
	}
}
