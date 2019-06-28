package uk.ac.cam.cares.jps.ship.test;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
public class TestCoordinatorCollection extends TestCase{

//	public void testreadwritedata() {
//		HKUWeatherRetriever.readWritedata();
//		}
	
	public void testcallingAgent() {
		JSONObject jo = new JSONObject();
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/CollectorCoordination",jo.toString());
		System.out.println("it is executed");
		System.out.println("result= "+resultStart);
		
	}
	
	public void testcallhkuweatheragent() {
		JSONObject jo = new JSONObject();
		String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/GetHKUWeatherData",jo.toString());
		System.out.println("result= "+result);
	}
	 
}
