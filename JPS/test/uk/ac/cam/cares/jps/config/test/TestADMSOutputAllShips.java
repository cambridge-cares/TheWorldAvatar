package uk.ac.cam.cares.jps.config.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestADMSOutputAllShips extends TestCase {
	public void testoutputcall() {
		JSONObject empty= new JSONObject();
		empty.put("folder","C:/JPS_DATA/workingdir/JPS_SCENARIO/scenario/base/localhost_8080/data/d9441c17-728f-4c81-b0e2-a4dacbedd1e6/JPS_ADMS/test.levels.gst");
		String resp=AgentCaller.executeGetWithJsonParameter("JPS/ADMSOutputAllForShips", empty.toString());
		System.out.println("resp= "+resp);
	}

}
