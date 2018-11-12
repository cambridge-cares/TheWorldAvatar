package uk.ac.cam.cares.jps.factormodel.test;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class Testfactormodel extends TestCase {

	private String getContextPathForJPSco2emissionefactor() {
		return "/JPS_CO2EMISSIONS/FactorModel";
	}
	
	public void testCallAgent() throws IOException, URISyntaxException, JSONException {


		
		// EIP --> one parameter
		JSONObject dataSet = new JSONObject();
		try {
			dataSet.put("plant", "http://www.theworldavatar.com/kb/powerplants/Uskmouth_B_Coal_Power_Station__UK.owl#Uskmouth_B_Coal_Power_Station__UK") ;
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		

		String json = dataSet.toString();
		String resultjson = AgentCaller.executeGet(getContextPathForJPSco2emissionefactor(), "query", json);
		
		double emission = new JSONObject(resultjson).getDouble("emission");

		assertEquals(1142.4, emission, 1000.);


	}
}
