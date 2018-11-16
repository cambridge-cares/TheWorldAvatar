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
			dataSet.put("plant", "http://www.theworldavatar.com/kb/powerplants/Langerlo_Thermal_Power_Plant_Belgium.owl#Langerlo_Thermal_Power_Plant_Belgium") ;
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		

		String json = dataSet.toString();
		String resultjson = AgentCaller.executeGet(getContextPathForJPSco2emissionefactor(), "query", json);
		

		double emission = new JSONObject(resultjson).getJSONObject("hasEmission").getJSONObject("hasValue").getDouble("numericalValue");
		assertEquals(444.8, emission, 1.);


	}
}
