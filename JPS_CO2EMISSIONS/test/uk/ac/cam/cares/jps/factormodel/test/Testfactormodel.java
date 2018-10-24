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


		
		//AgentRequest request = new AgentRequest();

		
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
		

		//Gson gson=new Gson();
		//Parameter param = new Parameter("query", dataSet.toString());
		//Parameter param = new Parameter("plantiri", "http://www.theworldavatar.com/kb/powerplants/Uskmouth_B_Coal_Power_Station__UK.owl#Uskmouth_B_Coal_Power_Station__UK");
		//request.getInputParameters().add(param);

		

		// add output parameter with key = test

		//AgentResponse resp = AgentCaller.callAgent(getContextPathForJPSco2emissionefactor(), request);

		// search for outputparameter with key = test
		//Double emission = Double.valueOf((String) resp.getOutputParameters().get(0).getValue());

	

		assertEquals(1142.4, emission, 1000.);


	}
}
