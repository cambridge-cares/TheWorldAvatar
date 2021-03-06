package uk.ac.cam.cares.jps.powerplant.test;

import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestPowerPlantAgent extends TestCase {
	
	
	private String getContextPathForJPSco2emissionefactor() {
		return "/JPS/PowerPlant";
	}
	
	public void testCallAgent () throws JSONException {

		// EIP --> one parameter
		JSONObject dataSet = new JSONObject();
		try {
			dataSet.put("reactionmechanism", "http://www.theworldavatar.com/kb/ontokin/Toluene.owl#ReactionMechanism_187077735769001") ;
			dataSet.put("plant", "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002") ;
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
		String json = dataSet.toString();
		String resultjson = AgentCaller.executeGet(getContextPathForJPSco2emissionefactor(), "query", json);
		System.out.println ("resultjson= "+resultjson);
	
		
		//double emission = new JSONObject(resultjson).getJSONObject("hasEmission").getJSONObject("hasValue").getDouble("numericalValue"); //changed later
		String emission = new JSONObject(resultjson).getString("waste");
		assertEquals("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#WasteStream-001", emission);
	
	}
}
