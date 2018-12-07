package uk.ac.cam.cares.jps.srm.test;

import org.json.JSONException;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestSRMAgent extends TestCase {
	
	private String getContextPathForJPSSRMAgent() {
		return "/JPS/SRMAgent";
	}
	
	public void testCallAgent () throws JSONException {
		JSONObject dataSet = new JSONObject();
		try {
			dataSet.put("reactionmechanism", "https://como.cheng.cam.ac.uk/kb/MD.owl#ReactionMechanism_141528749904597") ;
			dataSet.put("engine", "http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#DieselEngine-001") ;
		}
		catch (JSONException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		
		String json = dataSet.toString();
		String resultjson = AgentCaller.executeGet(getContextPathForJPSSRMAgent(), "query", json);
		System.out.println ("resultjson= "+resultjson);
		
		String srmjsonfile = new JSONObject(resultjson).getString("file");
		System.out.println("corrected json file= "+srmjsonfile);
	}
}
