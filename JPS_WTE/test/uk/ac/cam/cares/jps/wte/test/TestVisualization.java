package uk.ac.cam.cares.jps.wte.test;

import org.apache.jena.ontology.OntModel;
import org.json.JSONArray;
import org.json.JSONObject;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;
import uk.ac.cam.cares.jps.wte.visualization.WTEVisualization;

public class TestVisualization  extends TestCase {
	public String WasteTopNode = "http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
	static String iriofnetwork="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
	
	/** produces xy coordinates of each FC on the network. 
	 * tests if the result is empty or if it contains the coordinates/values
	 */
	public void testFCQueryDirect(){
		WTEVisualization a = new WTEVisualization();
		JSONObject jo = new JSONObject();

		try {
		OntModel model = WastetoEnergyAgent.readModelGreedy(iriofnetwork);
			String result = a.createMarkers(model, jo);
			assertNotNull(result);
			JSONObject fcMap = new JSONObject(result);
			assertTrue(fcMap.has("result"));
			//result has to be a string and not a JSONobject because of how javascript retrieves the value
			JSONArray coordinates = fcMap.getJSONArray("result");
			System.out.println(coordinates.length());
			assertNotNull(coordinates);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	/** produces xy coordinates of each FC on the network. 
	 *  tests if the result is empty or if it contains the coordinates/values
	 *  calls via agentcaller rather than user
	 */
	public void testFCQueryAgent(){
		JSONObject jo = new JSONObject().put("wastenetwork",
				"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		try {
			String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_WTE/WTEVisualization/createMarkers", jo.toString());
			System.out.println(resultStart);
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	/**
	 * returns only one entry (OnsiteWTF#0) in base case
	 * if it returns more than one, then it's probably modified. restore from base case your WasteNetwork OWL
	 */
	public void testOnsiteDirect(){ 
		// OnSiteWasteTreatment-0
		WTEVisualization a = new WTEVisualization();
		JSONObject jo = new JSONObject();
		OntModel model = WastetoEnergyAgent.readModelGreedy(iriofnetwork);
		try {
			String result = a.searchOnsite(model, jo);
			assertNotNull(result);
			JSONObject fcMap = new JSONObject(result);
			assertTrue(fcMap.has("result"));
			//result has to be a string and not a JSONobject because of how javascript retrieves the value
			JSONArray coordinates = fcMap.getJSONArray("result");
			assertEquals(coordinates.length(),1);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	/**
	 * returns only one entry (OnsiteWTF#0) in base case
	 * if it returns more than one, then it's probably modified. restore from base case. 
	 */
	public void testOnsiteQueryAgent(){
		JSONObject jo = new JSONObject().put("wastenetwork",
				"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		try {
			String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_WTE/WTEVisualization/queryOnsite", jo.toString());
			System.out.println(resultStart);
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	/** returns tech outputs, separated into JSON Objects
	 * {onsite:[tax, installationcost, operationcost, manpowercost], offsite:[]}
	 * 
	 */
	public void testreadInputsDirect(){
		WTEVisualization a = new WTEVisualization();
		try {
		OntModel model = WastetoEnergyAgent.readModelGreedy(iriofnetwork);
		String g = a.readInputs(model);
		JSONObject jo = new JSONObject(g);
		System.out.println(g);

		assertNotNull(g);
		assertTrue(jo.has("offsite"));
		assertTrue(jo.has("onsite"));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	public void testreadInputsAgent(){
		JSONObject jo = new JSONObject().put("wastenetwork",
				"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		try {
			String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_WTE/WTEVisualization/readInputs", jo.toString());
			System.out.println(resultStart);
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
}
