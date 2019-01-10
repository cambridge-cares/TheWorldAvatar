package uk.ac.cam.cares.jps.ship.test;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestGetShipListFromRegion extends TestCase {
	
	public void testGetShipListFromRegion() throws JSONException {
		String jsonArrayOfShipIRI = AgentCaller.executeGet("JPS_SHIP/GetShipListFromRegion", "query", "TEST");
//		Gson g = new Gson();
//		String[] shipIRIs = g.fromJson(jsonArrayOfShipIRI, String[].class);
		
		JSONObject jsonShipIRIs = new JSONObject(jsonArrayOfShipIRI);
		JSONArray shipIRIs = (JSONArray) jsonShipIRIs.get("shipIRIs");
		
		System.out.println(jsonArrayOfShipIRI);
		System.out.println(shipIRIs);
		
//		for(String shipIRI: shipIRIs) {
//			System.out.println(shipIRI);
//		}
		
		for (int i = 0; i < shipIRIs.length(); i++) {
			Object shipIRI = shipIRIs.get(i);
			System.out.println(shipIRI);
		}
	}
}
