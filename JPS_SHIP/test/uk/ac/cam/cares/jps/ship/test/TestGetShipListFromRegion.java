package uk.ac.cam.cares.jps.ship.test;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestGetShipListFromRegion extends TestCase { //unused as the servlet is replaced by postgresql
	
	public void testGetShipListFromRegion() throws JSONException {
		
		
		JSONWriter jsonInput = new JSONStringer().object().
		key("region").object()
			.key("srsname").value("EPSG:3857")
			.key("lowercorner").object()
				.key("lowerx").value("11558666.37")
				.key("lowery").value("139186.423").endObject()
			.key("uppercorner").object()
				.key("upperx").value("11562079.502")
				.key("uppery").value("141908.33").endObject()
		.endObject()
		.key("reactionmechanism").value("https://como.cheng.cam.ac.uk/kb/Toluene.owl#ReactionMechanism_4631074216281807")
		.endObject(); 
		
		String jsonArrayOfShipIRI = AgentCaller.executeGet("JPS_SHIP/GetShipListFromRegion", "query", jsonInput.toString());
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
