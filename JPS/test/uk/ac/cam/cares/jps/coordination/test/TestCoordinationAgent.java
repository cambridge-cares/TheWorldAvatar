package uk.ac.cam.cares.jps.coordination.test;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestCoordinationAgent extends TestCase {

	/**
	 *This test does not write a new apl file because an exception occurs in admsInputDataRetrieverNew.py
	 *	 *
	 *INFO [Python] admsTest.py find closed bdn for src: http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002 with x: 699583.49 y: 532938.392018-11-19 12:46:14,719 
	 *ERROR [Python] admsTest.py Dear lord, no closed buildinf found for ...
	 *
	 *Compare this with the log result of testCoordinationAgentWithoutCompositionForBerlinWithEPSG28992Coordinates 
	 *INFO [Python] admsTest.py find closed bdn for src: http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002 with x: 699583.49 y: 532938.392018-11-19 
	 *INFO [Python] admsTest.py path for adms met file=C://JPS_DATA/workingdir/JPS/ADMS/test.met2018-11-19 12:45:19,270 
	 *INFO [Python] admsTest.py calling admsAplWirter ...
	 * 
	 * 
	 * Cause: The region coordinates (in WSG94) are not adapted to the power plant coordinates:
	 * 
	 * INFO [http-nio-8080-exec-6] ADMSAgent retrieveBuildingDataInJSON, city=http://dbpedia.org/resource/Berlin, plantx=699582.7165141392, planty=532937.7313147049, lowerx=13.414204, lowery=52.508493, upperx=13.427214, uppery=52.514838
	 * 
	 * 
	 * 
	 * @throws JSONException
	 */
	public void testCoordinationAgentWithoutCompositionForBerlinWithWSG94Coordinates() throws JSONException {
						
		JSONWriter jsonInput = new JSONStringer().object().
				key("region").object()
					.key("srsname").value("EPSG:4326")
					.key("lowercorner").object()
						.key("lowerx").value("13.414204")
						.key("lowery").value("52.508493").endObject()
					.key("uppercorner").object()
						.key("upperx").value("13.427214")
						.key("uppery").value("52.514838").endObject()
				.endObject()
				.key("plant").value("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002")
				.endObject(); 
		
		// direct call without AgentCaller
		//ADMSCoordinationAgentWithoutComposition agent = new ADMSCoordinationAgentWithoutComposition();
		//JSONObject result = agent.executeWithoutComposition(jsonInput.toString());
		
		String result = AgentCaller.executeGetWithJsonParameter("/JPS/ADMSCoordinationAgentWithoutComposition", jsonInput.toString());
		
		System.out.println(result);
		
		JSONObject jo = new JSONObject(result);
		String city = jo.getString("city");
		assertEquals("http://dbpedia.org/resource/Berlin", city);
		JSONObject hasWind = jo.getJSONObject("weatherstate").getJSONObject("haswind");
		assertNotNull(hasWind);
		JSONArray building = jo.getJSONArray("building");
		assertNotNull(building);
		assertEquals(25, building.length());
	}
	
	public void testCoordinationAgentWithoutCompositionForBerlinWithEPSG28992Coordinates() throws JSONException {
		
		JSONWriter jsonInput = new JSONStringer().object().
				key("region").object()
					.key("srsname").value("EPSG:28992")
					.key("lowercorner").object()
						.key("lowerx").value("699182")
						.key("lowery").value("532537").endObject()
					.key("uppercorner").object()
						.key("upperx").value("699983")
						.key("uppery").value("533338").endObject()
				.endObject()
				.key("plant").value("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002")
				.endObject(); 
				
		String result = AgentCaller.executeGetWithJsonParameter("/JPS/ADMSCoordinationAgentWithoutComposition", jsonInput.toString());
				
		JSONObject jo = new JSONObject(result);
		String city = jo.getString("city");
		assertEquals("http://dbpedia.org/resource/Berlin", city);
		JSONObject hasWind = jo.getJSONObject("weatherstate").getJSONObject("haswind");
		assertNotNull(hasWind);
		JSONArray building = jo.getJSONArray("building");
		assertNotNull(building);
		assertEquals(25, building.length());
	}
	
	public void testCoordinationAgentWithoutCompositionForTheHagueWithEPSG28992Coordinates() throws JSONException {
		
		JSONWriter jsonInput = new JSONStringer().object().
				key("region").object()
					.key("srsname").value("EPSG:28992")
					.key("lowercorner").object()
						.key("lowerx").value("79480")
						.key("lowery").value("454670").endObject()
					.key("uppercorner").object()
						.key("upperx").value("80000")
						.key("uppery").value("455190").endObject()
				.endObject()
				.key("plant").value("http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl#Plant-001")
				.endObject(); 
				
		String result = AgentCaller.executeGetWithJsonParameter("/JPS/ADMSCoordinationAgentWithoutComposition", jsonInput.toString());
		
		System.out.println(result);
		
		JSONObject jo = new JSONObject(result);
		String city = jo.getString("city");
		assertEquals("http://dbpedia.org/resource/The_Hague", city);
		JSONObject hasWind = jo.getJSONObject("weatherstate").getJSONObject("haswind");
		assertNotNull(hasWind);
		JSONArray building = jo.getJSONArray("building");
		assertNotNull(building);
		assertEquals(25, building.length());
	}
}
