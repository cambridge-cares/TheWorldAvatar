package uk.ac.cam.cares.jps.ship.test;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.sparql.JenaModelWrapper;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

public class TestShipAgent extends TestCase {
	
	public void testShipAgent () throws JSONException {
		JSONObject arguments = new JSONObject();
		arguments.put("reactionmechanism", "http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001");
		arguments.put("ship", "http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1");
		
		String wasteResult = AgentCaller.executeGet("/JPS_SHIP/ShipAgent", "query", arguments.toString());
		System.out.println(wasteResult);
		
		String emission = new JSONObject(wasteResult).getString("waste");
		assertEquals("http://www.theworldavatar.com/kb/ships/Chimney-1.owl#WasteStreamOfChimney-1", emission);
	}
	
	public void testCoordinationAgentWithoutCompositionForSGWithEPSG3857Coordinates() throws JSONException {
		
		JSONWriter jsonInput = new JSONStringer().object().
				key("region").object()
				.key("lowercorner").object() //52.508287, 13.415407
					.key("lowerx").value("11560879.832") // 699182 / 13.415407 // 13728088
					.key("lowery").value("140107.739").endObject() // 532537 / 52.508287 // 2281341
				.key("uppercorner").object() //52.511112, 13.424336
					.key("upperx").value("11563323.926") // 699983 / 13.424336 // 13736486
					.key("uppery").value("143305.896").endObject() // 533338 / 52.511112 // 2286829
				.key("srsname").value("EPSG:3857") // EPSG:4326
			.endObject()
				.key("agent").value("http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service")
				.key("reactionmechanism").value("http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001")
				.key("city").value("http://dbpedia.org/resource/Singapore")
				.endObject(); 
				
		String result = AgentCaller.executeGetWithJsonParameter("/JPS/GetBuildingListFromRegion", jsonInput.toString());
				
		JSONObject jo = new JSONObject(result);
		//String city = jo.getString("city");
		//assertEquals("http://dbpedia.org/resource/Berlin", city);
//		JSONObject hasWind = jo.getJSONObject("weatherstate").getJSONObject("haswind");
//		assertNotNull(hasWind);
//		JSONArray building = jo.getJSONArray("building");
//		assertNotNull(building);
//		assertEquals(25, building.length());
	}
	
	public void testRemoveSubtree() {
	    String iriOfChimney = "http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Material2_WasteStreamOfChimney-1";
	    String filePath = AgentLocator.getPathToWorkingDir(new TestShipAgent()) + "/Chimney-1.owl";
	    OntModel jenaOwlModel = ModelFactory.createOntologyModel();	
		jenaOwlModel.read(filePath);
		
	    JenaModelWrapper c= new JenaModelWrapper(jenaOwlModel, null);
	    assertEquals(52, c.removeSubtree(iriOfChimney, Prefixes.OCPMATE, "intrinsicCharacteristics"));
	}
}
