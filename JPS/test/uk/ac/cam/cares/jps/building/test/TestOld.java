package uk.ac.cam.cares.jps.building.test;

import java.util.List;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;

public class TestOld extends TestCase {

	public void testTheHagueAgentBuildingsFromRegion() {
		
		String response = AgentCaller.executeGet("/JPS/buildings/fromregion", "cityiri", BuildingQueryPerformer.THE_HAGUE_IRI, "buildinglimit", "25", 
				"lowerx", "79000.", "lowery", "454000.", "upperx", "79800.", "uppery", "455200.");
		System.out.println(response);
		List<String> buildingIRIs = new Gson().fromJson(response, List.class);
		assertEquals(25, buildingIRIs.size());
	}
	
	public void testTheHagueAgentBuildingsSimpleShape() {
		
		String prefix = TestBuildingQueryPerformer.BUILDING_IRI_THE_HAGUE_PREFIX;
		
		String buildingIRIs = "[\"" + prefix + "10_buildings0.owl#BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC\","
				+ "\"" + prefix + "10_buildings0.owl#BuildingGUID_21FFA968-1D0D-46F1-9C6A-DEB511EDE8EC\","
				+ "\"" + prefix + "10_buildings0.owl#BuildingGUID_E7EAC652-9675-4075-9B77-9119130FFC01\","
				+ "\"" + prefix + "10_buildings0.owl#BuildingGUID_0DDFE8F6-C689-411B-A40B-7AB0B322DAA4\","
				+ "\"" + prefix + "10_buildings0.owl#BuildingGUID_75633FA6-1816-4681-AED1-28477B9E8306\"]";		
		
		String response = AgentCaller.executeGet("/JPS/buildings/simpleshape", "cityiri", BuildingQueryPerformer.THE_HAGUE_IRI, "buildingiris", buildingIRIs);
		System.out.println(response);
		SimpleBuildingData data = new Gson().fromJson(response, SimpleBuildingData.class);
		assertEquals(5, data.BldIRI.size());
		assertEquals(5, data.BldAngle.size());
	}
}
