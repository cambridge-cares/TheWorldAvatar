package uk.ac.cam.cares.jps.building.test;

import java.util.ArrayList;
import java.util.List;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;

public class TestBuildingQueryAgent extends TestCase {
	
	public static BuildingQueryPerformer createQueryPerformerForTheHague() {
		return TestBuildingQueryPerformer.createQueryPerformerForTheHague();
	}

	public void testTheHagueAgentBuildingsFromRegion() {
		
		String response = AgentCaller.executeGet("/JPS/buildings/fromregion", "cityiri", BuildingQueryPerformer.THE_HAGUE_IRI, "buildinglimit", "25", 
				"lowerx", "79000.", "lowery", "454000.", "upperx", "79800.", "uppery", "455200.");
		System.out.println(response);
		List<String> buildingIRIs = new Gson().fromJson(response, List.class);
		assertEquals(25, buildingIRIs.size());
	}
	
	public void testTheHagueAgentBuildingsSimpleShape() {
		
		String buildingIRIs = "[\"" + TestBuildingQueryPerformer.BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_83EFA0E4-FC06-46B3-8482-E38C8CF602BC\","
				+ "\"" + TestBuildingQueryPerformer.BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_21FFA968-1D0D-46F1-9C6A-DEB511EDE8EC\","
				+ "\"" + TestBuildingQueryPerformer.BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_E7EAC652-9675-4075-9B77-9119130FFC01\","
				+ "\"" + TestBuildingQueryPerformer.BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_0DDFE8F6-C689-411B-A40B-7AB0B322DAA4\","
				+ "\"" + TestBuildingQueryPerformer.BUILDING_IRI_THE_HAGUE_PREFIX + "10_buildings0.owl#BuildingGUID_75633FA6-1816-4681-AED1-28477B9E8306\"]";		
		
		String response = AgentCaller.executeGet("/JPS/buildings/simpleshape", "cityiri", BuildingQueryPerformer.THE_HAGUE_IRI, "buildingiris", buildingIRIs);
		System.out.println(response);
		SimpleBuildingData data = new Gson().fromJson(response, SimpleBuildingData.class);
		assertEquals(5, data.BldIRI.size());
		assertEquals(5, data.BldAngle.size());
	}
	
	public void testTheHagueIntegrationWithPython() throws InterruptedException {
		
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsMainNew.py"); 
		//args.add("http://www.theworldavatar.com/Plant-001.owl#Plant-001");
		args.add("http://www.theworldavatar.com/Plant-001.owl");
		//args.add(coordinates.toString().replaceAll(",", "#"));
		args.add("{'xmin':84400, 'xmax':84600, 'ymin':451000, 'ymax':451300}");
		String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";
		System.out.println("ADMS working dir =  " + fullPath);
		args.add(fullPath); // this extra parameter tells the python script where to put the input files, in
							// this case, working dir
		String buildingData = retrieveTheHagueBuildingDataInJSON();
		buildingData = buildingData.replace('\"', '\'');
		args.add(buildingData);
		System.out.println("building data =  \n" + buildingData);
		
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		String result = CommandHelper.executeCommands(targetFolder, args);
	
		System.out.println("Python result: \n" + result);
		
		// TODO-AE assert statement is missing here 
	}
	
	private String retrieveTheHagueBuildingDataInJSON() {
		List<String> buildingIRIs = createQueryPerformerForTheHague().performQueryBuildingsFromRegion(BuildingQueryPerformer.THE_HAGUE_IRI, 25, 79000., 454000., 79800., 455200.);
		SimpleBuildingData result = createQueryPerformerForTheHague().performQuerySimpleBuildingData(BuildingQueryPerformer.THE_HAGUE_IRI, buildingIRIs);
		String argument = new Gson().toJson(result);
		return argument;
	}
}
