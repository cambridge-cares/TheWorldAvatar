package uk.ac.cam.cares.jps.building.test;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
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
		
		String city = BuildingQueryPerformer.THE_HAGUE_IRI;
		String plant = "http://www.theworldavatar.com/Plant-001.owl";
		int buildingLimit = 25;
		
		double plantx = 79831;
		double planty = 454766;
		double lowerx = plantx - 100;
		double lowery = planty - 100;
		double upperx = plantx + 100;
		double uppery = planty + 200;

		String targetFolder = startIntegrationWithPython(city, plant, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery);
		//long delta = System.currentTimeMillis() - GetLastModifiedTime(targetFolder, "test.apl");
		//assertTrue(delta <= 1000*60);
	}
	
	public void testBerlinIntegrationWithPython() throws InterruptedException {
		
		String cityIRI = BuildingQueryPerformer.BERLIN_IRI;
		String plantIRI = "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002";
		int buildingLimit = 25;
		
		// transform the points around the plant in Berlin into the CRS of GUI, BuildingQueryPerformer will translate them back to Berlin CRS
		// this transformed target plants are the correct ones for ADMS Python scripts
		String sourceCRS = CRSTransformer.EPSG_25833; // Berlin
		double[] sourceCenter = new double[]{392825, 5819122};
		String targetCRS = CRSTransformer.EPSG_28992; // The Hague
		double[] targetCenter = CRSTransformer.transform(sourceCRS, targetCRS, sourceCenter);

		double plantx = targetCenter[0];
		double planty = targetCenter[1];
//		double lowerx = plantx - 400;
//		double lowery = planty - 400;
//		double upperx = plantx + 400;
//		double uppery = planty + 400;
		
		double lowerx = 699208.47;
		double lowery = 533059.02;
		double upperx = 699959.88;
		double uppery = 533841.67;
		String targetFolder = startIntegrationWithPython(cityIRI, plantIRI, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery);
		//long delta = System.currentTimeMillis() - GetLastModifiedTime(targetFolder, "test.apl");
		//assertTrue(delta <= 1000*60);
		// TODO-AE assert statement is missing here 
	}
	
	private String startIntegrationWithPython(String cityIRI, String plantIRI, double plantx, double planty, int buildingLimit, double lowerx, double lowery, double upperx, double uppery) throws InterruptedException {
		
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsMainNew.py"); 
		args.add(plantIRI);
		
		String coordintates = getCoordinatesForPython(lowerx, lowery, upperx, uppery);
		args.add(coordintates);
		
		String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";
		args.add(fullPath); // this extra parameter tells the python script where to put the input files
		
		String buildingData = retrieveBuildingDataInJSON(cityIRI, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery);
		buildingData = buildingData.replace('\"', '\'');
		args.add(buildingData);
		System.out.println("building data =  \n" + buildingData);
		
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		System.out.println(targetFolder);
		String result = CommandHelper.executeCommands(targetFolder, args);
		return targetFolder;
	}	
	
	private String retrieveBuildingDataInJSON(String cityIRI, double plantx, double planty, int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		// TODO-AE URGENT URGENT activate the query for closest buildings from Region
		//List<String> buildingIRIs = createQueryPerformerForTheHague().performQueryBuildingsFromRegion(cityIRI , buildingLimit, lowerx, lowery, upperx, uppery);
		List<String> buildingIRIs = createQueryPerformerForTheHague().performQueryClosestBuildingsFromRegion(cityIRI, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery);
		SimpleBuildingData result = createQueryPerformerForTheHague().performQuerySimpleBuildingData(cityIRI, buildingIRIs);
		String argument = new Gson().toJson(result);
		return argument;
	}
	
	private String getCoordinatesForPython(double lowerx, double lowery, double upperx, double uppery) {
		String template = "{'xmin':%f, 'xmax':%f, 'ymin':%f, 'ymax':%f}";
		return String.format(template, lowerx, upperx, lowery, uppery);
	}
	
	public long GetLastModifiedTime(String targetFolder, String fileName) {
        File f = new File(targetFolder + "/" + fileName);
        return f.lastModified();
	}
}
