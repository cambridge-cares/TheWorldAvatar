package uk.ac.cam.cares.jps.building.test;

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
		
		//args.add("http://www.theworldavatar.com/Plant-001.owl#Plant-001");
		String plant = "http://www.theworldavatar.com/Plant-001.owl";
		//args.add(coordinates.toString().replaceAll(",", "#"));
		//args.add("{'xmin':84400, 'xmax':84600, 'ymin':451000, 'ymax':451300}");
		//args.add("{'xmin':79770, 'xmax':79910, 'ymin':454680, 'ymax':454980}");
		double lowerx = 79770;
		double lowery = 454680;
		double upperx = 79910;
		double uppery = 454980;
		
		int buildingLimit = 25;
		
		startIntegrationWithPython(city, plant, lowerx, lowery, upperx, uppery, buildingLimit);
		
		// TODO-AE assert statement is missing here 
	}
	
	public void testBerlinIntegrationWithPython() throws InterruptedException {
		
		String city = BuildingQueryPerformer.BERLIN_IRI;
		String plant = "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002";
		
		// transform the points around the plant in Berlin into the CRS of GUI, BuildingQueryPerformer will translate them back to Berlin CRS
		// this transformed target plants are the correct ones for ADMS Python scripts
		String sourceCRS = CRSTransformer.EPSG_25833; // Berlin
		double[] sourcePoints = new double[]{390000., 5815000., 396000., 5826000.};
		String targetCRS = CRSTransformer.EPSG_28992; // The Hague
		double[] targetPoints = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoints);
		
		double lowerx = targetPoints[0];
		double lowery = targetPoints[1];
		double upperx = targetPoints[2];
		double uppery = targetPoints[3];
		
		int buildingLimit = 25;
		
		startIntegrationWithPython(city, plant, lowerx, lowery, upperx, uppery, buildingLimit);
		
		// TODO-AE assert statement is missing here 
	}
	
	private void startIntegrationWithPython(String city, String plant, double lowerx, double lowery, double upperx, double uppery, int buildingLimit) throws InterruptedException {
		
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsMainNew.py"); 
		args.add(plant);
		
		String coordintates = getCoordinatesForPython(lowerx, lowery, upperx, uppery);
		args.add(coordintates);
		
		String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";
		args.add(fullPath); // this extra parameter tells the python script where to put the input files
		
		String buildingData = retrieveBuildingDataInJSON(city, lowerx, lowery, upperx, uppery, buildingLimit);
		buildingData = buildingData.replace('\"', '\'');
		args.add(buildingData);
		System.out.println("building data =  \n" + buildingData);
		
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		String result = CommandHelper.executeCommands(targetFolder, args);
	
		System.out.println("Python result: \n" + result);
	}
	
	private String retrieveBuildingDataInJSON(String city, double lowerx, double lowery, double upperx, double uppery, int buildingLimit) {
		List<String> buildingIRIs = createQueryPerformerForTheHague().performQueryBuildingsFromRegion(city , buildingLimit, lowerx, lowery, upperx, uppery);
		SimpleBuildingData result = createQueryPerformerForTheHague().performQuerySimpleBuildingData(city, buildingIRIs);
		String argument = new Gson().toJson(result);
		return argument;
	}
	
	private String getCoordinatesForPython(double lowerx, double lowery, double upperx, double uppery) {
		String template = "{'xmin':%f, 'xmax':%f, 'ymin':%f, 'ymax':%f}";
		return String.format(template, lowerx, upperx, lowery, uppery);
	}
}
