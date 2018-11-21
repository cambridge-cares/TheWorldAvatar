package uk.ac.cam.cares.jps.building.test;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;

public class TestBuildingQueryAgent extends TestCase {
	

	
	public void testTheHagueIntegrationWithPython() throws InterruptedException {
		
		String city = BuildingQueryPerformer.THE_HAGUE_IRI;
		String plant = "http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl";
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
		startIntegrationWithPython(cityIRI, plantIRI, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery);
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
		return CommandHelper.executeCommands(targetFolder, args);
	}	
	
	private String retrieveBuildingDataInJSON(String cityIRI, double plantx, double planty, int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		List<String> buildingIRIs = new BuildingQueryPerformer().performQueryClosestBuildingsFromRegion(cityIRI, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery);
		SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(cityIRI, buildingIRIs);
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
