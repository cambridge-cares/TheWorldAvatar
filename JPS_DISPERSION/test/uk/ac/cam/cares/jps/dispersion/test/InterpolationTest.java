package uk.ac.cam.cares.jps.dispersion.test;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.dispersion.interpolation.InterpolationAgent;

public class InterpolationTest extends TestCase{
	//test simulation
	public void testepisoderunTestinSequenceDirect() {
		InterpolationAgent ag = new InterpolationAgent();
		String baseUrl= QueryBroker.getLocalDataPath()+"/JPS_DIS";
		String coordinates = "[30207.15 26784.95 0]";
		String gasType, dispMatrix ="";
		String agentiri = "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service";
		String location = "http://dbpedia.org/resource/Singapore";
		String directoryFolder = ag.getLastModifiedDirectory(agentiri, location);
		String[] arrayFile = ag.finder(directoryFolder);
		String fGas = arrayFile[0];
		File lstName = new File(directoryFolder, fGas);//fGas is the name of the test.levels.gst
		if (lstName.getName().endsWith(".dat")) {
			ArrayList<String> gsType = ag.determineGas(directoryFolder, lstName);
			gasType = gsType.get(0);
			String fileName = gsType.get(1);		
			dispMatrix = ag.copyOverFile(baseUrl,fileName);//to be swapped with copyOverFile
		}
		else {
			ArrayList<String> gsType = ag.determineGasGst(directoryFolder, lstName);
			gasType = gsType.get(0);
			String fileName = gsType.get(1);
			dispMatrix = ag.rearrangeGst(baseUrl, fileName, gasType);
			
		}
		ag.copyTemplate(baseUrl, "virtual_sensor.m");
		//modify matlab to read 
			try {
				ag.createBat(baseUrl, coordinates,gasType, "1", dispMatrix);
				ag.runModel(baseUrl);
	           
			} catch (Exception e) {
				e.printStackTrace();
			}
		 }
	
	//test processRequestParameters
	public void testAgentCallfromFrontEnd() {
		JSONObject jo = new JSONObject();
		jo.put("agentiri","http://www.theworldavatar.com/kb/agents/Service__ComposedEpisode.owl#Service");
		jo.put("options","1");
		jo.put("coordinates","[364638.312 131904.703 0]");
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/InterpolationAgent/startSimulation", jo.toString());	
	}
	public void testAgentCallfromFrontEndADMS() {
		JSONObject jo = new JSONObject();
		jo.put("agentiri","http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		jo.put("options","1");
		jo.put("coordinates","[32124.80 26825.16, 0]");//reminder! They don't work with the same sort of coordinates!
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/InterpolationAgent/startSimulation", jo.toString());	
	}
	//test determineGas
	public void testdetermineGas() {
		System.out.println(new InterpolationAgent()
				.determineGasGst("C:\\Users\\ongajong\\Downloads\\JPS_ADMS\\JPS_ADMS",
						new File("C:\\Users\\ongajong\\Downloads\\JPS_ADMS\\JPS_ADMS","test.levels.gst")));
	}
	//test getLastModifiedDirectory
	public void testAddMetadataAnnotator() {
		String baseUrl = QueryBroker.getLocalDataPath();//folder baseUrl should be // and not \\
		//expect baseUrl to be returned
		String agent = "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS#Service";
		String location = "http://dbpedia.org/resource/Singapore";
		List<String> lst = new ArrayList<String>();
		lst.add(location);
		MetaDataAnnotator.annotate(baseUrl, null, agent, true, lst);
		assertEquals(new InterpolationAgent().getLastModifiedDirectory(agent, location), baseUrl);
	}
	//test copyOverFile
	public void testcopyOverFile() {
		System.out.println(new InterpolationAgent().copyOverFile("C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\ddfd101b-33ca-4511-82f2-1f4fa48f4ee8\\JPS_DIS",
				"C://Users//ongajong//JParkSimulator-git//JPS_DISPERSION//workingdir//3D_instantanous_mainconc_center.dat"));
	}
	public void testgetLastModified() {
		System.out.println(new InterpolationAgent().getLastModifiedDirectory("http://www.theworldavatar.com/kb/agents/Service__ComposedEpisode.owl#Service",
				"http://dbpedia.org/resource/Singapore"));
	}
	public void testrearrangeGst() {
		System.out.println(new InterpolationAgent().rearrangeGst("C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\f031dc2a-a8a2-48ab-ab85-270d07e8c08a\\JPS_DIS",
				"C:\\Users\\ongajong\\Downloads\\JPS_ADMS\\JPS_ADMS\\test.levels.gst","['CO2 CO NO2 HC NOx SO2 O3 PM2.5-0 PM2.5-1 PM2.5-2']"));
	}
	public void testgrabCoordinates() {
		System.out.println(new InterpolationAgent().readCoordinate("http://www.theworldavatar.com/kb/sgp/singapore/SGCOSensor-001.owl#SGCOSensor-001"));
	}
}