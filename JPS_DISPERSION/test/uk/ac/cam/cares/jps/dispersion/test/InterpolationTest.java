package uk.ac.cam.cares.jps.dispersion.test;

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
		String baseUrl= QueryBroker.getLocalDataPath();
		InterpolationAgent ag = new InterpolationAgent();
		String coordinates = "[380000 150000 0]";
		String gasType = "['NO NO2']";
		String options = "1";
		String dispMatrix = "3D_instantanous_mainconc_center.dat";
		ag.copyTemplate(baseUrl,"3D_instantanous_mainconc_center.dat");
		ag.copyTemplate(baseUrl, "virtual_sensor.m");
	
		try {
			ag.createBat(baseUrl, coordinates,gasType, options,dispMatrix );
			ag.runModel(baseUrl);
		}catch (Exception e) {
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
	//test determineGas
	public void testdetermineGas() {
		System.out.println(new InterpolationAgent().determineGasGst("C:\\Users\\ongajong\\Downloads\\JPS_ADMS\\JPS_ADMS"));
	}
	//test getLastModifiedDirectory
	public void testAddMetadataAnnotator() {
		String baseUrl = QueryBroker.getLocalDataPath();//folder baseUrl should be // and not \\
		//expect baseUrl to be returned
		String agent = "http://www.theworldavatar.com/kb/agents/Service__ComposedEpisode.owl#Service";
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
}