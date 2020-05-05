package uk.ac.cam.cares.jps.dispersion.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.dispersion.interpolation.InterpolationAgent;

public class InterpolationTest extends TestCase{

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
	public void testAgentCallfromFrontEnd() {
		JSONObject jo = new JSONObject();
		jo.put("typ", 1);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/InterpolationAgent/startSimulation", jo.toString());
		
		
	}
}