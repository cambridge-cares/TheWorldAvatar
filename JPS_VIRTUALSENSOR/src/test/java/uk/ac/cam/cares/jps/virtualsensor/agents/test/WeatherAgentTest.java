package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.virtualsensor.agents.WeatherAgent;
import uk.ac.cam.cares.jps.virtualsensor.sparql.WeatherStation;

public class WeatherAgentTest extends TestCase {
	
	/**
	 * You must have a weather station called http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation1
	 * instantiated in the triple store to run this test
	 */
	public void testUpdateWeatherStationWithAPI() {
		WeatherStation ws = new WeatherStation();
		String stationiri = "http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#weatherstation1";
		ws.setStationiri(stationiri);
		ws.setXcoord(114);
		ws.setYcoord(22.47);
		ws.setTimestamp(0);
		WeatherAgent wa = new WeatherAgent();
		wa.updateWeatherStationWithAPI(ws);
	}
	
	/**
	 * This will query for weather stations within the scope defined by the region class
	 */
	public void testWeatherAgentCall() {
		JSONObject jo = new JSONObject();
		Region.putRegion(jo,2);
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/WeatherAgent", jo.getJSONObject("region").toString());
	}
}
