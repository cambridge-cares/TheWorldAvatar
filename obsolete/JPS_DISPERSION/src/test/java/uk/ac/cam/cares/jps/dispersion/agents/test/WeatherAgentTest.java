package uk.ac.cam.cares.jps.dispersion.agents.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.dispersion.agents.WeatherAgent;
import uk.ac.cam.cares.jps.dispersion.sparql.WeatherStation;

public class WeatherAgentTest extends TestCase {
	
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
	
	public void testWeatherAgentCall() {
		JSONObject jo = new JSONObject();
		Region.putRegion(jo,2);
		AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/WeatherAgent", jo.getJSONObject("region").toString());
	}
}
