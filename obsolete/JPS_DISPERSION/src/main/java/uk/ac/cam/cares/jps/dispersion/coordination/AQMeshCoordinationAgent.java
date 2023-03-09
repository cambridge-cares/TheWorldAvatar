package uk.ac.cam.cares.jps.dispersion.coordination;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet("/AQMeshCoordinationAgent")
public class AQMeshCoordinationAgent extends JPSHttpServlet  {
	Logger logger= LoggerFactory.getLogger(AQMeshCoordinationAgent.class);
	
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams) {
		
		String result = execute("/JPS_DISPERSION/AirQualitySensorAgent", requestParams.toString());
		String stationiri = new JSONObject(result).getString("airStationIRI");
		requestParams.put("airStationIRI", stationiri);
		
		String statisticcal = execute("/JPS_DISPERSION/StatisticAnalysis", requestParams.toString());
		
		return requestParams;
	}
	

}
