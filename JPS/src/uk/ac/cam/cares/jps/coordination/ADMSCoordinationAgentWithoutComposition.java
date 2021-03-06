package uk.ac.cam.cares.jps.coordination;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

@WebServlet("/ADMSCoordinationAgentWithoutComposition")
public class ADMSCoordinationAgentWithoutComposition extends HttpServlet {

	private static final long serialVersionUID = -2264681360832342804L;
	Logger logger = LoggerFactory.getLogger(ADMSCoordinationAgentWithoutComposition.class);

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		String jsonInput = AgentCaller.readJsonParameter(request).toString();		
		JSONObject result = executeWithoutComposition(jsonInput);
		AgentCaller.writeJsonParameter(response, result);
	}
	
	public JSONObject executeWithoutComposition(String jsonInput) {
		
		try {
			JSONObject jo = new JSONObject(jsonInput);
			

//			String plantresult = execute("/JPS/GetPlantsInRegion", jsonInput);
//			String plant=new JSONObject(plantresult).getString("plant");
//			System.out.println("result from coordination= "+new JSONObject(plantresult).getString("plant"));
//			jo.put("plant", plant);
			
			String wasteresult = execute("/JPS/PowerPlant", jo.toString());
			String waste=new JSONObject(wasteresult).getString("waste");
			jo.put("waste", waste);
						
			String regionToCityResult = execute("/JPS/RegionToCity", jsonInput);
			String city = new JSONObject(regionToCityResult).getString("city");
			jo.put("city", city);
			
			String result = execute("/JPS/GetBuildingListFromRegion", jo.toString());
			JSONArray building = new JSONObject(result).getJSONArray("building");
			jo.put("building", building);
			
			result = execute("/JPS_COMPOSITION/CityToWeather", regionToCityResult);
			JSONObject weatherstate = new JSONObject(result).getJSONObject("weatherstate");
			jo.put("weatherstate", weatherstate);
			
			result = execute("/JPS/ADMSAgent", jo.toString());
			String folder = new JSONObject(result).getString("folder");
			jo.put("folder", folder);
			
			return jo;
			
		} catch (JSONException e) {
			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	protected String execute(String path, String jsonInput) {

		logger.info("execute for path=" + path + ", json=" + jsonInput);
		String result = AgentCaller.executeGet(path, "query", jsonInput);
		logger.info("execution result=" + result);
		return result;
	}
}
