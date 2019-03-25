package uk.ac.cam.cares.jps.coordination;
import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

@WebServlet("/ADMSCoordinationAgentForShipWithoutCompositionWithMocks")

public class ADMSCoordinationAgentForShipWithoutCompositionWithMocks extends ADMSCoordinationAgentForShipWithoutComposition  {
	private static final long serialVersionUID = 374231883384377111L;
	
	@Override
	protected String execute(String path, String jsonInput) {
		
		logger.info("execute for path=" + path + ", json=" + jsonInput);
		String result = null;

		if ("/JPS/RegionToCity".contentEquals(path)) {
			
			double[] xlower = new double[] {79173, 699182, 11560879, 12706653};
			String[] cities = new String[] {"http://dbpedia.org/resource/The_Hague", "http://dbpedia.org/resource/Berlin", "http://dbpedia.org/resource/Singapore", "http://dbpedia.org/resource/Hong_Kong"};
			
			JSONObject jo = new JSONObject(jsonInput);
			double selectedLowerX = jo.getJSONObject("region").getJSONObject("lowercorner").getDouble("lowerx");
		
			int closestIndex = -1;
			double closestDistance = 0;
			for (int i=0; i<xlower.length; i++) {
				double currentDistance = Math.abs(xlower[i] - selectedLowerX);
				if ((i==0) || (currentDistance < closestDistance)) {
					closestIndex = i;
					closestDistance = currentDistance;
				}
			}
					
			result = "{\"city\":\"" + cities[closestIndex] + "\"}";
			
		} else if ("/JPS_COMPOSITION/CityToWeather".contentEquals(path)) {
			
			result = "{\"weatherstate\":{\"hashumidity\":{\"hasvalue\":\"49\"},\"hasexteriortemperature\":{\"hasvalue\":\"33.18\"},\"haswind\":{\"hasspeed\":\"4.1\",\"hasdirection\":\"95\"},\"hascloudcover\":{\"hascloudcovervalue\":\"75\"},\"hasweathercondition\":\"few_clouds\",\"hasprecipation\":{\"hasintensity\":\"0.0\"}}}";
			
		} else {
			 result = AgentCaller.executeGet(path, "query", jsonInput);
		}
		
		logger.info("execution result=" + result);
		
		return result;
	}
}
