package uk.ac.cam.cares.jps.servicespool.test;

import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.servicespool.ADMSAgent;

public class TestADMSWithJSON extends TestCase {

	String myHost = "localhost";
	int myPort = 8080;
	public void test() throws JSONException {
		String regionInString = new JSONStringer().object().
				key("region").object()
					.key("lowercorner").object() //52.508287, 13.415407
						.key("lowerx").value("13.415407")
						.key("lowery").value("52.508287").endObject()
					.key("uppercorner").object()
						.key("upperx").value("13.424336") //52.511112, 13.424336
						.key("uppery").value("52.511112").endObject()
					.key("srsname").value("EPSG:4326")
				.endObject()
				.endObject().toString(); 
		
		
		JSONObject regionJSON = new JSONObject(regionInString);
		JSONObject bundle = new JSONObject();
		
		bundle.put("city", "http://dbpedia.org/resource/Berlin");
		bundle.put("plant", "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002");
		bundle.put("region", regionJSON.getJSONObject("region"));

		
		JSONObject city = new JSONObject();
		city.put("city", "http://dbpedia.org/resource/Berlin");
		 
		URIBuilder weatherbuilder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/JPS_COMPOSITION/CityToWeather")
				.setParameter("query", city.toString());
		
		String Weatherresult = executeGet(weatherbuilder);
		bundle.put("weatherstate", new JSONObject(Weatherresult).getJSONObject("weatherstate"));
		
		
		JSONObject resultInJSON = new JSONObject(Weatherresult);
		assertTrue(resultInJSON.has("weatherstate"));
		
		
	}
	
	public void testretrieveBuildingDataInJSONOLD() {
		ADMSAgent a= new ADMSAgent();
		double plantx=(476584.89+478230.04)/2;
		double planty=(6812941.68+6814587.35)/2;
		String result=a.retrieveBuildingDataInJSONOLD("http://dbpedia.org/resource/The_Hague",plantx, planty, 476584.89, 6812941.68, 478230.04, 6814587.35);
		System.out.println("result= "+result);
	}
	
	public void testretrieveBuildingDataInJSON() {
		String regionInString = new JSONStringer().object().
				key("region").object()
					.key("lowercorner").object() //52.508287, 13.415407
						.key("lowerx").value("476584.89")
						.key("lowery").value("6812941.68").endObject()
					.key("uppercorner").object()
						.key("upperx").value("478230.04") //52.511112, 13.424336
						.key("uppery").value("6814587.35").endObject()
					.key("srsname").value("EPSG:3857")
				.endObject()
				.endObject().toString(); 
		
		
		JSONObject regionJSON = new JSONObject(regionInString);
		ADMSAgent a= new ADMSAgent();
		JSONObject jo = new JSONObject();
		jo.put("city", "http://dbpedia.org/resource/The_Hague");
		jo.put("region",regionJSON.getJSONObject("region"));
		String result = AgentCaller.executeGet("http://www.theworldavatar.com:80/JPS/GetBuildingListFromRegion", "query", jo.toString());
		JSONArray building = new JSONObject(result).getJSONArray("building");
		jo.put("building", building);
		String result2=a.retrieveBuildingDataInJSON(jo);
		System.out.println("result= "+result2);
	}
	 
	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			System.out.println(uri.toASCIIString());
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
}
