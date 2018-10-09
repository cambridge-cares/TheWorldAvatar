package uk.ac.cam.cares.jps.servicespool.test;

import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TestADMSWithJSON {

	String myHost = "localhost";
	int myPort = 8080;
	@Test
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
				.setParameter("value", city.toString());
		
		String Weatherresult = executeGet(weatherbuilder);
		bundle.put("weatherstate", new JSONObject(Weatherresult).getJSONObject("weatherstate"));
		
		
		System.out.println(Weatherresult);
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/JPS/ADMSAgent")
				.setParameter("value", bundle.toString());
		
		String ADMSresult = executeGet(builder);
		
		
	}
	 
	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
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
