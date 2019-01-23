package uk.ac.cam.cares.jps.composition.test;

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

public class TestCityToWeather {
	public String myHost = "localhost";
	public int myPort = 8080;
	@Test
	public void test() throws JSONException {
		
		String objectString = new JSONStringer().object().
				key("weatherstate").object()
					.key("hashumidity").object() //52.508287, 13.415407
						.key("hasvalue").value("x").endObject()
					.key("hasexteriortemperature").object()
						.key("hasvalue").value("x").endObject()
					.key("haswind").object()
						.key("hasspeed").value("x")
						.key("hasdirection").value("x").endObject()	
					.key("hascloudcover").object()
						.key("hascloudcovervalue").value("x").endObject()
					.key("hasweathercondition").value("x")			
					.key("hasprecipation").object()
						.key("hasintensity").value("x").endObject()
			.endObject().endObject().toString(); 
		System.out.println(objectString);
		
		
		JSONObject input = new JSONObject();
		input.put("city", "http://dbpedia.org/resource/Singapore");
		

		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/JPS_COMPOSITION/CityToWeather")
				.setParameter("query",input.toString());
		String result = executeGet(builder);
		System.out.println("Result:" + result);
	
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
