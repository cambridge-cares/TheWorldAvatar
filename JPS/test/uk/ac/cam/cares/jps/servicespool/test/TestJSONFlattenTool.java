package uk.ac.cam.cares.jps.servicespool.test;

import static org.junit.Assert.assertEquals;

import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONException;
import org.json.JSONStringer;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TestJSONFlattenTool {
	public String myHost = "localhost";
	public int myPort = 8080;
	@Test
	public void test() throws JSONException {
		String input = new JSONStringer().object().
				key("region").object()
					.key("lowercorner").object()
						.key("lowerx").value("13.4074096")
						.key("lowery").value("52.5177665").endObject()
					.key("uppercorner").object()
						.key("upperx").value("13.4075")
						.key("uppery").value("52.5178").endObject()
					.key("srsname").value("EPSG:4326")
				.endObject()
				.endObject().toString(); // Berlin in google coordinate system
 
		
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/JPS/RegionToCity")
				.setParameter("query", input);
		String plants  = executeGet(builder);
		System.out.println(plants);
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
	
	@Test
	public void testGetCitySingapore() throws JSONException {
		String input = new JSONStringer().object().
				key("region").object()
					.key("lowercorner").object()
						.key("lowerx").value("13728088")
						.key("lowery").value("2281341").endObject()
					.key("uppercorner").object()
						.key("upperx").value("13736486")
						.key("uppery").value("2286829").endObject()
					.key("srsname").value("EPSG:28992")
				.endObject()
				.endObject().toString(); // Berlin in google coordinate system
 
		
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/JPS/RegionToCity")
				.setParameter("query", input);
		String city  = executeGet(builder);
		assertEquals("{\"city\":\"http://dbpedia.org/resource/Singapore\"}", city);
	}
}
