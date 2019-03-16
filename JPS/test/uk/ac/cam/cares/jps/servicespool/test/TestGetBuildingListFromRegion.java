package uk.ac.cam.cares.jps.servicespool.test;

import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONException;
import org.json.JSONStringer;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TestGetBuildingListFromRegion extends TestCase {
	
	public void testGetCentrePoint() throws JSONException {
		String regionInString = new JSONStringer().object().
				key("region").object()
					.key("lowercorner").object() //52.508287, 13.415407
						.key("lowerx").value("13728088") // 699182 / 13.415407 // 13728088
						.key("lowery").value("2281341").endObject() // 532537 / 52.508287 // 2281341
					.key("uppercorner").object() //52.511112, 13.424336
						.key("upperx").value("13736486") // 699983 / 13.424336 // 13736486
						.key("uppery").value("2286829").endObject() // 533338 / 52.511112 // 2286829
					.key("srsname").value("EPSG:28992") // EPSG:4326
				.endObject()
				.key("city").value("http://dbpedia.org/resource/Singapore")
//				.key("city").value("http://dbpedia.org/resource/Berlin")
//				.key("plant").value("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002")
				.endObject().toString();
		
		URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost").setPort(8080)
				.setPath("/JPS/GetBuildingListFromRegion")
				.setParameter("query", regionInString);
		
//		String regionInString = new JSONStringer().object().
//				key("region").object()
//					.key("lowercorner").object() //52.508287, 13.415407
//						.key("lowerx").value("13.415407")
//						.key("lowery").value("52.508287").endObject()
//					.key("uppercorner").object()
//						.key("upperx").value("13.424336") //52.511112, 13.424336
//						.key("uppery").value("52.511112").endObject()
//					.key("srsname").value("EPSG:4326")
//				.endObject()
//				.endObject().toString(); 
//		
//		
//		JSONObject regionJSON = new JSONObject(regionInString);
//		JSONObject bundle = new JSONObject();
//		
//		bundle.put("city", "http://dbpedia.org/resource/Berlin");
//		bundle.put("plant", "http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002");
//		bundle.put("region", regionJSON.getJSONObject("region"));
//
//		URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost").setPort(8080)
//				.setPath("/JPS/GetBuildingListFromRegion")
//				.setParameter("query", bundle.toString());
		
		String result = executeGet(builder);
		System.out.println(result);
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
