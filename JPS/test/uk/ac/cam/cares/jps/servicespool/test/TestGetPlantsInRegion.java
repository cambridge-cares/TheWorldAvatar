package uk.ac.cam.cares.jps.servicespool.test;

import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TestGetPlantsInRegion {

	@Test
	public void test() {


		String myHost = "localhost";
		int myPort = 8080;
		String ADMSAgentPath = "/JPS/GetPlantsInRegion";
		//===========================================================================
		String region = "{\"region\":{\"lowercorner\":{\"lowerx\":\"52.507849\",\"lowery\":\"13.412299\"},\"uppercorner\":{\"upperx\":\"52.512977\",\"uppery\":\"13.423577\"},\"srsname\":\"EPSG:4326\"}}";
		
		 //52.507849, 13.412299 lower 
		// 52.512977, 13.423577 uppper
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(ADMSAgentPath)
				.setParameter("value", region);
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
