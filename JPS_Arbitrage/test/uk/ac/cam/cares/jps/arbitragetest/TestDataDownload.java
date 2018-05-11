package uk.ac.cam.cares.jps.arbitragetest;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import junit.framework.TestCase;


public class TestDataDownload extends TestCase{

	
	public static String executeGet(String path, String key, String value)
			throws ParseException, IOException, URISyntaxException {
		// TODO-AE change localhost, maybe use directly class java.net.URI, maybe move this class to JPS_BASE
		URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost:8080")
				.setPath(path)
				.setParameter(key, value);
		return executeGet(builder);
	}	
		
	private static String executeGet(URIBuilder builder)
			throws ParseException, IOException, URISyntaxException {
		
		HttpGet request = new HttpGet(builder.build());
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		return EntityUtils.toString(httpResponse.getEntity());
	}

	public void testDoGetHttpServletRequestHttpServletResponse() throws URISyntaxException, ClientProtocolException, IOException {
		String path = "/JPS_Arbitrage/download";
		String key = "CPO_page";
		String value = "http://www.cmegroup.com/trading/agricultural/grain-and-oilseed/usd-malaysian-crude-palm-oil-calendar.html?optionProductId=8075";
		String actual = executeGet(path,key,value);
		System.out.println(actual);
		assertTrue(actual.contains("JAN 2021,FEB 2021,MAR 2021,APR 2021"));

	}
}
