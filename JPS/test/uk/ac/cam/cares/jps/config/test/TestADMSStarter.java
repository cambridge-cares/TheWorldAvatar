package uk.ac.cam.cares.jps.config.test;

import java.net.URI;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.junit.Ignore;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TestADMSStarter {

	@Ignore
	void test() {
		
		String myHost = "www.theworldavatar.com";
		int myPort = 80;
		
		String path = "C:\\TOMCAT\\webapps\\JPS\\workingdir\\ADMS";
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("JPS/ADMSStarter")
				.setParameter("targetFolder", path);
		
		
		
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
