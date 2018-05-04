package uk.ac.cam.cares.jps.discovery.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

public class Helper {
	
	public static String executeGet(String path, String key, String value)
			throws ParseException, IOException, URISyntaxException {
		// TODO-AE change localhost

		URIBuilder builder = new URIBuilder().setScheme("http").setHost("localhost:8080")
				.setPath(path)
				.setParameter(key, value);

		HttpGet request = new HttpGet(builder.build());

		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		return EntityUtils.toString(httpResponse.getEntity());
	}
	
	public static String getRequestBody(final HttpServletRequest request) {
	    final StringBuilder builder = new StringBuilder();
	    try (final BufferedReader reader = request.getReader()) {
	        String line;
	        while ((line = reader.readLine()) != null) {
	            builder.append(line);
	        }
	        return builder.toString();
	    } catch (final Exception e) {
	        return null;
	    }
	}
	
	public static UUID createUUID() {
		return UUID.randomUUID();
	}
}
