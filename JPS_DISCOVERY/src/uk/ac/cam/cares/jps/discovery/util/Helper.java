package uk.ac.cam.cares.jps.discovery.util;

import java.io.BufferedReader;

import javax.servlet.http.HttpServletRequest;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class Helper {
	
	private static String hostPort = null;
	
	private static synchronized String getHostPort() {
		if (hostPort == null) {
			hostPort = AgentLocator.getProperty("host") + ":" + AgentLocator.getProperty("port");
		}
		return hostPort;
	}
	
	public static String executeGet(String path) {
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(getHostPort())
				.setPath(path);
		return executeGet(builder);
	}
	
	public static String executeGet(String path, String key, String value) {
		// TODO-AE maybe use directly class java.net.URI, maybe move this class to JPS_BASE
		// TODO-AE refactor get hostname
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(getHostPort())
				.setPath(path)
				.setParameter(key, value);
		try {
			return executeGet(builder);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}	
		
	private static String executeGet(URIBuilder builder) {
		try {
			HttpGet request = new HttpGet(builder.build());
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
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
}
