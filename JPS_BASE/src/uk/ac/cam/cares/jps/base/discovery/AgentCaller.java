package uk.ac.cam.cares.jps.base.discovery;

import java.io.BufferedReader;
import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


public class AgentCaller {
	
	private static Logger logger = LoggerFactory.getLogger(AgentCaller.class);
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
	
	// TODO-AE this method seems not to be required. 
	public static String getRequestBody(final HttpServletRequest req) {
	    final StringBuilder builder = new StringBuilder();
	    try (final BufferedReader reader = req.getReader()) {
	        String line;
	        while ((line = reader.readLine()) != null) {
	            builder.append(line);
	        }
	        return builder.toString();
	    } catch (final Exception e) {
	        return null;
	    }
	}
	
	public static AgentResponse callAgent(String contextPath, AgentRequest agentRequest)  {
		
		Gson gson = new Gson();
		
		logger.debug("callAgent start ");
		
		String serializedAgentRequest = gson.toJson(agentRequest);
		
		logger.debug("SerAgRequ " + serializedAgentRequest);
		
		try {
			String serializedAgentResponse = executeGet(contextPath, "agentrequest", serializedAgentRequest);
			
			logger.debug("SerAgResp " + serializedAgentResponse);
						
			return gson.fromJson(serializedAgentResponse, AgentResponse.class);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	public static AgentRequest getAgentRequest(HttpServletRequest req) {
		String serializedAgentRequest = req.getParameter("agentrequest");
		return new Gson().fromJson(serializedAgentRequest, AgentRequest.class);
	}
	
	public static void printToResponse(Object object, HttpServletResponse resp) {
		
		String message = new Gson().toJson(object);
		resp.setContentType("text/plain");
		resp.setCharacterEncoding("UTF-8");
		try {
			resp.getWriter().print(message);
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}	
	}
}
