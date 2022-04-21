package uk.ac.cam.cares.jps.base.agent;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.router.AgentRouter;

/**
 * Agent Caller class to send HTTP requests to agent given an agentID
 * 
 * @author CLIN01
 *
 */
public class AgentCaller {

	//TODO add logging
	private static final Logger LOGGER = LogManager.getLogger(AgentCaller.class);

	/**
	 * Execute HTTP GET request on agentID with JSON parameters
	 * @param agentID
	 * @param jsonParameters
	 * @return
	 */
	public String getWithJsonParameter(String agentID, JSONObject jsonParameters) {
		
		String agentUrl = getAgentUrl(agentID);
		
		try {
			HttpGet request = Http.get(agentUrl, MediaType.APPLICATION_JSON.type, jsonParameters);
			return Http.execute(request);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}		
	}

	public String post(String agentID, String body) {
		return post(agentID, body, MediaType.APPLICATION_JSON.type, MediaType.APPLICATION_JSON.type);
	}
	
	/**
	 * Execute HTTP POST request on agentID with body
	 * @param agentID
	 * @param body
	 * @return
	 */
	public String post(String agentID, String body, String contentType, String accept) {
		
		String agentUrl = getAgentUrl(agentID);
		
		try {
			HttpPost request = Http.post(agentUrl, body, contentType, accept);
			return Http.execute(request);	
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}

	private String getAgentUrl(String agentID) {
		return AgentRouter.getInstance().get(agentID);
	}
}
