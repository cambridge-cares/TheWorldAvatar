package uk.ac.cam.cares.jps.base.router;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;

/**
 * Agent Caller class to send HTTP requests to an agent with a given "agentID" (name).
 * The agent url is retrieved by the {@link uk.ac.cam.cares.jps.base.router.AgentRouter AgentRouter}. 
 * 
 * @author csl37
 *
 */
public class AgentCaller {

	private static final Logger LOGGER = LogManager.getLogger(AgentCaller.class);

	/**
	 * Execute HTTP GET request on agentID with JSON parameters
	 * @param agentID
	 * @param jsonParameters
	 * @return
	 */
	public String getWithJson(String agentID, JSONObject jsonParameters) {
		
		String agentUrl = getAgentUrl(agentID);
		
		try {
			LOGGER.info("Execute HTTP GET with JSON parameters.");
			HttpGet request = Http.get(agentUrl, MediaType.APPLICATION_JSON.type, jsonParameters);
			return Http.execute(request);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}		
	}

	/**
	 *  Execute HTTP POST request on agentID with JSON contentType and accept
	 * @param agentID
	 * @param body
	 * @return
	 */
	public String postWithJson(String agentID, JSONObject jsonParameters) {
		LOGGER.info("Execute HTTP POST with JSON parameters.");
		return post(agentID, jsonParameters.toString(), MediaType.APPLICATION_JSON.type, MediaType.APPLICATION_JSON.type);
	}
	
	/**
	 * Execute HTTP POST request on agentID
	 * @param agentID
	 * @param body
	 * @param contentType {@link uk.ac.cam.cares.jps.base.discovery.MediaType see MediaType}
	 * @param accept {@link uk.ac.cam.cares.jps.base.discovery.MediaType see MediaType}
	 * @return
	 */
	public String post(String agentID, String body, String contentType, String accept) {
		
		String agentUrl = getAgentUrl(agentID);
		
		try {
			LOGGER.info("Execute HTTP POST.");
			HttpPost request = Http.post(agentUrl, body, contentType, accept);
			return Http.execute(request);	
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}

	/**
	 * Get URL from AgentRouter
	 * @param agentID
	 * @return
	 */
	public String getAgentUrl(String agentID) {
		return AgentRouter.getInstance().get(agentID);
	}
}
