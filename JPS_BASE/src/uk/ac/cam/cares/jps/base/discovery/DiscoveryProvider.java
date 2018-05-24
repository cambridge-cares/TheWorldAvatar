package uk.ac.cam.cares.jps.base.discovery;

import java.util.ArrayList;
import java.util.List;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class DiscoveryProvider {
	
	public static List<String> searchAgents(AgentRequest searchDescr) {

		String serialized = new Gson().toJson(searchDescr);
		String response = null;
		try {
			response = AgentCaller.executeGet("/JPS_DISCOVERY/search", "agentrequest", serialized);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		return new Gson().fromJson(response, ArrayList.class);
	}

	public static AgentResponse callAgent(AgentRequest agentRequest)  {
		return AgentCaller.callAgent("/JPS_DISCOVERY/call", agentRequest);
	}
	
	public static void registerAgent(Agent agent) {
		String serialized = new Gson().toJson(agent);
		try {
			AgentCaller.executeGet("/JPS_DISCOVERY/register", "agent", serialized);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	public static void deregisterAgent(String agentAddress) {
		try {
			AgentCaller.executeGet("/JPS_DISCOVERY/deregister", "agentname", agentAddress);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}

	/**
	 * Only use this method for test purposes !!!
	 * 
	 * @return
	 */
	public static List<String> getAllAgentNames() {
		
		String response = null;
		try {
			response = AgentCaller.executeGet("/JPS_DISCOVERY/agents");
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		return new Gson().fromJson(response, ArrayList.class);
	}
}
