package uk.ac.cam.cares.jps.base.discovery;

import java.util.ArrayList;
import java.util.List;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class DiscoveryProvider {
	
	public static List<String> searchAgents(AgentRequest searchDescr) {

		String serialized = convertToJson(searchDescr);
		String response = null;
		try {
			response = AgentCaller.executeGet("/JPS_DISCOVERY/search", "agentrequest", serialized);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		return convertFromJson(response, ArrayList.class);
	}

	public static AgentResponse callAgent(AgentRequest agentRequest)  {
		return AgentCallAdditionalMethods.callAgent("/JPS_DISCOVERY/call", agentRequest);
	}
	
	public static void registerAgent(Agent agent) {
		String serialized = convertToJson(agent);
		try {
			AgentCaller.executeGet("/JPS_DISCOVERY/register", "agent", serialized);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	public static void clear() {
		try {
			AgentCaller.executeGet("/JPS_DISCOVERY/clear");
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
		
		return convertFromJson(response, ArrayList.class);
	}
	
	public static String convertToJson(Object object) {
		return new Gson().toJson(object);
	}
	
	public static <T> T convertFromJson(String objectAsString, Class<T> classtype) {
		return new Gson().fromJson(objectAsString, classtype);
	}
}
