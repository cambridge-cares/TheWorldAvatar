package uk.ac.cam.cares.jps.discovery.client;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentResponse;
import uk.ac.cam.cares.jps.base.discovery.IAgentCommunication;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class DiscoveryProvider implements IAgentCommunication {
	
	@Override
	public List<String> searchAgents(AgentRequest searchDescr) {
		
		List<String> result = new ArrayList<String>();
		
		String serialized = new Gson().toJson(searchDescr);
		try {
			String response = AgentCaller.executeGet("/JPS_DISCOVERY/search", "agentrequest", serialized);
			
			//TODO-AE choose another separator
			StringTokenizer tokenizer = new StringTokenizer(response, " ");
			while (tokenizer.hasMoreTokens()) {	
				String token = tokenizer.nextToken();
				result.add(new String(token));
			}
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		return result;
	}

	@Override
	public AgentResponse callAgent(AgentRequest agentRequest)  {
		return AgentCaller.callAgent("/JPS_DISCOVERY/call", agentRequest);
	}
	
	@Override
	public void registerAgent(Agent agent) {
		// TODO-AE remove project dependency to JPS by moving Agentlocator etc.
		// attention:
		// a) added property for discovery agent to config properties
		// b) remove ClientProtocolException from callAgent
		// but I changed this on the branch and it has to be checked in on the main
		// branch
		// String responseRegistry = AgentLocator.callAgent("agent.discovery.registry");
		
		// TODO-AE here we still use the Java binary serializer instead of OWL
		// reason: Tests use register method and do not run as there is no deserialization
		// from OWL to Java class AgentDescription yet!
		//String serialized = serializer.convertToString(description);
		String serialized = new Gson().toJson(agent);
		try {
			AgentCaller.executeGet("/JPS_DISCOVERY/register", "agent", serialized);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	@Override
	public void deregisterAgent(String agentAddress) {
		try {
			AgentCaller.executeGet("/JPS_DISCOVERY/deregister", "agentname", agentAddress);
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.cam.cares.jps.discovery.api.IAgentCommunication#getAllAgentNames()
	 * 
	 * Only use this method for test purposes !!!
	 */
	@Override
	public List<String> getAllAgentNames() {
		
		List<String> result = new ArrayList<String>();
		
		try {
			String response = AgentCaller.executeGet("/JPS_DISCOVERY/agents");
			
			System.out.println("GETALLAGENTNAMES");
			System.out.println(response);
			
			StringTokenizer tokenizer = new StringTokenizer(response, " ");
			while (tokenizer.hasMoreTokens()) {	
				String token = tokenizer.nextToken();
				result.add(new String(token));
			}
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		return result;
	}
}
