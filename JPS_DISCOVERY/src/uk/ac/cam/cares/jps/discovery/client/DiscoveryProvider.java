package uk.ac.cam.cares.jps.discovery.client;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.http.ParseException;

import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentResponse;
import uk.ac.cam.cares.jps.base.discovery.IAgentCommunication;
import uk.ac.cam.cares.jps.base.discovery.TypeString;
import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;
import uk.ac.cam.cares.jps.discovery.util.Helper;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;
import uk.ac.cam.cares.jps.discovery.util.JavaSerializer;

public class DiscoveryProvider implements IAgentCommunication {

	
	private ISerializer serializer = DiscoveryFactory.getSerializer();
	
	@Override
	public List<TypeString> searchAgents(AgentRequest searchDescr) {
		
		List<TypeString> result = new ArrayList<TypeString>();
		
		String serialized = serializer.convertToString(searchDescr);
		try {
			String response = Helper.executeGet("/JPS_DISCOVERY/search", "agentrequest", serialized);
			
			//TODO-AE choose another separator
			StringTokenizer tokenizer = new StringTokenizer(response, " ");
			while (tokenizer.hasMoreTokens()) {	
				String token = tokenizer.nextToken();
				result.add(new TypeString(token));
			}
			
		} catch (ParseException | IOException | URISyntaxException e) {
			// TODO-AE throws further?
			e.printStackTrace();
			result = null;
		}
		
		return result;
	}

	@Override
	public AgentResponse callAgent(AgentRequest agentRequest)  {
		
		String serializedAgentRequest = serializer.convertToString(agentRequest);
		try {
			String serializedAgentResponse = Helper.executeGet("/JPS_DISCOVERY/call", "agentrequest", serializedAgentRequest);
			return serializer.<AgentResponse>convertFrom(serializedAgentResponse).get();
		} catch (ParseException | IOException | URISyntaxException e) {
			// TODO-AE throws further?
			e.printStackTrace();
		}
		
		return null;
	}
	
	@Override
	public void registerAgent(Agent agent) throws IOException {
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
		String serialized = new JavaSerializer().convertToString(agent);
		try {
			Helper.executeGet("/JPS_DISCOVERY/register", "agent", serialized);
		} catch (ParseException | IOException | URISyntaxException e) {
			// TODO-AE throws further?
			e.printStackTrace();
		}
	}
	
	@Override
	public void deregisterAgent(TypeString agentAddress) throws IOException {
		try {
			Helper.executeGet("/JPS_DISCOVERY/deregister", "agentname", agentAddress.getValue());
		} catch (ParseException | IOException | URISyntaxException e) {
			// TODO-AE throws further?
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.cam.cares.jps.discovery.api.IAgentCommunication#getAllAgentNames()
	 * 
	 * Only use this method for test purposes !!!
	 */
	@Override
	public List<TypeString> getAllAgentNames() {
		
		List<TypeString> result = new ArrayList<TypeString>();
		
		try {
			String response = Helper.executeGet("/JPS_DISCOVERY/agents");
			
			System.out.println("GETALLAGENTNAMES");
			System.out.println(response);
			
			StringTokenizer tokenizer = new StringTokenizer(response, " ");
			while (tokenizer.hasMoreTokens()) {	
				String token = tokenizer.nextToken();
				result.add(new TypeString(token));
			}
		} catch (ParseException | IOException | URISyntaxException e) {
			// TODO-AE throws further?
			e.printStackTrace();
		}
		
		return result;
	}
}
