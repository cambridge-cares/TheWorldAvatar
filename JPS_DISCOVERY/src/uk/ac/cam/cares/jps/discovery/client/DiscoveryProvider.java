package uk.ac.cam.cares.jps.discovery.client;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;

import uk.ac.cam.cares.jps.discovery.api.Agent;
import uk.ac.cam.cares.jps.discovery.api.AgentServiceDescription;
import uk.ac.cam.cares.jps.discovery.api.AgentRequest;
import uk.ac.cam.cares.jps.discovery.api.AgentResponse;
import uk.ac.cam.cares.jps.discovery.api.IAgentCommunication;
import uk.ac.cam.cares.jps.discovery.api.TypeIRI;
import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;
import uk.ac.cam.cares.jps.discovery.util.Helper;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;
import uk.ac.cam.cares.jps.discovery.util.JavaSerializer;

public class DiscoveryProvider implements IAgentCommunication {

	
	private ISerializer serializer = DiscoveryFactory.getSerializer();
	
	@Override
	public List<TypeIRI> searchAgents(AgentRequest searchDescr) {
		
		List<TypeIRI> result = new ArrayList<TypeIRI>();
		
		String serialized = serializer.convertToString(searchDescr);
		try {
			String response = Helper.executeGet("/JPS_DISCOVERY/search", "searchdescription", serialized);
			
			//TODO-AE choose another separator
			StringTokenizer tokenizer = new StringTokenizer(response, " ");
			while (tokenizer.hasMoreTokens()) {	
				String token = tokenizer.nextToken();
				result.add(new TypeIRI(token));
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
			Helper.executeGet("/JPS_DISCOVERY/register", "agentdescription", serialized);
		} catch (ParseException | IOException | URISyntaxException e) {
			// TODO-AE throws further?
			e.printStackTrace();
		}
	}
	
	@Override
	public void deregisterAgent(TypeIRI agentAddress) throws IOException {
		try {
			// TODO-AE change parameter into agentname
			Helper.executeGet("/JPS_DISCOVERY/deregister", "agentaddress", agentAddress.getValue());
		} catch (ParseException | IOException | URISyntaxException e) {
			// TODO-AE throws further?
			e.printStackTrace();
		}
	}
}
