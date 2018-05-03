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

import uk.ac.cam.cares.jps.discovery.api.AgentDescription;
import uk.ac.cam.cares.jps.discovery.api.AgentRequest;
import uk.ac.cam.cares.jps.discovery.api.AgentResponse;
import uk.ac.cam.cares.jps.discovery.api.IAgentCommunication;
import uk.ac.cam.cares.jps.discovery.api.TypeIRI;
import uk.ac.cam.cares.jps.discovery.util.Helper;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;
import uk.ac.cam.cares.jps.discovery.util.SerializerFactory;

public class DiscoveryProvider implements IAgentCommunication {

	
	private ISerializer serializer = SerializerFactory.createSerializer();
	
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
	public void registerAgent(AgentDescription description) throws IOException {
		// TODO-AE remove project dependency to JPS by moving Agentlocator etc.
		// attention:
		// a) added property for discovery agent to config properties
		// b) remove ClientProtocolException from callAgent
		// but I changed this on the branch and it has to be checked in on the main
		// branch
		// String responseRegistry = AgentLocator.callAgent("agent.discovery.registry");

		String serialized = serializer.convertToString(description);
		try {
			Helper.executeGet("/JPS_DISCOVERY/register", "agentdescription", serialized);
		} catch (ParseException | IOException | URISyntaxException e) {
			// TODO-AE throws further?
			e.printStackTrace();
		}
	}
	
	private void registerAgentOLD(AgentDescription description) throws IOException {
		// TODO-AE remove project dependency to JPS by moving Agentlocator etc.
		// attention:
		// a) added property for discovery agent to config properties
		// b) remove ClientProtocolException from callAgent
		// but I changed this on the branch and it has to be checked in on the main
		// branch
		// String responseRegistry = AgentLocator.callAgent("agent.discovery.registry");

		// String serialized = Serializer.convertToString(description);

		// TODO-AE registry agent uri should not be hard coded

		// String uri = "http://localhost:8080/JPS_DISCOVERY/register";

		// uri = "http://localhost:8080/JPS/Configtest/AgentOne";

		String serialized = serializer.convertToString(description);

		URIBuilder builder =  new URIBuilder().setScheme("http").setHost("localhost:8080").setPath("/JPS_DISCOVERY/register")
				.setParameter("agentdescription", serialized);

		// builder.setScheme("http").setHost("localhost:8080").setPath("/JPS/Configtest/AgentOne");
		// builder.setScheme("http").setHost("localhost:8080").setPath("/JPS_DISCOVERY/DiscoveryTest/AgentOne");

		HttpGet request = null;
		try {
			request = new HttpGet(builder.build());
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// ContentType contentType = ContentType.create("text/plain", Consts.UTF_8);
		// HttpEntity entity = new StringEntity(serialized, contentType);
		// HttpPut request2 = new HttpPut(uri);
		// request2.setEntity(entity);

		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		// String response = EntityUtils.toString(httpResponse.getEntity());
		// System.out.println("RESPONSE = " + response);
	}
	

	@Override
	public void deregisterAgent(TypeIRI agentAddress) throws IOException {
		try {
			Helper.executeGet("/JPS_DISCOVERY/deregister", "agentaddress", agentAddress.getValue());
		} catch (ParseException | IOException | URISyntaxException e) {
			// TODO-AE throws further?
			e.printStackTrace();
		}
	}
}
