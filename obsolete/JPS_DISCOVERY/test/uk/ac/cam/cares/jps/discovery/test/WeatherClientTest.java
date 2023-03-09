package uk.ac.cam.cares.jps.discovery.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentResponse;
import uk.ac.cam.cares.jps.base.discovery.DiscoveryProvider;
import uk.ac.cam.cares.jps.weather.WeatherAgent;


public class WeatherClientTest extends TestCase  {

	private String getUrlForDiscovery() {
		return "http://localhost:8080/JPS_DISCOVERY";
	}
	
	//call all the agents via http request and return the result of response as a string
	private String callAgent(String url) throws ParseException, IOException {
		HttpUriRequest request = new HttpGet(url);
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request); //match the request and the response
		String response = EntityUtils.toString(httpResponse.getEntity()); //convert it to string to be shown as the result in return
		return response;
	}
	
	
	//make one big string of response into tokenizer in a variable called result
	private List<String> getAgents() throws ClientProtocolException, IOException {
		
		List<String> result = new ArrayList<String>();
		
		String url = getUrlForDiscovery() + "/agents";
		String response = callAgent(url);
		StringTokenizer tokenizer = new StringTokenizer(response, " ");
		while (tokenizer.hasMoreTokens()) {	
			String token = tokenizer.nextToken();
			result.add(token);
		}
		
		return result;
	}
	
	//deregister all the agents which exist and make the registry empty
	private void deregisterAllAgents() throws ClientProtocolException, IOException {	
		for (String current : getAgents()) {
			String address = new String(current);
			new DiscoveryProvider().clear();
		}
	}
	
	//register the agent which has the description as the input of the function
	private void register(Agent agent) throws IOException {
		new DiscoveryProvider().registerAgent(agent);
	}
	
	public void testSearchAndCallAgentByDiscoveryService() throws IOException {
		
		
		
		deregisterAllAgents();
		
		//register the weather agent into the list in "KB"
		Agent agent = new WeatherAgent().getAgent();
		register(agent);	
		
		//declare the request that specify the agent, whether it exist or not in the list
		String general = "domain,weather";
		String input = "city,denhaag";
		String output ="temperature,null,cloudcover,null,windspeed,null,winddirection,null";
		AgentRequest agentRequest = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		//get the response by try calling the "requested agent" whether it exist or not
		AgentResponse agentResponse = new DiscoveryProvider().callAgent(agentRequest);

		//get the value from the response obtained and see whether the value is matched with the agent  listed in kb
		String actual = (String) agentResponse.getOutputParameters().get(0).getValue();
		assertEquals("28", actual);

		String actual2 = (String) agentResponse.getOutputParameters().get(1).getValue();
		assertEquals("5", actual2);

	}
}
