package uk.ac.cam.cares.jps.discovery.test;

import static org.junit.Assert.assertNotEquals;

import java.io.IOException;
import java.util.List;

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
import uk.ac.cam.cares.jps.base.discovery.AgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.discovery.client.DiscoveryProvider;
import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;
import uk.ac.cam.cares.jps.discovery.knowledgebase.OWLSerializer;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;

public class TestDiscovery extends TestCase {
	
	public void testSerializeAgentServiceDescriptionWithJavaSerializer() {
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
	
		AgentServiceDescription descr = DescriptionFactory.createAgentServiceDescription(general, input, output);
		
		ISerializer serializer = DiscoveryFactory.getSerializer();
		String s = serializer.convertToString(descr);
		
		System.out.println("serialized = " + s);
		
		AgentServiceDescription actual = serializer.<AgentServiceDescription>convertFrom(s, AgentServiceDescription.class).get();
		
		// the objects itself are different
		assertNotEquals(descr, actual);
		
		// but their attributes are the same
		Parameter pDescr = descr.getProperties().get(0);
		Parameter pActual = actual.getProperties().get(0);
		assertEquals(pDescr.getKey(), pActual.getKey());
		assertEquals(pDescr.getValue(), pActual.getValue());
		pDescr = descr.getInputParameters().get(0);
		pActual = actual.getInputParameters().get(0);
		assertEquals(pDescr.getKey(), pActual.getKey());
		assertEquals(pDescr.getValue(), pActual.getValue());
		pDescr = descr.getOutputParameters().get(0);
		pActual = actual.getOutputParameters().get(0);
		assertEquals(pDescr.getKey(), pActual.getKey());
		assertEquals(pDescr.getValue(), pActual.getValue());
	}
	
	public void testSerializeAgentServiceDescriptionWithOWLSerializer() {
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
	
		AgentServiceDescription descr = DescriptionFactory.createAgentServiceDescription(general, input, output);
		
		String s = OWLSerializer.getInstance().convertToString(descr);
		
		System.out.println("\n\nserialized = \n" + s);
	}
	
	public void testSerializeAgentWithOWLSerializer() {
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
	
		Agent agent = DescriptionFactory.createAgent("IRIsomeAgent", general, input, output);
		
		String s = OWLSerializer.getInstance().convertToString(agent);
		
		System.out.println("\n\nserialized = \n" + s);
	}
	
	public void testWriteAgentToOwlFile() throws IOException {
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
		
		Agent agent = DescriptionFactory.createAgent("IRIsomeAgent", general, input, output);
		OWLSerializer.getInstance().writeAsOwlFile(agent);
	}
	
	private String callAgent(String url) throws ParseException, IOException {
		HttpUriRequest request = new HttpGet(url);
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		String response = EntityUtils.toString(httpResponse.getEntity());
		return response;
	}
	
	private List<String> getAgents() throws ClientProtocolException, IOException {
		return  new DiscoveryProvider().getAllAgentNames();
	}

	private void deregisterAllAgents() throws ClientProtocolException, IOException {	
		for (String current : getAgents()) {
			new DiscoveryProvider().deregisterAgent(current);
		}
	}
	
	private void register(Agent agent) throws IOException {
		new DiscoveryProvider().registerAgent(agent);
	}
	
	public void testRegisterOneAgentTwice() throws IOException {
		
		deregisterAllAgents();
		
		Agent agent = new WeatherAgentOne().getAgent();
		
		register(agent);	
		register(agent);	
		List<String> actual = getAgents();
		assertEquals(1, actual.size());
		assertEquals(agent.getName(), actual.get(0));
	}
	
	private void registerFiveAgents() throws IOException {
		String general = "domain,building";
		String input = "city,null,region,null";
		String output = "IRINumberOfBuildings,null";
		
		for (int i=1; i<6; i++) {
			Agent agent = DescriptionFactory.createAgent("IRIagent"+i, general, input, output);
			register(agent);
		}
	}
	
	public void testRegisterAndUnregisterFiveAgents() throws IOException {
		
		deregisterAllAgents();
		
		registerFiveAgents();
		
		List<String> actual = getAgents();
		assertEquals(5, actual.size());
		
		deregisterAllAgents();
		
		actual = getAgents();
		assertEquals(0, actual.size());
	}
	
	public void testSearchTwoAgentsOutofSevenRegisteredAgentsAndCallOneAgent() throws IOException {
		
		deregisterAllAgents();
		
		registerFiveAgents();
		Agent agentOne = new WeatherAgentOne().getAgent();
		register(agentOne);
		Agent agentTwo = new WeatherAgentTwo().getAgent();
		register(agentTwo);	
		
		List<String> actual = getAgents();
		assertEquals(7, actual.size());
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
		AgentRequest searchDescr = DescriptionFactory.createDiscoveryMessage(general, input, output);

		List<String> actualSearch = new DiscoveryProvider().searchAgents(searchDescr);
		assertEquals(2, actualSearch.size());
		
		String actv0 = actualSearch.get(0);
		String actv1 = actualSearch.get(1);
		String expvOne = agentOne.getName();
		String expvTwo = agentTwo.getName();
		boolean b = (actv0.equals(expvOne) && actv1.equals(expvTwo)) 
				|| (actv0.equals(expvTwo) && actv1.equals(expvOne));
		assertTrue(b);
		
		String name = actv0;
		if (name.endsWith("AgentTwo")) {
			name = actv1;
		}
		
		String messageFromAgentOne = callAgent(name);
		assertEquals("I'm weather agent one", messageFromAgentOne);
	}
	
	
	public void testSearchAndCallAgentByDiscoveryService() throws IOException {
		
		deregisterAllAgents();
		
		Agent agentTwo = new WeatherAgentTwo().getAgent();
		register(agentTwo);	
		
		String general = "domain,weather";
		String input = "city,berlin";
		String output = "IRItemperature,null";
		AgentRequest agentRequest = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		AgentResponse agentResponse = new DiscoveryProvider().callAgent(agentRequest);
		String actual = agentResponse.getOutputParameters().get(0).getValue();
		assertEquals("30.3", actual);
	}
}
