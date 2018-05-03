package uk.ac.cam.cares.jps.discovery.test;

import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.discovery.api.AgentDescription;
import uk.ac.cam.cares.jps.discovery.api.AgentRequest;
import uk.ac.cam.cares.jps.discovery.matching.exact.ExactMatcher;
import uk.ac.cam.cares.jps.discovery.registry.SimpleAgentRegistry;

public class TestExactMatcher extends TestCase {
	
	private ExactMatcher createMatcherWithOneAgent() {
		
		SimpleAgentRegistry registry = SimpleAgentRegistry.getInstance();
		registry.clear();
		
		String general = "domain,weather,address,IRIagentOne";
		String input = "city,null";
		String output = "IRItemperature,null";
	
		AgentDescription description = DescriptionFactory.createAgentDescription(general, input, output);
		registry.register(description);
		
		return new ExactMatcher(registry);
	}
	
	private ExactMatcher createMatcherWithFiveAgents() {
		
		SimpleAgentRegistry registry = SimpleAgentRegistry.getInstance();
		registry.clear();
		
		// agent 1
		String general = "domain,weather,address,IRIagentOne";
		String input = "city,null";
		String output = "IRItemperature,null";
		AgentDescription description = DescriptionFactory.createAgentDescription(general, input, output);
		registry.register(description);
		
		// agent 2 (same description as agent 1)
		general = "domain,weather,address,IRIagentTwo";
		input = "city,null";
		output = "IRItemperature,null";
		description = DescriptionFactory.createAgentDescription(general, input, output);
		registry.register(description);
		
		// agent 3
		general = "domain,weather,address,IRIagentThree";
		input = "city,null,region,null";
		output = "IRItemperature,null";
		description = DescriptionFactory.createAgentDescription(general, input, output);
		registry.register(description);
		
		// agent 4
		general = "domain,weather,address,IRIagentFour";
		input = "city,null";
		output = "temperature,null";
		description = DescriptionFactory.createAgentDescription(general, input, output);
		registry.register(description);
		
		// agent 5
		general = "address,IRIagentFive";
		input = "city,null";
		output = "IRItemperature,null";
		description = DescriptionFactory.createAgentDescription(general, input, output);
		registry.register(description);
		
		return new ExactMatcher(registry);
	}
	
	public void testMatchForRegistryWithOneAgent() {

		ExactMatcher matcher = createMatcherWithOneAgent();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,30";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<AgentDescription> result = matcher.getMatches(message);
		
		assertEquals(1, result.size());
	}
	
	public void testNoMatchForRegistryWithOneAgent1() {

		ExactMatcher matcher = createMatcherWithOneAgent();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRIwind,30";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<AgentDescription> result = matcher.getMatches(message);
		
		assertEquals(0, result.size());
	}
	
	public void testNoMatchForRegistryWithOneAgent2() {

		ExactMatcher matcher = createMatcherWithOneAgent();
		
		String general = "domain,weather";
		String input = "city,null,date,25042018";
		String output = "IRIwind,30";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<AgentDescription> result = matcher.getMatches(message);
		
		assertEquals(0, result.size());
	}
	
	public void testNoMatchForRegistryWithOneAgent3() {

		ExactMatcher matcher = createMatcherWithOneAgent();
		
		String general = "domain,adms";
		String input = "city,null,date,25042018";
		String output = "IRIwind,30";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<AgentDescription> result = matcher.getMatches(message);
		
		assertEquals(0, result.size());
	}
	
	public void testTwoMatchesForRegistryWithFiveAgents() {

		ExactMatcher matcher = createMatcherWithFiveAgents();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<AgentDescription> result = matcher.getMatches(message);
		
		assertEquals(2, result.size());
	}
	
	public void testNoMatchesForRegistryWithFiveAgents() {

		ExactMatcher matcher = createMatcherWithFiveAgents();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<AgentDescription> result = matcher.getMatches(message);
		
		assertEquals(0, result.size());
	}
}
