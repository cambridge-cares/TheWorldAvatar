package uk.ac.cam.cares.jps.discovery.test;

import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;
import uk.ac.cam.cares.jps.discovery.matching.exact.ExactMatcher;
import uk.ac.cam.cares.jps.discovery.registry.IRegistry;
import uk.ac.cam.cares.jps.discovery.registry.SimpleInMemoryRegistry;

public class TestExactMatcher extends TestCase {
	
	private ExactMatcher createMatcherWithOneAgent() {
		
		IRegistry registry = DiscoveryFactory.getRegistry();
		// TODO-AE make clear part of IRegistry? 
		((SimpleInMemoryRegistry) registry).clear();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
	
		Agent agent = DescriptionFactory.createAgent("IRIagentOne", general, input, output);
		registry.register(agent);
		
		return new ExactMatcher(registry);
	}
	
	private ExactMatcher createMatcherWithFiveAgents() {
		
		IRegistry registry = DiscoveryFactory.getRegistry();
		// TODO-AE make clear part of IRegistry? 
		((SimpleInMemoryRegistry) registry).clear();
		
		// agent 1
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
		Agent agent = DescriptionFactory.createAgent("IRIagentOne", general, input, output);
		registry.register(agent);
		
		// agent 2 (same description as agent 1)
		general = "domain,weather";
		input = "city,null";
		output = "IRItemperature,null";
		agent = DescriptionFactory.createAgent("IRIagentTwo", general, input, output);
		registry.register(agent);
		
		// agent 3
		general = "domain,weather";
		input = "city,null,region,null";
		output = "IRItemperature,null";
		agent = DescriptionFactory.createAgent("IRIagentThree", general, input, output);
		registry.register(agent);
		
		// agent 4
		general = "domain,weather";
		input = "city,null";
		output = "temperature,null";
		agent = DescriptionFactory.createAgent("IRIagentFour", general, input, output);
		registry.register(agent);
		
		// agent 5
		general = "";
		input = "city,null";
		output = "IRItemperature,null";
		agent = DescriptionFactory.createAgent("IRIagentFive", general, input, output);
		registry.register(agent);
		
		return new ExactMatcher(registry);
	}
	
	public void testMatchForRegistryWithOneAgent() {

		ExactMatcher matcher = createMatcherWithOneAgent();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,30";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<Agent> result = matcher.getMatches(message);
		
		assertEquals(1, result.size());
		assertEquals(1, result.get(0).getDescriptions().size());
	}
	
	public void testNoMatchForRegistryWithOneAgent1() {

		ExactMatcher matcher = createMatcherWithOneAgent();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRIwind,30";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<Agent> result = matcher.getMatches(message);
		
		assertEquals(0, result.size());
	}
	
	public void testNoMatchForRegistryWithOneAgent2() {

		ExactMatcher matcher = createMatcherWithOneAgent();
		
		String general = "domain,weather";
		String input = "city,null,date,25042018";
		String output = "IRIwind,30";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<Agent> result = matcher.getMatches(message);
		
		assertEquals(0, result.size());
	}
	
	public void testNoMatchForRegistryWithOneAgent3() {

		ExactMatcher matcher = createMatcherWithOneAgent();
		
		String general = "domain,adms";
		String input = "city,null,date,25042018";
		String output = "IRIwind,30";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<Agent> result = matcher.getMatches(message);
		
		assertEquals(0, result.size());
	}
	
	public void testTwoMatchesForRegistryWithFiveAgents() {

		ExactMatcher matcher = createMatcherWithFiveAgents();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<Agent> result = matcher.getMatches(message);
		
		assertEquals(2, result.size());
		assertEquals(1, result.get(0).getDescriptions().size());
		assertEquals(1, result.get(1).getDescriptions().size());
	}
	
	public void testNoMatchesForRegistryWithFiveAgents() {

		ExactMatcher matcher = createMatcherWithFiveAgents();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "";
		AgentRequest message = DescriptionFactory.createDiscoveryMessage(general, input, output);
		
		List<Agent> result = matcher.getMatches(message);
		
		assertEquals(0, result.size());
	}
}
