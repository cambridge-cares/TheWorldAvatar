package uk.ac.cam.cares.jps.discovery.test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.discovery.matching.exact.ExactMatcher;

public class TestExactMatcher extends TestCase {
	
	
	private ExactMatcher createMatcherMock(final Collection<Agent> agents) {
		
		return new ExactMatcher() {

			@Override
			public Collection<Agent> searchAgents(String domain) {
				return agents;
			}
		};
	}
	
	private ExactMatcher createMatcherWithOneAgent() {
		
		List<Agent> agents = new ArrayList<Agent>();
		
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
	
		Agent agent = DescriptionFactory.createAgent("IRIagentOne", general, input, output);
		agents.add(agent);
		
		return createMatcherMock(agents);
	}
	
	private ExactMatcher createMatcherWithFiveAgents() {
		
		List<Agent> agents = new ArrayList<Agent>();
		
		// agent 1
		String general = "domain,weather";
		String input = "city,null";
		String output = "IRItemperature,null";
		Agent agent = DescriptionFactory.createAgent("IRIagentOne", general, input, output);
		agents.add(agent);
		
		// agent 2 (same description as agent 1)
		general = "domain,weather";
		input = "city,null";
		output = "IRItemperature,null";
		agent = DescriptionFactory.createAgent("IRIagentTwo", general, input, output);
		agents.add(agent);
		
		// agent 3
		general = "domain,weather";
		input = "city,null,region,null";
		output = "IRItemperature,null";
		agent = DescriptionFactory.createAgent("IRIagentThree", general, input, output);
		agents.add(agent);
		
		// agent 4
		general = "domain,weather";
		input = "city,null";
		output = "temperature,null";
		agent = DescriptionFactory.createAgent("IRIagentFour", general, input, output);
		agents.add(agent);
		
		// agent 5
		general = "";
		input = "city,null";
		output = "IRItemperature,null";
		agent = DescriptionFactory.createAgent("IRIagentFive", general, input, output);
		agents.add(agent);
		
		return createMatcherMock(agents);
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
