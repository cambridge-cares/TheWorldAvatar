package uk.ac.cam.cares.jps.discovery.registry;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.discovery.api.Agent;

public class SimpleInMemoryRegistry implements IRegistry {
	
	private static SimpleInMemoryRegistry instance = null;
	private Logger logger = LoggerFactory.getLogger(SimpleInMemoryRegistry.class);
	// map from agent name to agent (with its descriptions)
	private Map<String, Agent> agents = new HashMap<String, Agent>();
	
	private SimpleInMemoryRegistry() {
	}
	
	public static synchronized SimpleInMemoryRegistry getInstance() {
		if (instance == null) {
			instance = new SimpleInMemoryRegistry();
			instance.logger.info(SimpleInMemoryRegistry.class.getSimpleName() + " was created");
		}
		return instance;
	}

	@Override
	public Collection<Agent> getAllAgents() {
		return agents.values();
	}
	
	@Override
	public void register(Agent agent) {
		
		String address = agent.getName().getValue();
		boolean contained = agents.containsKey(address);
		agents.put(address, agent);
		
		if (contained) {
			logger.info("Agent was updated, agent name = " + agent.getName().getValue());
		} else {
			logger.info("Agent was registered, agent name = " + agent.getName().getValue());
		}
	}
	

	@Override
	public Agent get(String agentName) {
		return agents.get(agentName);
	}
	
	@Override
	public void deregister(String agentName) {
		Object found = agents.remove(agentName);
		if (found == null) {
			logger.warn("Agent was not found for deregistration, agent address = " + agentName);
		} else {
			logger.info("Agent was deregistered, agent address = " + agentName);
		}
	}
	
	public void clear() {
		instance.agents.clear();
	}
}
