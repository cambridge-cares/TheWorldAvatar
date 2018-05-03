package uk.ac.cam.cares.jps.discovery.registry;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.discovery.api.AgentDescription;

public class SimpleAgentRegistry {
	
	private static SimpleAgentRegistry instance = null;
	private Logger logger = LoggerFactory.getLogger(SimpleAgentRegistry.class);
	
	private SimpleAgentRegistry() {
	}
	
	public static synchronized SimpleAgentRegistry getInstance() {
		if (instance == null) {
			instance = new SimpleAgentRegistry();
			instance.logger.info("SimpleAgentRegistry was created");
		}
		return instance;
	}

	// map from address of an agent to its description
	private Map<String, AgentDescription> descriptions = new HashMap<String, AgentDescription>();

	public Collection<AgentDescription> getAllAgentDescriptions() {
		return descriptions.values();
	}
	
	public void register(AgentDescription description) {
		
		String address = description.getAddress().getValue();
		boolean contained = descriptions.containsKey(address);
		descriptions.put(address, description);
		
		if (contained) {
			logger.info("AgentDescription was updated, agent address = " + description.getAddress().getValue());
		} else {
			logger.info("AgentDescription was registered, agent address = " + description.getAddress().getValue());
		}
	}
	
	public AgentDescription get(String agentAddress) {
		return descriptions.get(agentAddress);
	}
	
	public void deregister(String agentAddress) {
		Object found = descriptions.remove(agentAddress);
		if (found == null) {
			logger.warn("AgentDescription was not found for deregistration, agent address = " + agentAddress);
		} else {
			logger.info("AgentDescription was deregistered, agent address = " + agentAddress);
		}
	}
	
	public void clear() {
		instance.descriptions.clear();
	}
}
