package uk.ac.cam.cares.jps.discovery.registry;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.discovery.api.AgentDescription;

public class SimpleInMemoryRegistry implements IRegistry {
	
	private static SimpleInMemoryRegistry instance = null;
	private Logger logger = LoggerFactory.getLogger(SimpleInMemoryRegistry.class);
	// map from address of an agent to its description
	private Map<String, AgentDescription> descriptions = new HashMap<String, AgentDescription>();
	
	private SimpleInMemoryRegistry() {
	}
	
	public static synchronized SimpleInMemoryRegistry getInstance() {
		if (instance == null) {
			instance = new SimpleInMemoryRegistry();
			instance.logger.info(SimpleInMemoryRegistry.class.getSimpleName() + " was created");
		}
		return instance;
	}

	/* (non-Javadoc)
	 * @see uk.ac.cam.cares.jps.discovery.registry.IRegistry#getAllAgentDescriptions()
	 */
	@Override
	public Collection<AgentDescription> getAllAgentDescriptions() {
		return descriptions.values();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.cam.cares.jps.discovery.registry.IRegistry#register(uk.ac.cam.cares.jps.discovery.api.AgentDescription)
	 */
	@Override
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
	
	/* (non-Javadoc)
	 * @see uk.ac.cam.cares.jps.discovery.registry.IRegistry#get(java.lang.String)
	 */
	@Override
	public AgentDescription get(String agentAddress) {
		return descriptions.get(agentAddress);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.cam.cares.jps.discovery.registry.IRegistry#deregister(java.lang.String)
	 */
	@Override
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
