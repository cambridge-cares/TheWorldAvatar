package uk.ac.cam.cares.jps.discovery.registry;

import java.util.Collection;

import uk.ac.cam.cares.jps.base.discovery.Agent;

public interface IRegistry {

	Collection<Agent> getAllAgents();

	void register(Agent description);

	Agent get(String agentName);

	void deregister(String agentName);

}