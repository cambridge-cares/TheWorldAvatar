package uk.ac.cam.cares.jps.discovery.registry;

import java.util.Collection;

import uk.ac.cam.cares.jps.discovery.api.AgentDescription;

public interface IRegistry {

	Collection<AgentDescription> getAllAgentDescriptions();

	void register(AgentDescription description);

	AgentDescription get(String agentAddress);

	void deregister(String agentAddress);

}