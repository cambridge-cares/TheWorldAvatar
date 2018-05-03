package uk.ac.cam.cares.jps.discovery.api;

import java.util.List;

public interface IMatcher {

	public List<AgentDescription> getMatches(AgentRequest agentRequest);
}
