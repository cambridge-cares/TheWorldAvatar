package uk.ac.cam.cares.jps.discovery.api;

import java.util.List;

public interface IMatcher {

	public List<Agent> getMatches(AgentRequest agentRequest);
}
