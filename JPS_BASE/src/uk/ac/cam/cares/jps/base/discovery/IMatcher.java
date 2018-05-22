package uk.ac.cam.cares.jps.base.discovery;

import java.util.List;

public interface IMatcher {

	public List<Agent> getMatches(AgentRequest agentRequest);
}
