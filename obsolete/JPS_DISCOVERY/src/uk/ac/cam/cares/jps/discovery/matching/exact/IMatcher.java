package uk.ac.cam.cares.jps.discovery.matching.exact;

import java.util.List;

import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;

public interface IMatcher {

	public List<Agent> getMatches(AgentRequest agentRequest);
}
