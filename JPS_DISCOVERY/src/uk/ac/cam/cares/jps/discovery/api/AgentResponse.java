package uk.ac.cam.cares.jps.discovery.api;

public class AgentResponse extends AbstractAgentDescription {

	private static final long serialVersionUID = 5627102762567906547L;
	private boolean agentFound = true;

	public boolean isAgentFound() {
		return agentFound;
	}

	//TODO-AE do we still need this? It is missing the OWL file so far
	public void setAgentFound(boolean agentFound) {
		this.agentFound = agentFound;
	}
}
