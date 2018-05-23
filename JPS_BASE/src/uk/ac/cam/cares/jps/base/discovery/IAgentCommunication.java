package uk.ac.cam.cares.jps.base.discovery;

import java.util.List;

public interface IAgentCommunication {

	List<String> searchAgents(AgentRequest agentRequest);
	
	AgentResponse callAgent(AgentRequest agentRequest);
	
	void registerAgent(Agent agent);
	
	void deregisterAgent(String agentAddress);
	
	/**
	 * This method returns the names of all agents registered in JPS.
	 * Only use this method for test purposes!
	 *  
	 * @return
	 */
	List<String> getAllAgentNames();
}
