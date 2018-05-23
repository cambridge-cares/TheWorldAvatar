package uk.ac.cam.cares.jps.base.discovery;

import java.io.IOException;
import java.util.List;

public interface IAgentCommunication {

	List<TypeString> searchAgents(AgentRequest agentRequest);
	
	AgentResponse callAgent(AgentRequest agentRequest);
	
	void registerAgent(Agent agent) throws IOException;
	
	void deregisterAgent(TypeString agentAddress) throws IOException;
	
	/**
	 * This method returns the names of all agents registered in JPS.
	 * Only use this method for test purposes!
	 *  
	 * @return
	 */
	List<TypeString> getAllAgentNames();
}
