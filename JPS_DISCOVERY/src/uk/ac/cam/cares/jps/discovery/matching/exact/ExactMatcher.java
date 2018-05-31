package uk.ac.cam.cares.jps.discovery.matching.exact;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import uk.ac.cam.cares.jps.base.discovery.AbstractAgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.discovery.knowledgebase.AgentKnowledgeBase;

public class ExactMatcher implements IMatcher{
	
	public ExactMatcher() {
	}
	
	@Override
	public List<Agent> getMatches(AgentRequest agentRequest) {
		
		List<Agent> result = new ArrayList<Agent>();
		
		Parameter domain = findProperty(agentRequest, "domain");
		Collection<Agent> foundAgents = searchAgents((String) domain.getValue());
		
		for (Agent currentAgent : foundAgents) {
			
			Agent agent = null;
			for (AgentServiceDescription currentDescr : currentAgent.getDescriptions()) {
				if (match(agentRequest, currentDescr)) {
					if (agent == null) {
						agent = new Agent();
						agent.setName(currentAgent.getName());
						result.add(agent);
					}
					agent.addDescription(currentDescr);
				}
			}
		}
		
		return result;
	}
	
	public Collection<Agent> searchAgents(String domain) {
		
		List<Agent> result = new ArrayList<Agent>();
		
		for (Agent currentAgent : AgentKnowledgeBase.getInstance().getAllAgents()) {
			for (AgentServiceDescription currentDescr : currentAgent.getDescriptions()) {
				
				Parameter domainParam = findProperty(currentDescr, "domain");
				if ((domainParam != null) && domain.equals(domainParam.getValue())) {
					result.add(currentAgent);
					break;
				}
			}					
		}
		
		return result;
	}
	
	private Parameter findProperty(AbstractAgentServiceDescription descr, String key) {
		for (Parameter current : descr.getProperties()) {
			if (key.equals(current.getKey())) {
				return current;
			}
		}
		return null;
	}
	
	private boolean matchTypes(String p, String q) {
		
		if ((p == null) || (q == null)) {
			return false;
		}
		
		if (!p.equals(q)) {
			return false;
		}
		
		return true;
	}
	
	/**
	 * returns true if and only if the keys of the first parameter list coincides with the
	 * keys of the second parameter and if the keys appear in the same order. The method
	 * does not check the parameter values. 
	 * 
	 * @param p
	 * @param q
	 * @return
	 */
	private boolean matchParameters(List<Parameter> p, List<Parameter> q) {
		
		if (p.size() != q.size()) {
			return false;
		}
		
		for (int i=0; i<p.size(); i++) {
			String pKey = p.get(i).getKey();
			String qKey = q.get(i).getKey();
			
			if (!matchTypes(pKey, qKey)) {
				return false;
			}
		}
		
		return true;
	}
	

	private boolean match(AgentRequest agentRequest, AgentServiceDescription descr) {
		
		if (!matchParameters(agentRequest.getProperties(), descr.getProperties())) {
			return false;
		}
		
		if (!matchParameters(agentRequest.getInputParameters(), descr.getInputParameters())) {
			return false;
		}
		
		if (!matchParameters(agentRequest.getOutputParameters(), descr.getOutputParameters())) {
			return false;
		}
		
		return true;
	}
}
