package uk.ac.cam.cares.jps.discovery.matching.exact;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.discovery.api.Agent;
import uk.ac.cam.cares.jps.discovery.api.AgentServiceDescription;
import uk.ac.cam.cares.jps.discovery.api.AgentRequest;
import uk.ac.cam.cares.jps.discovery.api.IMatcher;
import uk.ac.cam.cares.jps.discovery.api.IType;
import uk.ac.cam.cares.jps.discovery.api.Parameter;
import uk.ac.cam.cares.jps.discovery.registry.IRegistry;

public class ExactMatcher implements IMatcher{

	private IRegistry registry = null;
	
	public ExactMatcher(IRegistry registry) {
		this.registry = registry;
	}
	
	@Override
	public List<Agent> getMatches(AgentRequest agentRequest) {
		
		List<Agent> result = new ArrayList<Agent>();
		
		for (Agent currentAgent : registry.getAllAgents()) {
			
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
	
	private boolean matchTypes(IType p, IType q) {
		
		if ((p == null) || (q == null)) {
			return false;
		}
		
		if (!p.getClass().equals(q.getClass())) {
			return false;
		}
		
		if (!p.getValue().equals(q.getValue())) {
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
			IType pKey = p.get(i).getKey();
			IType qKey = q.get(i).getKey();
			
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
