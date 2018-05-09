package uk.ac.cam.cares.jps.discovery.registry;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.config.AgentLocator;
import uk.ac.cam.cares.jps.discovery.api.Agent;

public class SimpleOwlBasedRegistry implements IRegistry {

	private static SimpleOwlBasedRegistry instance = null;
	private Logger logger = LoggerFactory.getLogger(SimpleOwlBasedRegistry.class);
	// map from agent name to agent (with its descriptions)
	private Map<String, Agent> agents = new HashMap<String, Agent>();
	
	private SimpleOwlBasedRegistry() {
	}
	
	public static synchronized SimpleOwlBasedRegistry getInstance() {
		if (instance == null) {
			instance = new SimpleOwlBasedRegistry();
			instance.init();
			instance.logger.info(SimpleOwlBasedRegistry.class.getSimpleName() + " was created");
		}
		return instance;
	}
	
	private void init() {
		String kbPath = AgentLocator.getPathToJpsDataKnowledgeBaseDir() + "/OntoAgent";
		
		
		// TODO-AE READ all agent description files (besides the class ontology OWL file OntoAgent.owl)
		// We assume that /OntoAgent subdirectory together with OntoAgent.owl is already there
		// TODO-AE we have to commit OntoAgent.owl to GIT and maybe we need an extra directory to separate
		// class ontology OWL file from instance knowledgebase OWL files.
		
		
	}

	@Override
	public void deregister(String agentName) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException();
	}

	@Override
	public void register(Agent description) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Agent get(String agentName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<Agent> getAllAgents() {
		// TODO Auto-generated method stub
		return null;
	}
}
