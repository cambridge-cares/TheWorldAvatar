package uk.ac.cam.cares.jps.discovery.registry;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.config.AgentLocator;
import uk.ac.cam.cares.jps.discovery.api.AgentDescription;

public class SimpleFileBasedRegistry implements IRegistry {

	private static SimpleFileBasedRegistry instance = null;
	private Logger logger = LoggerFactory.getLogger(SimpleFileBasedRegistry.class);
	// map from address of an agent to its description
	private Map<String, AgentDescription> descriptions = new HashMap<String, AgentDescription>();
	
	private SimpleFileBasedRegistry() {
	}
	
	public static synchronized SimpleFileBasedRegistry getInstance() {
		if (instance == null) {
			instance = new SimpleFileBasedRegistry();
			instance.init();
			instance.logger.info(SimpleFileBasedRegistry.class.getSimpleName() + " was created");
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
	public Collection<AgentDescription> getAllAgentDescriptions() {
		return descriptions.values();
	}

	@Override
	public void register(AgentDescription description) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException();
	}

	@Override
	public AgentDescription get(String agentAddress) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void deregister(String agentAddress) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException();
	}

}
