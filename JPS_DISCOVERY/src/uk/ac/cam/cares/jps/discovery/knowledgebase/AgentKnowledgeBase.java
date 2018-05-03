package uk.ac.cam.cares.jps.discovery.knowledgebase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;

import uk.ac.cam.cares.jps.discovery.api.AgentDescription;
import uk.ac.cam.cares.jps.discovery.util.Helper;

public class AgentKnowledgeBase {

	private static AgentKnowledgeBase instance = null;
	//TODO-AE hard coded name space
	private static final String NAME_SPACE = "C://Users/Andreas/my/agentdiscovery#";
	private Logger logger = LoggerFactory.getLogger(AgentKnowledgeBase.class);
	private OntModel model = null;
	
	private AgentKnowledgeBase () {
	}
	
	public static synchronized AgentKnowledgeBase getInstance() {
		if (instance == null) {
			instance = new AgentKnowledgeBase();
			instance.init();
			instance.logger.info("AgentKnowledgeBase was created");
		}
		return instance;
	}
	
	private void init() {
		//TODO-AE file path hard coded
		String filepath = "C://Users/Andreas/my/agentdiscovery/AgentOntology.owl";
		model = RDFHelper.loadModel(filepath);
	}
	
	public void add(AgentDescription descr) {
		
		OntClass agentDescrClass = model.getOntClass(NAME_SPACE + "AgentDescription");
		
		Individual ad = createIndividual(agentDescrClass);
		
		//ad.add
		
	}
	
	private Individual createIndividual(OntClass ontClass) {
		String indName = NAME_SPACE + ontClass.getLocalName() + Helper.createUUID();
		return  model.createIndividual(indName, ontClass);
	}
}
