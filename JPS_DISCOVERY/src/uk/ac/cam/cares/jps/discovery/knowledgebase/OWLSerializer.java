package uk.ac.cam.cares.jps.discovery.knowledgebase;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.config.AgentLocator;
import uk.ac.cam.cares.jps.discovery.api.AbstractAgentDescription;
import uk.ac.cam.cares.jps.discovery.api.Agent;
import uk.ac.cam.cares.jps.discovery.api.AgentDescription;
import uk.ac.cam.cares.jps.discovery.api.Parameter;
import uk.ac.cam.cares.jps.discovery.util.Helper;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;

public class OWLSerializer implements ISerializer {
	
	private static OWLSerializer instance = null;
	
	private static final String ONTOAGENT_BASE = "http://www.theworldavatar.com/OntoAgent";
	private OntModel ontology = null;
	private String ontBaseIRI = null;
	private OntModel knowledgeBase = null;
	private String kbBaseIRI = null;
	private int counter = 0;
	
	private OWLSerializer() {

		// TODO-AE migrate to new version of JENA !!!
		// TODO-AE URGENT import statement missing in JENA report, but required when reading agent.owl in Protege
		// TODO-AE introduce special IRI for Properties/Keys such as domain and also for its possible values
	}
	
	public static synchronized OWLSerializer getInstance() {
		if (instance == null) {
			instance = new OWLSerializer();
			instance.init();
		}
		return instance;
	}
	
	private void init( ) {
		if (ontology == null) {
			// read the ontology once for the singleton
			ontology = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);
			String path = AgentLocator.getPathToJpsDataOntologyBaseDir() + "/OntoAgent/OntoAgent.owl";
			File file = new File(path);
			ontology.read(file.toURI().toString());
		}
		
		//TODO-AE adapt the IRI in the owl file to OntoAgent ...
		ontBaseIRI = ONTOAGENT_BASE + "/OntoAgent.owl#";
	}
	
	@Override
	public synchronized String convertToString(Serializable object) {
		// OWLSerializer is a singleton. Because it has java attributes such as knowledgeBase 
		// the convert methods have to be synchronized for parallel access

		// create a new model (knowledgeBase) for the instances that are created from the input parameter Serializable object 
		// in addition to the model (ontology) for the classes
		// If both are stored in the same mode, the instances would be stored together with the doubled class definitions 
		// when writing the instances to RDF/XML
		knowledgeBase = ModelFactory.createOntologyModel();
		knowledgeBase.setNsPrefix("jpsago", ontBaseIRI);
		
		if (object instanceof Agent) {
			UUID uuid = Helper.createUUID();	
			//TODO-AE adapt the IRI in the owl file to OntoAgent ...
			kbBaseIRI = ONTOAGENT_BASE + "/Agent" + uuid + ".owl#";
			knowledgeBase.setNsPrefix("jpsagkb", kbBaseIRI);
			
			//TODO-AE if then for AgentDesription, Request, Response ...
			//TODO-AE check that there is a least one output parameter (and domain, name...)
			createAgent((Agent) object, uuid);
		} else if (object instanceof AgentDescription) {
			UUID uuid = Helper.createUUID();	
			//TODO-AE adapt the IRI in the owl file to OntoAgent ...
			//TODO-AE Class in OntoAgent.owl is AgentServiceDescription --> rename java Code
			//TODO-AE AgentMessage --> AgentServiceRequest, AgentServiceResponse
			kbBaseIRI = ONTOAGENT_BASE + "/AgentMessage#";
			knowledgeBase.setNsPrefix("jpsagkb", kbBaseIRI);
			//TODO-AE check that there is a least one output parameter (and domain, name...)
			createAgentDescription((AbstractAgentDescription) object);
		} else {
			throw new RuntimeException("can't serialize the object of type = " + object.getClass().getName());
		}
		
		//knowledgeBase.write(System.out);
		
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		knowledgeBase.write(output, "RDF/XML");
		
		return output.toString();
	}

	@Override
	public synchronized <T extends Serializable> Optional<T> convertFrom(String objectAsString) {
		
		knowledgeBase = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);
		InputStream is = new ByteArrayInputStream( objectAsString.getBytes(StandardCharsets.UTF_8) );
		knowledgeBase.read(is, null);
		
		return null;
	}
	
	private void createAgent(Agent agent, UUID uuid) {
		// the way how individuals are created here follows:
		// https://stackoverflow.com/questions/43719469/create-individuals-using-jena
		
		OntClass agentClass = ontology.getOntClass(ontBaseIRI + "Agent");
		// TODO-AE why do we need .owl in the IRI
		Individual agentInd = knowledgeBase.createIndividual(ONTOAGENT_BASE + "/Agent" + uuid + ".owl#Agent", agentClass);
		
		createTripleWithDatatypeProperty(agentInd, "hasName", agent.getName().getValue());
		
		for (AgentDescription current : agent.getDescriptions()) {
			Individual descrInd = createAgentDescription(current);
			createTripleWithObjectProperty(agentInd, "hasAgentServiceDescription", descrInd);
		}
	}
	
	private Individual createAgentDescription(AbstractAgentDescription description) {
		
		Individual result = createIndividual("AgentServiceDescription");
		
		// create individuals for all parameters
		List<Parameter> params = description.getProperties();
		//TODO-AE use iri domain here
		List<Individual> allParameters = createIndividualList(params, "Property");
		params = description.getInputParameters();
		allParameters.addAll(createIndividualList(params, "InputParameter"));
		params = description.getOutputParameters();
		allParameters.addAll(createIndividualList(params, "OutputParameter"));
		
		// create OWL List from all Parameters
		Individual listInd = createTripleWithObjectProperty(result, "hasKeyValueList", "List");
		createTripleWithObjectProperty(listInd, "hasFirstListElement", allParameters.get(0));
		for (int i=0; i<allParameters.size()-1 ; i++) {
			createTripleWithObjectProperty(allParameters.get(i), "followedBy", allParameters.get(i+1));
		}
		
		return result;
	}
	
	private Individual createIndividual(String classForIndividual) {
		OntClass cl = ontology.getOntClass(ontBaseIRI + classForIndividual);
		return knowledgeBase.createIndividual(kbBaseIRI + classForIndividual + inc(), cl);
	}
	
	private List<Individual> createIndividualList(List<Parameter> params, String classForIndividualsinList) {
		List<Individual> result = new ArrayList<Individual>();
		
		OntClass cl = ontology.getOntClass(ontBaseIRI + classForIndividualsinList);
		for (Parameter current : params) {	
			Individual ind = knowledgeBase.createIndividual(kbBaseIRI + classForIndividualsinList + inc(), cl);
			createTripleWithDatatypeProperty(ind, "hasKey", current.getKey().getValue());
			if ((current.getValue() != null) && (!"null".equals(current.getValue().getValue()))) {
				createTripleWithDatatypeProperty(ind, "hasValue", current.getValue().getValue());
			}
			result.add(ind);
		}
		
		return result;
	}
	
	private int inc() {
		counter += 1;
		return counter;
	}
	
	private Individual createTripleWithObjectProperty(Individual subject, String predicate, String object) {
		OntClass cl = ontology.getOntClass(ontBaseIRI + object);
		Individual ind = knowledgeBase.createIndividual(kbBaseIRI + object + inc(), cl);
		ObjectProperty prop = ontology.getObjectProperty(ontBaseIRI + predicate);
		subject.addProperty(prop, ind);
		return ind;
	}
	
	private void createTripleWithObjectProperty(Individual subject, String predicate, Individual object) {
		ObjectProperty prop = ontology.getObjectProperty(ontBaseIRI + predicate);
		subject.addProperty(prop, object);
	}
	
	private void createTripleWithDatatypeProperty(Individual subject, String predicate, String datatype) {
		DatatypeProperty prop = ontology.getDatatypeProperty(ontBaseIRI + predicate);
		subject.addProperty(prop, datatype);
	}
	
	private void write(OntModel model) throws IOException {
		// TODO-AE remove this method
		
		File file = new File("C:/Users/Andreas/my/agentdiscovery/test/AgentKB.owl");
		if (!file.exists()) {
			file.createNewFile();
		}
				
		FileOutputStream outputstream = new FileOutputStream(file);
			
		model.write(outputstream, "RDF/XML");
	}
}
