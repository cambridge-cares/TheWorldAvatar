package uk.ac.cam.cares.jps.discovery.knowledgebase;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.base.discovery.AbstractAgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class OWLSerializer {
	
	private static OWLSerializer instance = null;
	
	Logger logger = LoggerFactory.getLogger(OWLSerializer.class);
	private OntModel ontology = null;
	private String ontBaseIRI = null;
	private OntModel knowledgeBase = null;
	private String kbBaseIRI = null;
	private int counter = -1;
	
	private OWLSerializer() {

		// TODO-AE migrate to new version of JENA !!!
		// TODO-AE URGENT import statement missing in JENA report, but required when reading agentxxxx.owl in Protege
		// TODO-AE introduce special IRI for Properties/Keys such as domain and also for its possible values
		// TODO-AE URGENT commit the agent ontology OWL file --> discuss directory convention, maybe own project only for ontology OWL files?
	}
	
	public static synchronized OWLSerializer getInstance() {
		if (instance == null) {
			instance = new OWLSerializer();
			instance.init();
		}
		return instance;
	}
	
	private void init( ) {
		// this class is a singleton. It is enough to read the ontology only once
		String path = AgentKnowledgeBase.getFileForAgentOntology();
		ontology = JenaHelper.createModel(path);
		ontBaseIRI = AgentKnowledgeBase.ONTOAGENT_ONTOLOGY_IRI + "#";
	}
	
	public synchronized String convertToString(Serializable object) {
		// OWLSerializer is a singleton. Because it has java attributes such as knowledgeBase 
		// all public convert methods have to be synchronized for parallel access
		
		UUID uuid = createUUID();	
		ByteArrayOutputStream stream = convertToString(object, uuid);
		String s = stream.toString();
		try {
			stream.close();
		} catch (IOException e) {
			logger.error(e.getMessage(), e);
		}
		return s;
	}
	
	private ByteArrayOutputStream convertToString(Serializable object, UUID uuid) {
		
		counter = -1;
		
		// Create a new model (knowledgeBase) for the instances that will be generated from the input parameter "object" 
		// This model is created in addition to the model (ontology) for the classes
		// If both instances and classes are stored in the same model, the class definition would be duplicated 
		// when writing the instances to RDF/XML
		knowledgeBase = ModelFactory.createOntologyModel();
		knowledgeBase.setNsPrefix("jpsago", ontBaseIRI);
		
		if (object instanceof Agent) {	
			kbBaseIRI = AgentKnowledgeBase.ONTOAGENT_BASE_IRI + "/Agent" + uuid + ".owl#";
			knowledgeBase.setNsPrefix("jpsagkb", kbBaseIRI);
			createAgent((Agent) object, uuid);
		} else {
			throw new JPSRuntimeException("can't serialize the object of type = " + object.getClass().getName());
		}
		
		//knowledgeBase.write(System.out);
		
		ByteArrayOutputStream stream = new ByteArrayOutputStream();
		knowledgeBase.write(stream, "RDF/XML");
		
		return stream;
	}
	
	private void createAgent(Agent agent, UUID uuid) {
		// the way how individuals are created here follows:
		// https://stackoverflow.com/questions/43719469/create-individuals-using-jena
		
		OntClass agentClass = ontology.getOntClass(ontBaseIRI + "Agent");
		Individual agentInd = knowledgeBase.createIndividual(AgentKnowledgeBase.ONTOAGENT_BASE_IRI + "/Agent" + uuid + ".owl#Agent", agentClass);
		createTripleWithDatatypeProperty(agentInd, "hasId", uuid.toString());
		createTripleWithDatatypeProperty(agentInd, "hasName", agent.getName());
		
		for (AgentServiceDescription current : agent.getDescriptions()) {
			Individual descrInd = createAgentDescription(current);
			createTripleWithObjectProperty(agentInd, "hasAgentServiceDescription", descrInd);
		}
	}
	
	private Individual createAgentDescription(AbstractAgentServiceDescription description) {
		
		if (description.getProperties().isEmpty()) {
			throw new JPSRuntimeException("empty property list of agent description");
		}
		
		Individual result = createIndividual("AgentServiceDescription");
		
		// create individuals for all parameters
		List<Parameter> params = description.getProperties();
		List<Individual> allParameters = createIndividualList(params, "Property");
		params = description.getInputParameters();
		allParameters.addAll(createIndividualList(params, "InputParameter"));
		params = description.getOutputParameters();
		allParameters.addAll(createIndividualList(params, "OutputParameter"));
				
		for (Individual current : allParameters) {
			createTripleWithObjectProperty(result, "hasKeyValuePair", current);
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
			int index = inc();
			Individual ind = knowledgeBase.createIndividual(kbBaseIRI + classForIndividualsinList + index, cl);
			createTripleWithDatatypeProperty(ind, "hasIndex", "" + index);
			createTripleWithDatatypeProperty(ind, "hasKey", current.getKey());
			if ((current.getValue() != null) && (!"null".equals(current.getValue()))) {
				createTripleWithDatatypeProperty(ind, "hasValue", (String) current.getValue());
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
	
	public void writeAsOwlFile(Agent agent) throws IOException {
		
		UUID uuid = createUUID();
		ByteArrayOutputStream bytestream = convertToString(agent, uuid);
		
		String path = null; //AgentLocator.getPathToJpsDataKnowledgeDir() + "/OntoAgent/Agent" + uuid + ".owl";
		File file = new File(path);
		if (!file.exists()) {
			file.createNewFile();
		}
		
		FileOutputStream filestream = new FileOutputStream(file);
		bytestream.writeTo(filestream);
		bytestream.close();
		filestream.close();
	}
	
	
	private static UUID createUUID() {
		return UUID.randomUUID();
	}
}
