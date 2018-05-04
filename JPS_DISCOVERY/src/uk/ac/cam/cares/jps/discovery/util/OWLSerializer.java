package uk.ac.cam.cares.jps.discovery.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.StringTokenizer;
import java.util.UUID;

import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.config.AgentLocator;
import uk.ac.cam.cares.jps.discovery.api.AgentDescription;
import uk.ac.cam.cares.jps.discovery.api.Parameter;
import uk.ac.cam.cares.jps.discovery.api.TypeString;

public class OWLSerializer implements ISerializer {
	
	private static OWLSerializer instance = null;
	
	private OntModel ontology = null;
	private String ontBaseIRI = null;
	private OntModel knowledgeBase = null;
	private String kbBaseIRI = null;
	private int counter = 0;
	
	private OWLSerializer() {

		// TODO-AE migrate to new version of JENA !!!
		
	
	}
	
	public static synchronized OWLSerializer getInstance() {
		if (instance == null) {
			instance = new OWLSerializer();
			instance.init();
		}
		return instance;
	}
	
	private void init( ) {
		//TODO-AE make a singleton of this class in conjunction with SerializerFactory
		if (ontology == null) {
			ontology = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

			// add an ontology stored in a file to the ontology model change the file name to your own
			String path = AgentLocator.getPathToJpsDataKnowledgeBaseDir() + "/OntoAgent/OntoAgent.owl";
			File file = new File(path);
			ontology.read(file.toURI().toString());
		}
		
		//TODO-AE adapt the IRI in the owl file to OntoAgent ...
		ontBaseIRI = "http://www.theworldavatar.com/Agent#";
	}
	
	@Override
	public String convertToString(Serializable object) {
		//TODO-AE we have to synchronize this as long there are properties such knoweldgeBase ...
		
		// create an new model for the instances (otherwise the agent classes
		// will stored in the agent description file and thus be doubled)
		knowledgeBase = ModelFactory.createOntologyModel();
		UUID uuid = Helper.createUUID();	
		//TODO-AE adapt the IRI in the owl file to OntoAgent ...
		kbBaseIRI = "http://www.theworldavatar.com/Agent/Agent" + uuid + "#";
		knowledgeBase.setNsPrefix("jpsago", ontBaseIRI);
		knowledgeBase.setNsPrefix("jpsagkb", kbBaseIRI);
		
		//TODO-AE if then for AgentDesription, Request, Response ...
		//TODO-AE check that there is a list one output parameter (and domain, name...)
		createAgentWithAgentDescription((AgentDescription) object, uuid);
		
		knowledgeBase.write(System.out);
		
		return null;
	}

	@Override
	public <T extends Serializable> Optional<T> convertFrom(String objectAsString) {
		// TODO Auto-generated method stub
		return null;
	}
	
	private void createAgentWithAgentDescription(AgentDescription description, UUID uuid) {
		
		OntClass agentClass = ontology.getOntClass(ontBaseIRI + "Agent");
		//TODO-AE better agent IRI with Agent ... # ...? Kevin: Agent ... . owl (like the owl file name)
		Individual agentInd = knowledgeBase.createIndividual(kbBaseIRI + "Agent" + uuid, agentClass);
		
		
		Individual descrInd = createAgentDescription(description);
		createTripleWithObjectProperty(agentInd, "hasAgentServiceDescription", descrInd);
	}
	
	private Individual createAgentDescription(AgentDescription description) {
		//https://stackoverflow.com/questions/43719469/create-individuals-using-jena
		
		Individual result = createIndividual("AgentServiceDescription");
		
		List<Parameter> params = new ArrayList<Parameter>();
		//TODO-AE use iri domain here, add address parameter
		//TODO-AE change AgentDescription: make domain etc. to Parameter List
		Parameter domain = new Parameter(new TypeString("domain"), description.getDomain());
		params.add(domain);
		
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
		OntClass descrClass = ontology.getOntClass(ontBaseIRI + classForIndividual);
		return knowledgeBase.createIndividual(kbBaseIRI + classForIndividual + inc(), descrClass);
	}
	
	private List<Individual> createIndividualList(List<Parameter> params, String classForIndividualsinList) {
		List<Individual> result = new ArrayList<Individual>();
		
		OntClass co = ontology.getOntClass(ontBaseIRI + classForIndividualsinList);
		System.out.println("co="+co);

		for (Parameter current : params) {	
			
			Individual io = knowledgeBase.createIndividual(kbBaseIRI + classForIndividualsinList + inc(), co);
			
			createTripleWithDatatypeProperty(io, "hasKey", current.getKey().getValue());
			if ((current.getValue() != null) && (!"null".equals(current.getValue().getValue()))) {
				createTripleWithDatatypeProperty(io, "hasValue", current.getValue().getValue());
			}
			
			result.add(io);
		}
		
		return result;
	}
	
	private int inc() {
		counter += 1;
		return counter;
	}
	
	private Individual createTripleWithObjectProperty(Individual subject, String predicate, String object) {
		
		OntClass co = ontology.getOntClass(ontBaseIRI + object);
		System.out.println("co="+co);
		Individual io = knowledgeBase.createIndividual(kbBaseIRI + object + inc(), co);
		System.out.println("io="+io);
		ObjectProperty p = ontology.getObjectProperty(ontBaseIRI + predicate);
		System.out.println("p="+p);
		subject.addProperty(p, io);
		
		return io;
	}
	
	private void createTripleWithObjectProperty(Individual subject, String predicate, Individual object) {
		ObjectProperty p = ontology.getObjectProperty(ontBaseIRI + predicate);
		System.out.println("p="+p);
		subject.addProperty(p, object);
	}
	
	private void createTripleWithDatatypeProperty(Individual subject, String predicate, String datatype) {
		
		DatatypeProperty p = ontology.getDatatypeProperty(ontBaseIRI + predicate);
		System.out.println("p="+p);
		subject.addProperty(p, datatype);
	}
	
	public void write(OntModel model) throws IOException {

			File file = new File("C:/Users/Andreas/my/agentdiscovery/test/AgentKB.owl");
			if (!file.exists()) {
				file.createNewFile();
			}
				
			FileOutputStream outputstream = new FileOutputStream(file);
			
			model.write(outputstream, "RDF/XML");
	}
}
