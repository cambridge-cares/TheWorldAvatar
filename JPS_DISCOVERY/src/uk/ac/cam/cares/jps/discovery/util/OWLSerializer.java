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

public class OWLSerializer implements ISerializer {
	
	private OntModel ontology = null;
	private String ontBaseIRI = null;
	private OntModel knowledgeBase = null;
	private String kbBaseIRI = null;
	private int counter = 0;

	@Override
	public String convertToString(Serializable object) {
		init();
		return null;
	}

	@Override
	public <T extends Serializable> Optional<T> convertFrom(String objectAsString) {
		// TODO Auto-generated method stub
		return null;
	}
	
	private void init( ) {
		//TODO-AE make a Singleton of this class in conjunction with SerializerFactory
		if (ontology == null) {
			ontology = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

			// add an ontology stored in a file to the ontology model change the file name to your own
			File file = new File("C:/Users/Andreas/my/agentdiscovery/AgentOntology.owl");
			ontology.read(file.toURI().toString());
		}
			
		ontBaseIRI = "http://www.theworldavatar.com/Agent#";
		
		UUID uuid = Helper.createUUID();
		
		kbBaseIRI = "http://www.theworldavatar.com/Agent/Agent" + uuid + "#";
		
		
		// create an new model for the instances (otherwise the agent classes
		// will stored in the agent description file and thus be doubled)
		
		knowledgeBase = ModelFactory.createOntologyModel();
		knowledgeBase.setNsPrefix("jpsago", ontBaseIRI);
		knowledgeBase.setNsPrefix("jpsagkb", kbBaseIRI);
	}
	
	private void createAgentWithAgentDescription() {
		
		OntClass agentClass = ontology.getOntClass(ontBaseIRI + "Agent");
		System.out.println("MY class=" + agentClass);
		Individual agentInd = knowledgeBase.createIndividual(kbBaseIRI, agentClass);
		Individual descrInd = createAgentDescription();
		createTripleWithObjectProperty(agentInd, "hasAgentServiceDescription", descrInd);
	}
	
	public Individual createAgentDescription() {
		//https://stackoverflow.com/questions/43719469/create-individuals-using-jena
		
		Individual result = null;
		

		//TODO-AE own method
		OntClass descrClass = ontology.getOntClass(ontBaseIRI + "AgentServiceDescription");
		result = knowledgeBase.createIndividual(kbBaseIRI, descrClass);
		
		
		String params = "k1,v1,k2,v2,k3,v3";
		List<Individual> individuals = createIndividualList(params, "InputParameter");
		
		if (individuals.size() > 0) {
			Individual listInd = createTripleWithObjectProperty(result, "hasKeyValueList", "List");
			createTripleWithObjectProperty(listInd, "hasFirstListElement", individuals.get(0));
		}
		
		knowledgeBase.write(System.out);
		
		return result;
	}
	

	
	
	private List<Individual> createIndividualList(String params, String classForIndividualsinList) {
		List<Individual> result = new ArrayList<Individual>();
		
		OntClass co = ontology.getOntClass(ontBaseIRI + classForIndividualsinList);
		System.out.println("co="+co);
		
		StringTokenizer tokenizer = new StringTokenizer(params, ",");
		while (tokenizer.hasMoreTokens()) {
			String key = tokenizer.nextToken();
			String value = tokenizer.nextToken();
			
			Individual io = knowledgeBase.createIndividual(kbBaseIRI + classForIndividualsinList + inc(), co);
			
			createTripleWithDatatypeProperty(io, "hasKey", key);
			if (!"null".equals(value)) {
				createTripleWithDatatypeProperty(io, "hasValue", value);
			}
			
			// combine the new list element with the last one if there is already a last one
			if (result.size() > 0) {
				Individual last = result.get(result.size()-1);
				createTripleWithObjectProperty(last, "followedBy", io);
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
