package uk.ac.cam.cares.jps.discovery.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import com.hp.hpl.jena.ontology.DatatypeProperty;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.ObjectProperty;
import com.hp.hpl.jena.ontology.OntClass;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.ontology.Ontology;
import com.hp.hpl.jena.query.Query;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.query.ResultSetFactory;
import com.hp.hpl.jena.query.ResultSetFormatter;
import com.hp.hpl.jena.query.ResultSetRewindable;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.vocabulary.RDFS;

import junit.framework.TestCase;

public class AndreasJenaTest extends TestCase {
	
	public static ResultSet sparql (String fileLocat, String Qstring) {
		
		OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		try {
		    File file = new File(fileLocat);
		    FileInputStream reader = new FileInputStream(file);
		    model.read(reader,null);     //load the ontology model
		} catch (Exception e) {
		    e.printStackTrace();
		}

		Query query = QueryFactory.create(Qstring);
		QueryExecution qe = QueryExecutionFactory.create(query, model);
		ResultSet rs = qe.execSelect();                                    //the ResultSet can only be iterated once
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    //reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetFormatter.out(System.out, results, query);
				
		return results;
	}
	
	public OntModel createModel(String filepath) {
		OntModel result = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		try {
		    File file = new File(filepath);
		    FileInputStream reader = new FileInputStream(file);
		    result.read(reader,null);     //load the ontology model
		} catch (Exception e) {
		    e.printStackTrace();
		}
		
		return result;
	}
	
	public void testJena1() throws IOException {
		
		//String filepath = "C:/Users/Andreas/my/agentdiscovery/AgentOntology.owl";
		//OntModel model = createModel(filepath);
		
		
		
		String filepath = "";
		OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);
		
		File file = new File("C:/Users/Andreas/my/agentdiscovery/AgentOntology.owl");
		FileInputStream reader = new FileInputStream(file);
		//model.read(reader, null);
		
		//OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM, modelbase);
		//model.addLoadedImport("C:/Users/Andreas/my/agentdiscovery/AgentOntology.owl");
		//model.loadImports();

	
		//OntClass c = model.getOntClass("http://www.theworldavatar.com/Agent#Agent");
		
		//System.out.println("MY class=" + c);
		
		//Individual i = c.createIndividual();
		
		write(model);
		
		
		//Resource resource = model.createResource();
		//resource.asNode().
		
		
		//model.createIndividual(arg0, arg1);
		
	}
	
	
	public void testJena2() throws IOException {
		
		String filepath = "C:/Users/Andreas/my/agentdiscovery/test/AgentKBTemplate.owl";
		OntModel model = createModel(filepath);
		
		//model.hasLoadedImport(arg0)
		
		
		
		OntClass c = model.getOntClass("http://www.theworldavatar.com/Agent#Agent");
		
		System.out.println("MY class=" + c);
		
		Individual i = c.createIndividual();
		
		write(model);
	}
	
	public void testJena3() throws IOException {
		// create an ontology model
		OntModel ontology = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		// add an ontology stored in a file to the ontology model ñ change the file name to your own
		File file = new File("C:/Users/Andreas/my/agentdiscovery/AgentOntology.owl");
		ontology.read(file.toURI().toString());
		
		OntClass c = ontology.getOntClass("http://www.theworldavatar.com/Agent#Agent");
		
		System.out.println("MY class=" + c);
		
		//Individual i = c.createIndividual();
		
		Individual i = ontology.createIndividual("http://www.theworldavatar.com/Agent/KB/Agent1", c);
		
		FileOutputStream out = new FileOutputStream("C:/Users/Andreas/my/agentdiscovery/test/AgentKB.owl");
		ontology.write(out);
		out.close();
		ontology.write(System.out);
	}
	
	
	public void testJena4() {
		// https://stackoverflow.com/questions/3354600/how-to-add-owlimports-to-a-owl-file-by-jena?rq=1
		String base = "http://www.theworldavatar.com/Agent/KB#";
		OntModel model = ModelFactory.createOntologyModel();
		Ontology ont = model.createOntology("");
		ont.addImport(model.createResource("C:/Users/Andreas/my/agentdiscovery/AgentOntology.owl"));
		//model.write(System.out, "RDF/XML-ABBREV", base);
		
	
		OntClass c = model.getOntClass("http://www.theworldavatar.com/Agent#Agent");
		
		System.out.println("MY class=" + c);
		
		//Individual i = c.createIndividual();
		
		Individual i = model.createIndividual("http://www.theworldavatar.com/Agent/KB#Agent1", c);
		
		model.write(System.out, "RDF/XML-ABBREV", base);
		
	}
	
	public void testJena5() {
		//https://stackoverflow.com/questions/553810/read-jena-ontmodel-with-dependencies?rq=1
		
		OntModel model = ModelFactory.createOntologyModel();
		
		String docURI = "http://www.theworldavatar.com/Agent#";
		String locationURL = "file:///C:/Users/Andreas/my/agentdiscovery/AgentOntology.owl";
		model.getDocumentManager().addAltEntry(docURI, locationURL);
		model.read(locationURL);
		
		OntClass c = model.getOntClass("http://www.theworldavatar.com/Agent#Agent");
		
		System.out.println("MY class=" + c);
		
		Individual i = model.createIndividual("http://www.theworldavatar.com/Agent/KB#Agent1", c);
		
		String base = "http://www.theworldavatar.com/Agent/KB#";
		model.write(System.out, "RDF/XML-ABBREV", base);
		
	}
	
	public void testJena6() {
		//https://stackoverflow.com/questions/43719469/create-individuals-using-jena
		
		OntModel m = ModelFactory.createOntologyModel();
		m.setNsPrefix("agkb", "http://www.theworldavatar.com/Agent/KB#");
		Individual i = m.createIndividual("http://www.theworldavatar.com/Agent/KB#Agent1", m.createResource("http://www.theworldavatar.com/Agent/KB#Agent1"));
		i.addProperty(RDFS.comment, "something");
		m.write(System.out);
	}
	
	public void testJena6plus5() {
		//https://stackoverflow.com/questions/43719469/create-individuals-using-jena
		
		ontology = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		// add an ontology stored in a file to the ontology model change the file name to your own
		File file = new File("C:/Users/Andreas/my/agentdiscovery/AgentOntology.owl");
		ontology.read(file.toURI().toString());
		
		ontBaseIRI = "http://www.theworldavatar.com/Agent#";
		kbBaseIRI = "http://www.theworldavatar.com/Agent/Agent1#";
		
		
		// create an new model for the instances (otherwise the agent classes
		// will stored in the agent description file and thus be doubled)
		
		knowledgeBase = ModelFactory.createOntologyModel();
		knowledgeBase.setNsPrefix("jpsago", ontBaseIRI);
		knowledgeBase.setNsPrefix("jpsagkb", kbBaseIRI);
		
		
		OntClass agentClass = ontology.getOntClass(ontBaseIRI + "Agent");
		
		System.out.println("MY class=" + agentClass);
		
		Individual agentInd = knowledgeBase.createIndividual("http://www.theworldavatar.com/Agent/Agent1#", agentClass);

		Individual descrInd = createTripleWithObjectProperty(agentInd, "hasAgentServiceDescription", "AgentServiceDescription");
		
		String params = "k1,v1,k2,v2,k3,v3";
		List<Individual> individuals = createIndividualList(params, "InputParameter");
		
		if (individuals.size() > 0) {
			Individual listInd = createTripleWithObjectProperty(descrInd, "hasKeyValueList", "List");
			createTripleWithObjectProperty(listInd, "hasFirstListElement", individuals.get(0));
		}
		
		knowledgeBase.write(System.out);
	}
	
	private OntModel ontology = null;
	private String ontBaseIRI = null;
	private OntModel knowledgeBase = null;
	private String kbBaseIRI = null;
	private int counter = 0;
	
	
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
