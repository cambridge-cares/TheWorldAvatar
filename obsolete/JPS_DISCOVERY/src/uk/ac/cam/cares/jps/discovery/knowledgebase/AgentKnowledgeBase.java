package uk.ac.cam.cares.jps.discovery.knowledgebase;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Resource;

import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class AgentKnowledgeBase {
	
	public static final String ONTOAGENT_BASE_IRI = "http://www.theworldavatar.com/OntoAgent";
	public static final String ONTOAGENT_ONTOLOGY_IRI = ONTOAGENT_BASE_IRI + "/OntoAgent.owl";
	
	private static AgentKnowledgeBase instance = null;

	private Logger logger = LoggerFactory.getLogger(AgentKnowledgeBase.class);
	private OntModel knowledgebase = null;
	
	private AgentKnowledgeBase () {
	}
	
	public static synchronized AgentKnowledgeBase getInstance() {
		if (instance == null) {
			instance = new AgentKnowledgeBase();
			String dir = getDirForAgentKnowledgesBase();
			instance.knowledgebase = JenaHelper.createModel(dir);
			instance.logger.info("AgentKnowledgeBase was created");
		}
		return instance;
	}
	
	public static synchronized AgentKnowledgeBase createNewInstanceWithoutReading() {
		instance = new AgentKnowledgeBase();
		instance.knowledgebase = JenaHelper.createModel();
		return instance;
	}
	
	public static String getFileForAgentOntology() {
		return null; //AgentLocator.getPathToJpsDataOntologyDir() + "/OntoAgent/OntoAgent.owl";
	}
	
	public static String getDirForAgentKnowledgesBase() {
		return null; //AgentLocator.getPathToJpsDataKnowledgeDir() + "/OntoAgent";
	}
	
	public synchronized void read(String path) {
		JenaHelper.read(path, knowledgebase);
	}
	
	public synchronized void add(Agent agent) {
		
		String sparql = "PREFIX jpsago:<http://www.theworldavatar.com/OntoAgent/OntoAgent.owl#> "
				+ "SELECT ?agent "
				+ "WHERE { "
				+ "?agent a jpsago:Agent ."
				+ "?agent jpsago:hasName \"" + agent.getName() + "\" ."
				+ "}";

		ResultSet rs = query(sparql);
		
		if (rs.hasNext()) {
			throw new JPSRuntimeException("can't add agent to knowledge base because an agent with the same already exists, name = " + agent.getName());
		}
		
		String serializedAgent = OWLSerializer.getInstance().convertToString(agent);
		JenaHelper.readFromString(serializedAgent, knowledgebase);
	}
	
	public synchronized ResultSet query(String sparql) {
		return JenaHelper.query(sparql, knowledgebase);
	}

	public Collection<Agent> getAllAgents() {
		
		List<Agent> result = new ArrayList<Agent>();
		
		//TODO-AE URGENT since we can use the uri in sparql we might not need the iri property for agent anymore
		String sparql = "PREFIX jpsago:<http://www.theworldavatar.com/OntoAgent/OntoAgent.owl#> "
				+ "SELECT ?agent ?name "
				+ "WHERE { "
				+ "?agent a jpsago:Agent ."
				//+ "?agent jpsago:hasId ?id ."
				+ "?agent jpsago:hasName ?name ."
				+ "}";

		ResultSet rs = query(sparql);
		
		QuerySolution sol = null;
		while (rs.hasNext()) {
			Agent agent = new Agent();
			sol = rs.nextSolution();
			String name = sol.getLiteral("name").getString();
			agent.setName(name);
			String agentURI = sol.getResource("agent").getURI();
			List<AgentServiceDescription> descriptions = createListOfAgentServiceDescriptions(agentURI);
			agent.getDescriptions().addAll(descriptions);
			result.add(agent);
		}
		
		return result;
	}
	
	private List<AgentServiceDescription> createListOfAgentServiceDescriptions(String agentIRI) {
		
		List<AgentServiceDescription> result = new ArrayList<AgentServiceDescription>();
		
		String sparql = "PREFIX jpsago:<http://www.theworldavatar.com/OntoAgent/OntoAgent.owl#> "
				+ "SELECT ?descr "
				+ "WHERE { "
				+ "<" + agentIRI + "> jpsago:hasAgentServiceDescription ?descr"
				+ "}";
			
		ResultSet rs = query(sparql);
		
		QuerySolution sol = null;
		while (rs.hasNext()) {	
			sol = rs.nextSolution();
			Resource descr = sol.getResource("descr");
			AgentServiceDescription description = createAgentServiceDescription(descr.getURI());
			result.add(description);
		}
		
		return result;
	}
	
	private AgentServiceDescription createAgentServiceDescription(String descriptionIRI) {
		
		AgentServiceDescription result = new AgentServiceDescription();
		
		String sparql = "PREFIX jpsago:<http://www.theworldavatar.com/OntoAgent/OntoAgent.owl#> "
		+ "SELECT  ?index ?type ?key ?value "
		+ "WHERE { "
		+ "<" + descriptionIRI + "> jpsago:hasKeyValuePair ?pair ."
		+ "?pair a ?type ."
		+ "?pair jpsago:hasIndex ?index ."
		+ "?pair jpsago:hasKey ?key . "
		+ "OPTIONAL { ?pair jpsago:hasValue ?value . }"	
		+ "} "
		+ "ORDER BY ASC(?index)";
		
		ResultSet rs = query(sparql);
		QuerySolution sol = null;
		while(rs.hasNext()) {
			sol = rs.nextSolution();
			int index = sol.getLiteral("index").getInt();
			String typeLocalName = sol.getResource("type").getLocalName();
			String key = sol.getLiteral("key").getString();
			Literal literal = sol.getLiteral("value");
			String value = (literal == null)? null : literal.getString();
			
			System.out.println(index + " " + typeLocalName + " " + key + " " + value);
			
			Parameter param = new Parameter(key, value);
			String type = getConceptName(typeLocalName);
			if ("Property".equals(type)) {
				result.getProperties().add(param);
			} else if ("InputParameter".equals(type)) {
				result.getInputParameters().add(param);
			} else if ("OutputParameter".equals(type)) {
				result.getOutputParameters().add(param);
			} else {
				throw new JPSRuntimeException("Parameter type is unknown, type = " + typeLocalName);
			}
		}
		
		return result;
	}

	private String getConceptName(String name) {
		int index = name.lastIndexOf(':');
		if (index == -1) {
			index = name.lastIndexOf('#');
		}
		if (index == -1) {
			return name;
		}
		return name.substring(index+1, name.length()-1);
	}
}
