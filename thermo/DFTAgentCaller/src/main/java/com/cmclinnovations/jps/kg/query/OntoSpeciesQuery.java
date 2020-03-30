package com.cmclinnovations.jps.kg.query;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.jps.agent.caller.AgentCaller;
import com.cmclinnovations.jps.agent.caller.DFTAgentCallerException;
import com.cmclinnovations.jps.agent.caller.Utils;
import com.cmclinnovations.jps.agent.caller.configuration.AgentCallerConfiguration;
import com.cmclinnovations.jps.agent.caller.configuration.DFTAgentCallerProperty;

/**
 * Queries the JPS OntoSpecies knowledge graph.
 * 
 * @author msff2
 *
 */
public class OntoSpeciesQuery extends AgentCaller{
	/**
	 * The default constructor of this class.
	 */
	public OntoSpeciesQuery(){
        if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(AgentCallerConfiguration.class);
		}
		if (dftAgentCallerProperty == null) {
			dftAgentCallerProperty = applicationContext.getBean(DFTAgentCallerProperty.class);
		}
	}
	
	/**
	 * Queries the OntoSpcies knowledge graph.
	 * 
	 * @return
	 * @throws DFTAgentCallerException
	 * @throws Exception
	 */
	public Set<String> queryOntoSpciesKG() throws DFTAgentCallerException, Exception{
		List<String> endpoints = Utils.getEndpoints(dftAgentCallerProperty.getEndpointOntoSpecies());
		if(endpoints == null){
			throw new DFTAgentCallerException("DFTAgentCaller: ontospecies endpoints are not correctly formatted.");
		}
		KnowledgeGraphQuery kgQuery = new KnowledgeGraphQuery(endpoints, formSpeciesQuery());
		return kgQuery.performQuery();
	}
	
	/**
	 * Forms the query that can retrieve all species IRIs from the Onto-<br>
	 * Species knowledge graph across multiple repositories.
	 * 
	 * @return
	 */
	private String formSpeciesQuery(){
		String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
				+ "PREFIX ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>\n"
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>\n"
				+ "SELECT ?species "
				+ "WHERE { "
				+ "?species rdf:type ontospecies:Species . "
				+ "}";		
		return query;

	}
}
