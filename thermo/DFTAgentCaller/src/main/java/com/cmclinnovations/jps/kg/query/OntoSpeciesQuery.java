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
	
	public HashSet<String> queryOntoSpciesKG() throws DFTAgentCallerException, Exception{
		List<String> endpoints = Utils.getEndpoints(dftAgentCallerProperty.getEndpointOntoSpecies());
		if(endpoints == null){
			throw new DFTAgentCallerException("DFTAgentCaller: endpoints are not correctly formatted.");
		}
		KnowledgeGraphQuery kgQuery = new KnowledgeGraphQuery(endpoints, formSpeciesQuery());
		kgQuery.performQuery();
		return null;
	}
	
	/**
	 * Forms the query that can retrieve all species IRIs from the Onto-<br>
	 * Species knowledge graph across multiple repositories.
	 * 
	 * @return
	 */
	private String formSpeciesQuery(){
		String query = "PREFIX owl: <http://www.w3.org/2002/07/owl#>\n"
				+ "PREFIX ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>\n"
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>\n"
				+ "SELECT ?species "
				+ "WHERE { "
				+ "?species owl:type ontospecies:Species . "
				+ "}";		
		return query;

	}
}
