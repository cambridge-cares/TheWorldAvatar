package com.cmclinnovations.jps.kg.query;

import java.util.List;
import java.util.Set;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.jps.agent.caller.AgentCaller;
import com.cmclinnovations.jps.agent.caller.DFTAgentCallerException;
import com.cmclinnovations.jps.agent.caller.Utils;
import com.cmclinnovations.jps.agent.caller.configuration.AgentCallerConfiguration;
import com.cmclinnovations.jps.agent.caller.configuration.DFTAgentCallerProperty;

/**
 * Queries the JPS OntoCompChem knowledge graph.
 * 
 * @author msff2
 *
 */
public class OntoCompChemQuery extends AgentCaller {
	public OntoCompChemQuery() {
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(AgentCallerConfiguration.class);
		}
		if (dftAgentCallerProperty == null) {
			dftAgentCallerProperty = applicationContext.getBean(DFTAgentCallerProperty.class);
		}
	}
	
	/**
	 * Queries the OntoCompChem knowledge graph.
	 *  
	 * @return
	 * @throws DFTAgentCallerException
	 * @throws Exception
	 */
	public Set<String> queryOntoCompChemKG() throws DFTAgentCallerException, Exception {
		List<String> endpoints = Utils.getEndpoints(dftAgentCallerProperty.getEndpointOntoCompChem());
		if (endpoints == null) {
			throw new DFTAgentCallerException("DFTAgentCaller: ontocompcehm endpoints are not formatted correctly.");
		}
		KnowledgeGraphQuery kgQuery = new KnowledgeGraphQuery(endpoints, formOntoCompChemSpeicesExtractionQuery());
		return kgQuery.performQuery();
	}

	/**
	 * Forms a query to extract species IRIs which are codified in the<br>
	 * OntoCompChem knowledge graph.  
	 * 
	 * @return
	 */
	private String formOntoCompChemSpeicesExtractionQuery() {
		String query = "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>\n"
				+ "SELECT ?uniqueSpecies \n" + "WHERE { \n"
				+ "?calculation ontocompchem:hasUniqueSpecies ?uniqueSpecies . \n" + "}";
		return query;
	}

}
