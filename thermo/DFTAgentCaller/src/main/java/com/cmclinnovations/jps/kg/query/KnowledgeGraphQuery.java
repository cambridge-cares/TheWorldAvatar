package com.cmclinnovations.jps.kg.query;
import java.util.List;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.jps.agent.caller.configuration.AgentCallerConfiguration;
import com.cmclinnovations.jps.agent.caller.configuration.DFTAgentCallerProperty;

/**
 * Any specific facet of the JPS knowledge graph can be resided in multiple<br>
 * knowledge bases. This class provides the feature to send the current<br>
 * query to be performed on all the knowledge bases requested by the<br>
 * caller method or agent. 
 * 
 * @author msff2
 *
 */
public class KnowledgeGraphQuery {
	
	/**
	 * List of SPARQL end points against which the current query will be<br>
	 * submitted.  
	 */
	private List<String> endpoints;
	
	/**
	 * The current SPARQL query. 
	 */
	private String query;
	
	public static ApplicationContext applicationContext;
	public static DFTAgentCallerProperty dftAgentCallerProperty;
	
	/**
	 * Construct of this class to initialise and call methods.
	 * 
	 * @param endpoints the list of SPARQL endpoints.
	 * @param query the current query.
	 */
	public KnowledgeGraphQuery(List<String> endpoints, String query){
		this.endpoints = endpoints;
		this.query = query;
        if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(AgentCallerConfiguration.class);
		}
		if (dftAgentCallerProperty == null) {
			dftAgentCallerProperty = applicationContext.getBean(DFTAgentCallerProperty.class);
		}
	}

	public List<String> getEndpoints() {
		return endpoints;
	}

	public void setEndpoints(List<String> endpoints) {
		this.endpoints = endpoints;
	}

	public String getQuery() {
		return query;
	}

	public void setQuery(String query) {
		this.query = query;
	}
	
}
