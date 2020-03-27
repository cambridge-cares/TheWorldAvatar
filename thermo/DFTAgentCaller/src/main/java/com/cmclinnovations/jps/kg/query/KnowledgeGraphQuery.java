package com.cmclinnovations.jps.kg.query;
import java.util.Arrays;
import java.util.List;

import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;

/**
 * Any specific facet of the JPS knowledge graph can be resided in multiple<br>
 * knowledge bases. For example, species, quantum calculations and kinetic<br>
 * mechanisms are facets. This class provides the feature to send the current<br>
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
	
	/**
	 * Construct of this class to initialise and call methods.
	 * 
	 * @param endpoints the list of SPARQL endpoints.
	 * @param query the current query.
	 */
	public KnowledgeGraphQuery(List<String> endpoints, String query){
		this.endpoints = endpoints;
		this.query = query;
	}
	
	/**
	 * Performs the current query.
	 *  
	 * @throws Exception
	 */
	public void performQuery() throws Exception{
		Repository repo = FedXFactory.createSparqlFederation(getEndpoints());
	repo.init();

	try (RepositoryConnection conn = repo.getConnection()) {
		TupleQuery query = conn.prepareTupleQuery(QueryLanguage.SPARQL, getQuery());
		try (TupleQueryResult res = query.evaluate()) {

			while (res.hasNext()) {
				System.out.println(res.next());
			}
		}
	}

	repo.shutDown();
	System.out.println("Done.");
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
