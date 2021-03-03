package uk.ac.cam.cares.jps.base.tools;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateRequest;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

/**
 * This tool clones triples from a source KnowledgeBaseClient to a graph in a  
 * target KnowledgeBaseClient. Either a graph url (named or null) in the source 
 * KnowledgeBaseClient or a sparql construct query is specified.  
 * 
 * @author Casper Lindberg
 *
 */
public class CloningTool {
	
	/**
	 * Clone triples from the source knowledge base that match the supplied sparql query  
	 * pattern to a graph (named or default) in the target knowledge base. 
	 * @param sourceKB 
	 * @param constructQuery
	 * @param targetKB
	 * @param targetGraph url (null for default graph)
	 */
	public static void clone(KnowledgeBaseClient sourceKB, Query constructQuery, KnowledgeBaseClient targetKB, String targetGraph) {
		
		if(!constructQuery.isConstructType()) {
			throw new JPSRuntimeException("CloningTool: query must be CONSTRUCT type!");
		}
		
		performClone(sourceKB, constructQuery, targetKB, targetGraph);
	}
	
	/**
	 * Clone a named graph from the source knowledge base to a graph (named or default) 
	 * in the target knowledge base.
	 * @param sourceKB
	 * @param sourceGraph url (null for default graph)
	 * @param targetKB
	 * @param targetGraph url (null for default graph)
	 */
	public static void clone(KnowledgeBaseClient sourceKB, String sourceGraph, KnowledgeBaseClient targetKB, String targetGraph) {
	
		//Get default construct query
		Query constructQuery = buildSparqlConstruct(sourceGraph);
		
		performClone(sourceKB, constructQuery, targetKB, targetGraph);
	}
	
	/**
	 * Perform clone using sparql construct query and update.
	 * @param sourceKB 
	 * @param constructQuery
	 * @param targetKB
	 * @param targetGraph url
	 */
	private static void performClone(KnowledgeBaseClient sourceKB, Query constructQuery, KnowledgeBaseClient targetKB, String targetGraph) {
		
		//Get model from source
		Model triples = sourceKB.queryConstruct(constructQuery);
		
		//Update target
		UpdateRequest update = buildSparqlUpdate(targetGraph, triples);
		targetKB.executeUpdate(update);
	}
	
	/**
	 * Build a sparql construct query to get triples.
	 * @param graph/context url (can be null)
	 * @return construct query
	 */
	private static Query buildSparqlConstruct(String graph) {
		
		Var S = Var.alloc("S");
		Var P = Var.alloc("P");
		Var O = Var.alloc("O");
		
		ConstructBuilder builder = new ConstructBuilder()
				.addConstruct(S, P, O);
				
		// Add where 
		if (graph == null) {
			//Default graph
			builder.addWhere(S, P, O);
		}else {	
			//Named graph
			String graphURI = "<" + graph + ">";
			builder.addGraph(graphURI, S, P, O);	
		}
	
		return builder.build();
	}
	
	/**
	 * Build a sparql update to insert triples into a graph (named or default).
	 * @param graph url (can be null)
	 * @param triples
	 * @return UpdateRequest
	 */
	private static UpdateRequest buildSparqlUpdate(String graph, Model triples) {
		
		UpdateBuilder builder = new UpdateBuilder();
				
		// Add insert
		if (graph == null) {
			//Default graph
			builder.addInsert(triples);
		}else {	
			//Named graph
			String graphURI = "<" + graph + ">";
			builder.addInsert(graphURI, triples);	
		}
	
		return builder.buildRequest();
	}
}