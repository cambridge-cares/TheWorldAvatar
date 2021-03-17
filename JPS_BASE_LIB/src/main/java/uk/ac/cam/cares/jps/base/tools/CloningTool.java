package uk.ac.cam.cares.jps.base.tools;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateRequest;

import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

/**
 * This tool clones a graph from one repository to another.
 * 
 * @author Casper Lindberg
 *
 */
public class CloningTool {
	
	/**
	 * Clone all triples from source repository to target repository. Note: context is lost!
	 * @param sourceKB
	 * @param targetKB
	 */
	/* // It is better that the graph is explicitly specified.  
	public static void clone(KnowledgeBaseClient sourceKB, KnowledgeBaseClient targetKB) {
		
		clone(sourceKB, null, targetKB, null);
	}
	*/
	
	/**
	 * Clone a named graph from the source knowledge base to a named graph in the target knowledge base.
	 * @param sourceKB
	 * @param sourceGraph
	 * @param targetKB
	 * @param targetGraph
	 */
	public static void clone(KnowledgeBaseClientInterface sourceKB, String sourceGraph, KnowledgeBaseClientInterface targetKB, String targetGraph) {
	
		//Get model using construct query
		Query construct = buildSparqlConstruct(sourceGraph);
		Model results = sourceKB.executeConstruct(construct);
		
		//Update target
		UpdateRequest update = buildSparqlUpdate(targetGraph, results);
		targetKB.executeUpdate(update);
	}
	
	/**
	 * Build sparql construct query to get triples.
	 * @param graph/context (optional)
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
	 * Build sparql update to insert triples.
	 * @param graph
	 * @param results
	 * @return updaterequest
	 */
	private static UpdateRequest buildSparqlUpdate(String graph, Model results) {
		
		// Build update
		UpdateBuilder builder = new UpdateBuilder();
				
		// Add insert
		if (graph == null) {
			//Default graph
			builder.addInsert(results);
		}else {	
			//Named graph
			String graphURI = "<" + graph + ">";
			builder.addInsert(graphURI, results);	
		}
	
		return builder.buildRequest();
	}
}
