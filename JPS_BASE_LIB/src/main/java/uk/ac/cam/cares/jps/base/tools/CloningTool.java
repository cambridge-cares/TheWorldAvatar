package uk.ac.cam.cares.jps.base.tools;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateRequest;

import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

public class CloningTool {

	//*************************************
	//sparql construct + insert
	
	/**
	 * Clone default graph from source knowledge base to default graph in target knowledge base.
	 * @param sourceKB
	 * @param targetKB
	 */
	public static void clone(KnowledgeBaseClient sourceKB, KnowledgeBaseClient targetKB) {
		
		clone(sourceKB, null, targetKB, null);
	}
	
	/**
	 * Clone named graph from source knowledge base to named graph in target knowledge base. (null for default graph)
	 * @param sourceKB
	 * @param sourceGraph
	 * @param targetKB
	 * @param targetGraph
	 */
	public static void clone(KnowledgeBaseClient sourceKB, String sourceGraph, KnowledgeBaseClient targetKB, String targetGraph) {
	
		//Get model using construct query
		Query construct = buildSparqlConstruct(sourceGraph);
		
		Model results = sourceKB.queryConstruct(construct);
		
		//Update target
		UpdateRequest update = buildSparqlUpdate(targetGraph, results);
		
		targetKB.executeUpdate(update);
	}
	
	/**
	 * Build sparql construct query to get graph.
	 * @param graph
	 * @return
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
	 * Build sparql update to insert graph.
	 * @param graph
	 * @param results
	 * @return
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
	
	//*************************************
	//fetch + put graph
	
	/**
	 * Clone default graph from source knowledge base to default graph in target knowledge base.
	 * @param sourceKB
	 * @param targetKB
	 */
	public static void cloneGSP(KnowledgeBaseClient sourceKB, KnowledgeBaseClient targetKB) {
		
		cloneGSP(sourceKB, null, targetKB, null);
	}
	
	/**
	 * Clone named graph from source knowledge base to named graph in target knowledge base. (null for default graph)
	 * @param sourceKB
	 * @param sourceGraph
	 * @param targetKB
	 * @param targetGraph
	 */
	public static void cloneGSP(KnowledgeBaseClient sourceKB, String sourceGraph, KnowledgeBaseClient targetKB, String targetGraph) {
		
		//get graph
		Model model = sourceKB.fetchGraph(sourceGraph);
		
		//put graph -- overrides existing data
		targetKB.putGraph(targetGraph, model);
	}
}
