package uk.ac.cam.cares.jps.base.tools;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateRequest;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * Tool for cloning triples (not quads) in a single step 
 * using a SPARQL construct query and SPARQL update. 
 * <p>
 * NOTE: <br> 
 * - This tool should only be used for small stores. <br>
 * - This tool does not support cloning a quad store; however,
 * 	 triples can be cloned to/from a named graph.
 * 
 * @author csl37
 *
 */
public class SingleStepCloningTool {
	
	static final Var varG = Var.alloc("g");
	static final Var varS = Var.alloc("s");
	static final Var varP = Var.alloc("p");
	static final Var varO = Var.alloc("o");
	
	/**
	 * Clone triples from source store to target store.
	 * @param sourceStore
	 * @param targetStore
	 */
	public void clone(StoreClientInterface sourceStore, StoreClientInterface targetStore) {
		
		clone(sourceStore, null, targetStore, null);
	}
	
	/**
	 * Clone a named graph from source store to a named graph (or default/null graph) in target store.
	 * @param sourceStore
	 * @param sourceGraph
	 * @param targetStore
	 * @param targetGraph
	 */
	public void clone(StoreClientInterface sourceStore, String sourceGraph, StoreClientInterface targetStore, String targetGraph) {
		
		//Get model using construct query
		Query construct = buildSparqlConstruct(sourceGraph);
		Model results = sourceStore.executeConstruct(construct);
		
		//Update target
		UpdateRequest update = buildSparqlUpdate(targetGraph, results);
		targetStore.executeUpdate(update);
	}
	
	/**
	 * Build sparql construct query to get quads.
	 * @param graph/context (optional)
	 * @return construct query
	 */
	public static Query buildSparqlConstruct(String graph) {
		
		ConstructBuilder builder = new ConstructBuilder()
				.addConstruct(varS, varP, varO);
				
		// Add where 
		if (graph == null) {
			//Default graph
			builder.addWhere(varS, varP, varO);
		}else {	
			//Named graph
			String graphURI = "<" + graph + ">";
			builder.addGraph(graphURI, varS, varP, varO);	
		}
	
		return builder.build();
	}
	
	/**
	 * Build sparql update to insert quads.
	 * @param graph
	 * @param results
	 * @return updaterequest
	 */
	public static UpdateRequest buildSparqlUpdate(String graph, Model results) {
		
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
