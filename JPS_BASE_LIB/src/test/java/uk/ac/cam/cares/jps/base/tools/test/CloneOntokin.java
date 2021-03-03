package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateProcessor;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;

class CloneOntokin {

	@Test
	void test() {
		
		String sourceQuery = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		String sourceUpdate = "http://localhost:8080/blazegraph/namespace/ontokin/update";

		String targetQuery = "http://localhost:8080/blazegraph/namespace/testSource/sparql";
		String targetUpdate = "http://localhost:8080/blazegraph/namespace/testSource/update";

		String targetQuery2 = "http://localhost:8080/blazegraph/namespace/testTarget/sparql";
		String targetUpdate2 = "http://localhost:8080/blazegraph/namespace/testTarget/update";
		
		KnowledgeBaseClient sourceKB = new RemoteKnowledgeBaseClient(sourceQuery,sourceUpdate);
		KnowledgeBaseClient targetKB = new RemoteKnowledgeBaseClient(targetQuery,targetUpdate);
		KnowledgeBaseClient targetKB2 = new RemoteKnowledgeBaseClient(targetQuery2,targetUpdate2);
		
		int stepSize = 500000;
		
		//count query 
		String query = countQuery(null);
		JSONArray result = sourceKB.executeQuery(query);
	    JSONObject jsonobject = result.getJSONObject(0);
	    int count = jsonobject.getInt(varCount);
		
	    int steps = count/stepSize;
		if(count%stepSize > 0) {steps++;}
	    
		for(int offset = 0; offset<steps; offset++) {
			//Get model using construct query
			Query construct = buildSparqlConstruct(stepSize, offset*stepSize);
			Model triples = sourceKB.queryConstruct(construct);		
			
			//build insert
			UpdateBuilder insertBuilder = new UpdateBuilder();
			insertBuilder.addInsert(triples);
			UpdateRequest insertRequest = insertBuilder.buildRequest();
			
			//insert triples to target1
			targetKB.executeUpdate(insertRequest);
		}
	}
	
	/**
	 * Build sparql construct query to get triples.
	 * @param graph/context (optional)
	 * @return construct query
	 */
	private static Query buildSparqlConstruct(int limit, int offset) {
		
		Var S = Var.alloc("S");
		Var P = Var.alloc("P");
		Var O = Var.alloc("O");
		
		ConstructBuilder builder = new ConstructBuilder()
				.addConstruct(S, P, O);
				
		// Add where 
		//Default graph
		builder.addWhere(S, P, O);
		builder.setLimit(limit);
		builder.setOffset(offset);
	
		return builder.build();
	}
	
	// old s, p, o
	static Var varS = Var.alloc("s");
	static Var varP = Var.alloc("p");
	static Var varO = Var.alloc("o");
	
	private String varCount = "count";
	private String countQuery(String graph) {
		
		WhereBuilder where = new WhereBuilder()
				.addWhere(varS, varP, varO);
		
		String query = "SELECT (COUNT(*) AS ?"+varCount+") ";
		
		// Add where 
		if (graph != null) {	
			// Graph
			String graphURI = "<" + graph + ">";
			where.addGraph(graphURI, where);
		}
		query += where.toString();
		
		return query;
	}


}
