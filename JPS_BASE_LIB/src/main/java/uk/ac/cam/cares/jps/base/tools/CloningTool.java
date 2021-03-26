package uk.ac.cam.cares.jps.base.tools;

import java.time.LocalDateTime;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateProcessor;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

public class CloningTool {

	String strTag = "_Tag";	//Tag ending
	boolean splitUpdate;
	int stepSize;
	int countTotal;		//Total number of triple to clone
	
	//// SPARQL variables
	
	static ExprFactory exprFactory = new ExprFactory();
		
	// Count variable
	static String varCount = "count";
	
	// new s
	static Var newS = Var.alloc("newS");
	
	// old s, p, o
	static Var varS = Var.alloc("s");
	static Var varP = Var.alloc("p");
	static Var varO = Var.alloc("o");
	static Var varG = Var.alloc("g");
	static ExprVar exprS = new ExprVar(varS);
	static ExprVar exprP = new ExprVar(varP);
	static ExprVar exprO = new ExprVar(varO);
	static ExprVar exprG = new ExprVar(varG);
	// STR(?s)
	static Expr exprStrS = exprFactory.str(exprS);
	
	///////////////////////// Constructors
	
	/**
	 * Default constructor. Cloning split over multiple operations of 1 million triples
	 */
	public CloningTool(){
		//set defaults
		splitUpdate = true;
		stepSize = 1000000;
	}
	
	/**
	 * Constructor to set number of triples cloned per step. Cloning is split over multiple operations. 
	 * @param stepSize
	 */
	public CloningTool(int stepSize){
		//set defaults
		splitUpdate = true;
		this.stepSize = stepSize;
	}
	
	///////////////////////// Set variables
	
	/**
	 * Set number of triples cloned per step
	 * @param stepSize
	 */
	public void setCloneSize(int stepSize) {
		this.stepSize = stepSize;
	}
	
	/**
	 * Perform clone as a single operation
	 */
	public void setSingleStepClone() {
		this.splitUpdate = false;
	}

	///////////////////////// Clone methods
	
	/**
	 * Clone all triples from source repository to target repository.
	 * WARNING: any context will be lost.
	 * @param sourceKB
	 * @param targetKB
	 */  
	public void clone(KnowledgeBaseClient sourceKB, KnowledgeBaseClient targetKB) {
		clone(sourceKB, null, targetKB, null);
	}
	
	/**
	 * Clone a named graph from the source knowledge base to a named graph in the target knowledge base.
	 * @param sourceKB
	 * @param targetKB
	 * @param graph
	 */
	public void clone(KnowledgeBaseClient sourceKB, KnowledgeBaseClient targetKB, String graph) {
		clone(sourceKB, graph, targetKB, graph); 
	}
	
	/**
	 * Clone a named graph from the source knowledge base to a different named graph in the target knowledge base.
	 * @param sourceKB
	 * @param sourceGraph
	 * @param targetKB
	 * @param targetGraph
	 */
	public void clone(KnowledgeBaseClient sourceKB, String sourceGraph, KnowledgeBaseClient targetKB, String targetGraph) {
		
		WhereBuilder whereCountAll = new WhereBuilder()
				.addWhere(varS, varP, varO);		    
	    countTotal = countTriples(sourceKB, sourceGraph, whereCountAll);
	    
		if(splitUpdate == false) {
			singleStepClone(sourceKB, sourceGraph, targetKB, targetGraph);
		}else {
			//perform using single step process if count <= stepsize    
		    if(countTotal <= stepSize) {
		    	singleStepClone(sourceKB, sourceGraph, targetKB, targetGraph);
		    }else {
		    	performClone(sourceKB, sourceGraph, targetKB, targetGraph);
		    }
		}
	/*	
		if(!checkCount(targetKB, targetGraph)) {
			throw new JPSRuntimeException("CloningTool: check failed, counts do not match!");
		}
		*/
	}
	
	//TODO: check graph clone : is context lost?
	/**
	 * Clone graph from source knowledge base to target knowledge base in a single step.
	 * @param sourceKB
	 * @param targetKB
	 * @param graph
	 */
	public void singleStepClone(KnowledgeBaseClient sourceKB, String sourceGraph, KnowledgeBaseClient targetKB, String targetGraph) {
		
		//Get model using construct query
		Query construct = buildSparqlConstruct(sourceGraph);
		Model results = sourceKB.queryConstruct(construct);
		
		//Update target
		UpdateRequest update = buildSparqlUpdate(targetGraph, results);
		targetKB.executeUpdate(update);
	}
	
	//TODO: check graph clone
	//TODO: describe method
	/**
	 * Clone graph from source knowledge base to target knowledge base over multiple steps.
	 * @param sourceKB
	 * @param targetKB
	 * @param graph
	 */
	private void performClone(KnowledgeBaseClient sourceKB, String sourceGraph, KnowledgeBaseClient targetKB, String targetGraph) {
		
		createTag(sourceKB);

		// Count triples excluding blanks
		WhereBuilder whereCount = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addFilter(exprFilterOutBlanks());
	    int count = countTriples(sourceKB, sourceGraph, whereCount);
		int steps = count/stepSize;
		if(count%stepSize > 0) {steps++;}
				 
		for(int i = 0; i<steps; i++) {
			// Iterate tag
			Expr exprTagN = buildExprTagN(i);
			
			// Add tag to source
			WhereBuilder whereNotTagged = new WhereBuilder()
					.addWhere(varS, varP, varO)
					.addFilter(exprFilterOutBlanks())
					.addFilter(exprNotTagged())
					.addBind(exprFactory.iri(exprFactory.concat(exprStrS,exprTagN)), newS);
			UpdateRequest tagUpdate = buildTagUpdate(sourceGraph, whereNotTagged, stepSize, true); 
			sourceKB.executeUpdate(tagUpdate);
			
			// Get triples 
			WhereBuilder whereConstructTagged = new WhereBuilder()
					.addWhere(varS, varP, varO)
					.addFilter(exprFactory.strends(exprStrS, exprTagN));
			Query constructQuery = buildConstruct(sourceGraph, whereConstructTagged);
			Model triples = sourceKB.queryConstruct(constructQuery);
			
			// Remove tag from triples going to target
			WhereBuilder whereRemoveTag = new WhereBuilder()
					.addWhere(varS, varP, varO)
					.addBind(exprBindIriRemoveTag(exprTagN), newS);
			UpdateRequest removeTagUpdate = buildTagUpdate(null, whereRemoveTag, stepSize, false); //TODO stepsize here should be 0 to update all?
			Dataset dataset = DatasetFactory.create(triples); // put triple into the default graph of a temporary dataset
			UpdateProcessor updateExec = UpdateExecutionFactory.create(removeTagUpdate, dataset);
			updateExec.execute();
		
			// Insert triples to target
			UpdateRequest update = buildInsert(targetGraph, dataset.getDefaultModel());
			targetKB.executeUpdate(update);
		}
		
		// Clone everything that is not tagged i.e. blank nodes
		Expr filterTag = exprFactory.or(
				exprNotTagged(),
				exprFactory.isBlank(exprS));
		WhereBuilder whereConstruct = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addFilter(filterTag);
		Query constructQuery = buildConstruct(sourceGraph, whereConstruct);
		Model triples = sourceKB.queryConstruct(constructQuery);
		UpdateRequest update = buildInsert(targetGraph, triples);
		targetKB.executeUpdate(update);
		
		// Remove tags from source
		for(int i = 0; i<steps; i++) {
			Expr exprTagN = buildExprTagN(i);
			WhereBuilder whereTagged = new WhereBuilder()
					.addWhere(varS, varP, varO)
					.addFilter(exprFactory.strends(exprStrS, exprTagN))
					.addBind(exprBindIriRemoveTag(exprTagN), newS);
			UpdateRequest tagUpdate = buildTagUpdate(sourceGraph, whereTagged, stepSize, true);
			sourceKB.executeUpdate(tagUpdate);
		}
	}
	
	/**
	 * Creates a tag by hashing the source KB endpoint and current date and time.
	 * @param sourceKB
	 */
	private void createTag(KnowledgeBaseClient kbClient) {
		LocalDateTime dateTime = LocalDateTime.now();
		String name = kbClient.getQueryEndpoint() + dateTime.toString();
		int hash = name.hashCode();
		if(hash < 0) {hash *= -1;};
		strTag += String.valueOf(hash);
	}
	
	//// Checks
	
	/**
	 * Check the number of triples in target matches source
	 */
	public boolean checkCount(KnowledgeBaseClient kbClient, String graph) {

		WhereBuilder whereCount = new WhereBuilder()
				.addWhere(varS, varP, varO);
	    int count = countTriples(kbClient, graph, whereCount);
	    
	    return count == countTotal;
	}
	
	/**
	 * Check no tags remain
	 */
	public boolean checkNoTags(KnowledgeBaseClient kbClient, String graph) {

		WhereBuilder whereCount = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addFilter(exprTagged());
	    int count = countTriples(kbClient, graph, whereCount);
	    if(count==0) {
	    	return true;
	    }else {
	    	return false;
	    }
	}
	
	//// Count
	
	/**
	 * Count triples in knowledge base client matching where statement.
	 * @param source knowledge base client
	 * @param graph (can be null)
	 * @param where statement
	 * @return
	 */
	private int countTriples(KnowledgeBaseClient kbClient, String graph, WhereBuilder where) {
		String query = countQuery(graph, where);
		JSONArray result = kbClient.executeQuery(query);
	    JSONObject jsonobject = result.getJSONObject(0);
	    return jsonobject.getInt(varCount);
	}
	
	/**
	 * Build count query.
	 * @param graph
	 * @param whereFilter
	 * @return
	 */
	private String countQuery(String graph, WhereBuilder whereFilter){
		
		WhereBuilder where = null;

		if (graph != null) {	
			where = new WhereBuilder();
			// Graph
			String graphURI = "<" + graph + ">";
			where.addGraph(graphURI, whereFilter);
		}else {
			where = whereFilter;
		}
		
		String query = "SELECT (COUNT(*) AS ?"+varCount+") ";
		query += where.toString();
		return query;
	}

	//// Sparql query/update builder	
	
	/**
	 * Build construct query to get triples.
	 * @param graph
	 * @param where
	 * @return
	 */
	private Query buildConstruct(String graph, WhereBuilder where) {
		ConstructBuilder builder = new ConstructBuilder()
				.addConstruct(varS, varP, varO);
		
		// Add where 
		if (graph == null) {
			//Default graph
			builder.addWhere(where);
		}else {	
			//Named graph
			String graphURI = "<" + graph + ">";
			builder.addGraph(graphURI, where);	
		}
	
		return builder.build();
	}
	
	/**
	 * Build SPARQL update to insert triples
	 * @param graph
	 * @param triples
	 * @return
	 */
	private static UpdateRequest buildInsert(String graph, Model triples) {
		
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

	/**
	 * Build SPARQL update to tag triples
	 * @param graph
	 * @param where
	 * @param limit: number of triples to update
	 * @return
	 */
	private UpdateRequest buildTagUpdate(String graph, WhereBuilder where, int limit, boolean quads) {
		
		// subquery selects new and old triples
		SelectBuilder select = new SelectBuilder();
		
		select.addVar(varS)
		.addVar(varP)
		.addVar(varO)
		.addVar(newS);
		
		if(limit > 0) {
			select.setLimit(limit);
		}
		
		UpdateBuilder builder = new UpdateBuilder();
		
		// Add select subquery and optional graph
		if (quads == true ) {
			if (graph == null) {
				select.addVar(varG);
				select.addGraph(varG, where);
				builder.addInsert(varG, newS, varP, varO)
					.addDelete(varG, varS, varP, varO)
					.addSubQuery(select);
			}else {	
				String graphURI = "<" + graph + ">";
				select.addGraph(graphURI, where);
				// Graph
				builder.addInsert(graphURI, newS, varP, varO)
					.addDelete(graphURI, varS, varP, varO)
					.addSubQuery(select);	
			}
		}else {
			select.addWhere(where);
			builder.addInsert(newS, varP, varO)
				.addDelete(varS, varP, varO)
				.addSubQuery(select);
		}
		return builder.buildRequest();
	}

	//// Expressions for filters
	
	/**
	 * Create full tag with counter.
	 * @param counter i
	 * @return
	 */
	private Expr buildExprTagN(int i) {
		return exprFactory.asExpr("_"+Integer.toString(i)+strTag);
	}
	
	/**
	 * Expression to filer out triples with blank nodes.
	 * @return
	 */
	private Expr exprFilterOutBlanks() {
		// (!isblank(?s) && (!isblank(?p) && !isblank(?o))) 
		return exprFactory.and(exprFactory.not(exprFactory.isBlank(exprS)),
				exprFactory.and(exprFactory.not(exprFactory.isBlank(exprP)),
								exprFactory.not(exprFactory.isBlank(exprO))));
	}
	
	/**
	 * Expression to filter out tagged triples
	 * @return
	 */
	private Expr exprNotTagged() {
		return exprFactory.not(exprTagged());
	}
	
	/**
	 * Expression to filter tagged triples
	 * @return
	 */
	private Expr exprTagged() {
		return exprFactory.strends(exprStrS, strTag);
	}
	
	/**
	 * Bind new IRI with tag removed
	 * @param tag expression
	 * @return
	 */
	private Expr exprBindIriRemoveTag(Expr exprTagN) {
		return exprFactory.iri(exprFactory.replace(exprStrS, exprTagN, ""));
	}

	//// SPARQL query builder for single step clone
	
	/**
	 * Build sparql construct query to get triples.
	 * @param graph/context (optional)
	 * @return construct query
	 */
	private static Query buildSparqlConstruct(String graph) {
		
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