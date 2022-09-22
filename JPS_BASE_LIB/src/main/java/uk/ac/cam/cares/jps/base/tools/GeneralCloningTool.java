package uk.ac.cam.cares.jps.base.tools;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * Cloning Tool
 * 
 * Two cloning methods are implemented: a single step cloning method suitable for cloning small 
 * stores and a method which splits a large cloning operation into multiple smaller ones
 * by tagging groups of triples in the source store. 
 * The single step clone can be used by first calling "setSingleStepClone" followed by a "clone" 
 * method, or alternatively by calling the method "singleStepClone" directly.
 * The split method is used be default, unless the total number of triples (or quads) in the store
 * is below the step size, set using "setCloneSize".  
 * Two methods: "checkNoTags" and "checkCount" are provided and can be called to check that no tags 
 * remain after cloning and that the total count remains unchanged.
 * NOTE: If the sourceKB is a remote triple store (rather than quad store) then "setTripleStore()" 
 * must be set for the tool to function.
 * 
 * @author csl37
 *
 */
public class GeneralCloningTool {

	boolean splitUpdate;
	int stepSize;
	int countTotal;			//Total number of triple to clone
	boolean quads = true;	//Is the source KBClient a quads store?
	
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
	public GeneralCloningTool(){
		//set defaults
		splitUpdate = true;
		stepSize = 1000000;
	}
	
	/**
	 * Constructor to set number of triples cloned per step. Cloning is split over multiple operations. 
	 * @param stepSize
	 */
	public GeneralCloningTool(int stepSize){
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

	/**
	 * Set source KBClient is a triple store 
	 */
	public void setTripleStore() {
		this.quads = false;
	}
	
	/**
	 * Set source KBClient is a quad store
	 */
	public void setQuadsStore() {
		this.quads = true;
	}
	///////////////////////// Clone methods
	
	/**
	 * Clone all triples/quads from source repository to target repository.
	 * WARNING: any context will be lost in the target.
	 * @param sourceKB
	 * @param targetKB
	 */  
	public void clone(StoreClientInterface sourceKB, StoreClientInterface targetKB) {
		clone(sourceKB, null, targetKB, null);
	}
		
	/**
	 * Clone a named graph from the source knowledge base to a different named graph in the target knowledge base.
	 * @param sourceKB
	 * @param sourceGraph
	 * @param targetKB
	 * @param targetGraph
	 */
	public void clone(StoreClientInterface sourceKB, String sourceGraph, StoreClientInterface targetKB, String targetGraph) {
		
		WhereBuilder whereCountAll = new WhereBuilder()
				.addWhere(varS, varP, varO);		    
	    countTotal = countTriples(sourceKB, sourceGraph, whereCountAll);
	    
		if(splitUpdate == false) {
			SingleStepCloningTool singleStepCloningTool = new SingleStepCloningTool(); 
			singleStepCloningTool.clone(sourceKB, sourceGraph, targetKB, targetGraph);
		}else {
			//perform using single step process if count <= stepsize    
		    if(countTotal <= stepSize) {
		    	SingleStepCloningTool singleStepCloningTool = new SingleStepCloningTool(); 
				singleStepCloningTool.clone(sourceKB, sourceGraph, targetKB, targetGraph);
		    }else {
		    	SourceTaggingCloningTool sourceTaggingCloningTool = new SourceTaggingCloningTool(stepSize, quads); 
		    	sourceTaggingCloningTool.clone(sourceKB, sourceGraph, targetKB, targetGraph);
		    }
		}
	}
	
	
	
	
	//// Count
	
	/**
	 * Count triples in knowledge base client matching the where statement.
	 * @param source knowledge base client
	 * @param graph (can be null)
	 * @param where statement
	 * @return
	 */
	private int countTriples(StoreClientInterface kbClient, String graph, WhereBuilder where) {
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
}