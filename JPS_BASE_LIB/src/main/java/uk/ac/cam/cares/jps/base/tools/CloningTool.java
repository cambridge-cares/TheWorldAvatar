package uk.ac.cam.cares.jps.base.tools;

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

	String strTag = "_Tag"; 
	boolean splitUpdate = true;
	int stepSize = 1000000;
	
	//// Sparql variables
	
	// Expression factory
	ExprFactory exprFactory = new ExprFactory();
		
	// Count sparql variable
	static String varCount = "count";
	
	// new s	
	static Var newS = Var.alloc("newS");
	
	// old s, p, o
	static Var varS = Var.alloc("s");
	static Var varP = Var.alloc("p");
	static Var varO = Var.alloc("o");
	static ExprVar exprS = new ExprVar(varS);
	static ExprVar exprP = new ExprVar(varP);
	static ExprVar exprO = new ExprVar(varO);
	
	
	public void setUpdateSize(int stepSize) {
		this.stepSize = stepSize;
	}
	
	public void perfrom(KnowledgeBaseClient sourceKB, KnowledgeBaseClient targetKB, String graph) {
		
		
		////////////////////////////////  0
		
		//count triples excluding blanks
		WhereBuilder whereCount = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addFilter(exprFilterOutBlanks());
		String query = countQuery(graph, whereCount);
		
		JSONArray result = sourceKB.executeQuery(query);
	    JSONObject jsonobject = result.getJSONObject(0);
	    int count = jsonobject.getInt(varCount);
		
		int steps = count/stepSize;
		if(count%stepSize > 0) {steps++;}
		
		//////////////////////////////// Loop
		
		//loop over splits 
		for(int i = 0; i<steps; i++) {
			
			//Iterate tag
			Expr exprTagN = buildExprTagN(i);
			
			// tag
			WhereBuilder whereNotTagged = whereNotTagged(exprTagN);
			UpdateRequest tagUpdate = buildTagUpdate(graph, whereNotTagged, stepSize); 
			sourceKB.executeUpdate(tagUpdate);
			
			// construct model 
			WhereBuilder whereConstruct = whereConstruct(exprTagN);
			Query constructQuery = buildConstruct(graph, whereConstruct);
			Model triples = sourceKB.queryConstruct(constructQuery);
			
			//remove tag from model going to target
			WhereBuilder whereTagged = whereTaggedAll(exprTagN);
			UpdateRequest removeTagUpdate = buildTagUpdate(graph, whereTagged, stepSize);
			
			Dataset dataset = DatasetFactory.create(triples);
			UpdateProcessor updateExec = UpdateExecutionFactory.create(removeTagUpdate, dataset);
			updateExec.execute();
		
			//insert to target
			UpdateRequest update = buildInsert(graph, dataset.getDefaultModel());
			targetKB.executeUpdate(update);
		}
		
		//Clone everything not tagged (blank nodes)
		// construct model 
		WhereBuilder whereConstruct = whereConstructAll();
		Query constructQuery = buildConstruct(graph, whereConstruct);
		Model triples = sourceKB.queryConstruct(constructQuery);
		
		//insert to target
		UpdateRequest update = buildInsert(graph, triples);
		targetKB.executeUpdate(update);
		
		//loop to remove tags from source
		for(int i = 0; i<steps; i++) {
			//Iterate tag
			Expr exprTagN = buildExprTagN(i);
			
			// remove tags
			WhereBuilder whereTagged = whereTagged(exprTagN);
			UpdateRequest tagUpdate = buildTagUpdate(graph, whereTagged, stepSize);
			sourceKB.executeUpdate(tagUpdate);
		}
		
		//count triples
		whereCount = new WhereBuilder()
				.addWhere(varS, varP, varO);
		query = countQuery(graph, whereCount);
		
		result = sourceKB.executeQuery(query);
	    jsonobject = result.getJSONObject(0);
	    int countSource = jsonobject.getInt(varCount);
	    
	    result = targetKB.executeQuery(query);
	    jsonobject = result.getJSONObject(0);
	    int countTarget = jsonobject.getInt(varCount);
	    
	    System.out.println("Source count:"+countSource);
	    System.out.println("Target count:"+countTarget);
	}
	
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
	
	private WhereBuilder whereConstructAll() {
		
		Expr exprStrS = exprFactory.str(exprS);
		Expr filterTag = exprFactory.or(exprFactory.not(exprFactory.strends(exprStrS, strTag)),
				exprFactory.isBlank(exprS));
		
		WhereBuilder where = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addFilter(filterTag);
		
		return where;
	}

	private WhereBuilder whereConstruct(Expr exprTagN) {
		
		Expr exprStrS = exprFactory.str(exprS);
		Expr filterTag = exprFactory.strends(exprStrS, exprTagN);
		
		WhereBuilder where = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addFilter(filterTag);
		
		return where;
	}
	
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
	
	public UpdateRequest buildTagUpdate(String graph, WhereBuilder where, int limit) {
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
		if (graph == null) {
			select.addWhere(where);
			builder.addInsert(newS, varP, varO)
				.addDelete(varS, varP, varO)
				.addSubQuery(select);
		}else {	
			select.addGraph(graph, where);
			// Graph
			String graphURI = "<" + graph + ">";
			builder.addInsert(graphURI, newS, varP, varO)
				.addDelete(graphURI, varS, varP, varO)
				.addSubQuery(select);	
		}
		
		return builder.buildRequest();
	}

	
	private WhereBuilder whereTaggedAll(Expr exprTagN) {
			
		Expr exprStrS = exprFactory.str(exprS);
		
		Expr bind = exprFactory.iri(exprFactory.replace(exprStrS, exprTagN, ""));
				
		WhereBuilder where = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addBind(bind, newS);
		
		return where;
	}
	
	private WhereBuilder whereNotTagged(Expr exprTagN) {
	
		Expr filterOutBlanks = exprFilterOutBlanks();
	
		Expr exprStrS = exprFactory.str(exprS);
		Expr filterNoTag = exprFactory.not(exprFactory.strends(exprStrS, strTag));
		
		Expr bind = exprFactory.iri(exprFactory.concat(exprStrS,exprTagN));
				
		WhereBuilder where = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addFilter(filterOutBlanks)
				.addFilter(filterNoTag)
				.addBind(bind, newS);
		
		return where;
	}
	
	public WhereBuilder whereTagged(Expr exprTagN) {
		
		//Expr filterOutBlanks = exprFilterOutBlanks();	//not needed
	
		Expr exprStrS = exprFactory.str(exprS);
		Expr filterTag = exprFactory.strends(exprStrS, exprTagN); //filter necessary?
		
		Expr bind = exprFactory.iri(exprFactory.replace(exprStrS, exprTagN, ""));
				
		WhereBuilder where = new WhereBuilder()
				.addWhere(varS, varP, varO)
				.addFilter(filterTag)
				.addBind(bind, newS);
		
		return where;
	}
	
	private Expr buildExprTagN(int i) {
		return exprFactory.asExpr("_"+Integer.toString(i)+strTag);
	}
	
	private Expr exprFilterOutBlanks() {
		// FILTER (!isblank(?s) && (!isblank(?p) && !isblank(?o))) 
		return exprFactory.and(exprFactory.not(exprFactory.isBlank(exprS)),
				exprFactory.and(exprFactory.not(exprFactory.isBlank(exprP)),
								exprFactory.not(exprFactory.isBlank(exprO))));
	}
	
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