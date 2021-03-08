package uk.ac.cam.cares.jps.base.tools;

import java.sql.SQLException;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.E_Conditional;
import org.apache.jena.sparql.expr.E_Equals;
import org.apache.jena.sparql.expr.E_IsBlank;
import org.apache.jena.sparql.expr.E_LogicalOr;
import org.apache.jena.sparql.expr.E_Regex;
import org.apache.jena.sparql.expr.E_Str;
import org.apache.jena.sparql.expr.E_StrReplace;
import org.apache.jena.sparql.expr.E_URI;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

public class RenamingTool {
	
	/*
	 * @param strMatch			match string in s/p/o to be renamed. Usually the same as strTarget.
	 * @param strTarget 		target string to be replaced in matching s/p/o.
	 * @param strReplacement 	replacement string.
	 * @param exprFilter 		additional filter expression can be applied to the update
	 * @param splitUpdate 		(boolean) split the renaming sparql update into multiple smaller updates
	 * @param stepSize			number of triples processed per update
	*/
	String strMatch;
	String strTarget;
	String strReplacement;
	Expr exprFilter = null;
	boolean splitUpdate = true;
	int stepSize = 1000000;
	
	//// Constructors
	
	/**
	 * @param strTarget 		target string or uri to be replaced in matching s/p/o
	 * @param strReplacement 	replacement string or uri
	 * */
	public RenamingTool(String strTarget, String strReplacement) {
		this.strMatch = strTarget;
		this.strTarget = strTarget;
		this.strReplacement = strReplacement;	
	}
	
	/**
	 * @param strMatch			match string in s/p/o to be renamed
	 * @param strTarget 		target string or uri to be replaced in matching s/p/o
	 * @param strReplacement 	replacement string or uri
	 * */
	public RenamingTool(String strMatch, String strTarget, String strReplacement) {
		this.strMatch = strMatch;
		this.strTarget = strTarget;
		this.strReplacement = strReplacement;	
	}
	
	//// Set methods
	
	/**
	 * The filter expression should created in the calling class using ExprFactory. 
	 * The static variables exprS, exprP, exprO from this class should be accessed for 
	 * {?s ?p ?o} to ensure compatibility with variables in the SPARQL update.
	 * */ 
	public void setFilter(Expr filter) {
		exprFilter = filter;
	}
	
	public void setMatch(String match) {
		strMatch = match;
	}
	
	public void setTarget(String target) {
		strTarget = target;
	}
	
	public void setReplacement(String replacement) {
		strReplacement = replacement;
	}
	
	/**
	 * Change the number of triples processed per update
	 * @param updateSize
	 */
	public void setUpdateSize(int updateSize) {
		splitUpdate = true;
		stepSize = updateSize;
	}
	
	/**
	 * Renaming will be performed using a single update
	 */
	public void setSingleUpdate() {
		splitUpdate = false;
	}
	
	
	//// Rename string
	
	/**
	 * Replace a target string in all URIs or literals containing the match string 
	 * in the default graph.
	 * 
	 * @param kbClient
	 * @throws ParseException
	 */
	public void renameString(KnowledgeBaseClient kbClient) throws ParseException {
		renameString(kbClient, null);
	}
	
	/**
	 * Replace a target string in all URIs or literals containing the match string
	 * in named a graph.
	 * 
	 * @param KnowledgeBaseClient
	 * @param graph
	 * @throws SQLException
	 * @throws ParseException
	 */
	public void renameString(KnowledgeBaseClient kbClient, String graph) throws ParseException {		
		
		if(strTarget == null || strReplacement == null) {
			throw new JPSRuntimeException("RenamingTool: target or replacement is null!");
		}else {
		
			if(strMatch == null) {strMatch = strTarget;};		
			
			if(splitUpdate == true) {
				
				//count query
				String query = countString(graph);
				
				JSONArray result = kbClient.executeQuery(query);
			    JSONObject jsonobject = result.getJSONObject(0);
			    int count = jsonobject.getInt(varCount);
				
				int steps = count/stepSize;
				if(count%stepSize > 0) {steps++;}
				
				UpdateRequest sparqlUpdate;
				//loop over splits 
				for(int offset = 0; offset<steps; offset++) {
					 sparqlUpdate = buildSparqlUpdateString(graph);
					 kbClient.executeUpdate(sparqlUpdate);
				}
				
				//check/final update
				result = kbClient.executeQuery(query);
				jsonobject = result.getJSONObject(0);
			    count = jsonobject.getInt(varCount);
			    if (count > 0) {
					sparqlUpdate = buildSparqlUpdateString(graph);
					kbClient.executeUpdate(sparqlUpdate);
			    }
			}else {
			
				UpdateRequest sparqlUpdate = buildSparqlUpdateString(graph);
				kbClient.executeUpdate(sparqlUpdate);
			}
		}
	}
		
	//// Rename URI
	
	/**
	 * Replaces a target URI with the replacement URI in the default graph.
	 * 
	 * @param KnowledgeBaseClient
	 * @throws SQLException
	 * @throws ParseException
	 */
	public void renameURI(KnowledgeBaseClient kbClient) throws ParseException {
		renameURI(kbClient, null);
	}
	
	/**
	 * Replaces a target URI with the replacement URI in a named graph.
	 * 
	 * @param kbClient KnowledgeBaseClient
	 * @param graph
	 * @throws SQLException
	 * @throws ParseException
	 */
	public void renameURI(KnowledgeBaseClient kbClient, String graph) throws ParseException {
		
		if(strTarget == null || strReplacement == null) {
			throw new JPSRuntimeException("RenamingTool: target or replacement is null!");
		}else {
			
			if(splitUpdate == true) {
				
				//count query
				String query = countURI(graph);
				
				JSONArray result = kbClient.executeQuery(query);
			    JSONObject jsonobject = result.getJSONObject(0);
			    int count = jsonobject.getInt(varCount);
				
				int steps = count/stepSize;
				if(count%stepSize > 0) {steps++;}
				
				UpdateRequest sparqlUpdate;
				//loop over splits 
				for(int offset = 0; offset<steps; offset++) {
					 sparqlUpdate = buildSparqlUpdateURI(graph);
					 kbClient.executeUpdate(sparqlUpdate);
				}
				
				//check/final update
				result = kbClient.executeQuery(query);
				jsonobject = result.getJSONObject(0);
			    count = jsonobject.getInt(varCount);
			    if (count > 0) {
					sparqlUpdate = buildSparqlUpdateURI(graph);
					kbClient.executeUpdate(sparqlUpdate);
			    }
			}else {
			
				UpdateRequest sparqlUpdate = buildSparqlUpdateURI(graph);
				kbClient.executeUpdate(sparqlUpdate);
			}
		}
	}
	
	////////////////////////////////
	
	/**
	 * Variables for sparql builder
	 */
	
	String varCount = "count";
	
	// new s, p, o		
	static Var newS = Var.alloc("newS");
	static Var newP = Var.alloc("newP");
	static Var newO = Var.alloc("newO");
	
	// old s, p, o
	static Var varS = Var.alloc("s");
	static Var varP = Var.alloc("p");
	static Var varO = Var.alloc("o");
	/**
	 * (Old) s,p,o variables accessible to calling methods in order to build filter expression
	 */
	public static ExprVar exprS = new ExprVar(varS);
	public static ExprVar exprP = new ExprVar(varP);
	public static ExprVar exprO = new ExprVar(varO);

	// match s, p, o 
	static Var matchS = Var.alloc("matchS");
	static Var matchP = Var.alloc("matchP");
	static Var matchO = Var.alloc("matchO");
	static ExprVar exprMatchS = new ExprVar(matchS);
	static ExprVar exprMatchP = new ExprVar(matchP);
	static ExprVar exprMatchO = new ExprVar(matchO);
	
	//// Build SPARQL update
	
	///// URI
	// targetURI
	Var varTarget = Var.alloc("targetURI");
	ExprVar exprTarget = new ExprVar(varTarget);
	
	//replacementURI
	Var varReplacement = Var.alloc("replacementURI");
	ExprVar exprReplacement = new ExprVar(varReplacement);
	
	/**
	 * 
	 * @return
	 * @throws ParseException
	 */
	private WhereBuilder whereFilterURI() throws ParseException {
		
		// Filter OR expression: FILTER( ?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI )
		Expr eqS = new E_Equals(exprS, exprTarget);
		Expr eqP = new E_Equals(exprP, exprTarget);
		Expr eqO = new E_Equals(exprO, exprTarget);
		Expr orSPO = new E_LogicalOr(eqS, new E_LogicalOr(eqP, eqO));
		
		////Build WHERE statement of the form:
		////"WHERE {" +
		////		  "?s ?p ?o ." +
		////		  "BIND( <target> AS ?targetURI ) ." +
		////		  "BIND( <replacement> AS ?replacementURI ) ." +
		////		  "FILTER( ?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI ) ."}
		
		WhereBuilder where = new WhereBuilder();
		
		// Optional additional filter
		if (exprFilter != null) {
			where.addFilter(exprFilter);
		}
		
		where.addWhere(varS, varP, varO)
			.addBind( "<" + strTarget + ">", varTarget)
			.addBind( "<" + strReplacement + ">", varReplacement)
			.addFilter(orSPO);
						
		return where;
	}
	
	
	/**
	 * Builds sparql update using Jena update builder
	 * target and replacement are URIs.
	 * 
	 * @param graph
	 * @return sparql update request
	 * @throws ParseException
	 */
	private UpdateRequest buildSparqlUpdateURI(String graph) throws ParseException {
						
		// EXPRESSIONS
		// IF statements: IF(?s = ?targetURI, ?replacementURI, ?s)
		Expr ifS = new E_Conditional(new E_Equals(exprS, exprTarget), exprReplacement, exprS);
		Expr ifP = new E_Conditional(new E_Equals(exprP, exprTarget), exprReplacement, exprP);
		Expr ifO = new E_Conditional(new E_Equals(exprO, exprTarget), exprReplacement, exprO);
		
		Expr ifSBlank = new E_Conditional(new E_IsBlank(exprS), exprS, ifS);
		Expr ifPBlank = new E_Conditional(new E_IsBlank(exprP), exprP, ifP);
		Expr ifOBlank = new E_Conditional(new E_IsBlank(exprO), exprO, ifO);
		
		//// Add to where statement:
		////		  "BIND ( IF(isblank(?s), ?s, IF( ?s = ?targetURI, ?replacementURI, ?s)) AS ?newS) ." +
		////		  "BIND ( IF(isblank(?p), ?p, IF( ?p = ?targetURI, ?replacementURI, ?p)) AS ?newP) ." +
		////		  "BIND ( IF(isblank(?o), ?o, IF( ?o = ?targetURI, ?replacementURI, ?o)) AS ?newO) . }";		
		WhereBuilder where = whereFilterURI()
				.addBind(ifSBlank, newS)
				.addBind(ifPBlank, newP)
				.addBind(ifOBlank, newO);
				
		SelectBuilder select = new SelectBuilder();
		
		select.addVar(varS)
		.addVar(varP)
		.addVar(varO)
		.addVar(newS)
		.addVar(newP)
		.addVar(newO);
		
		if(splitUpdate == true) {
			select.setLimit(stepSize);
		}
		
		// Build update
		UpdateBuilder builder = new UpdateBuilder();
		
		// Add where 
		if (graph == null) {
			select.addWhere(where);
			builder.addInsert(newS, newP, newO)
				.addDelete(varS, varP, varO)
				.addSubQuery(select);
		}else {	
			select.addGraph(graph, where);
			// Graph
			String graphURI = "<" + graph + ">";
			builder.addInsert(graphURI, newS, newP, newO)
				.addDelete(graphURI, varS, varP, varO)
				.addSubQuery(select);	
		}
		
		return builder.buildRequest();
	}

	private String countURI(String graph) throws ParseException {
						
		WhereBuilder where = null;

		// Add where 
		if (graph != null) {	
			where = new WhereBuilder();
			// Graph
			String graphURI = "<" + graph + ">";
			where.addGraph(graphURI, whereFilterURI());
		}else {
			where = whereFilterURI();
		}
		
		String query = "SELECT (COUNT(*) AS ?"+varCount+") ";
		query += where.toString();
		return query;
	}
	
	////////////////////////// String
	
	
	// Create expression factory
	ExprFactory exprFactory = new ExprFactory();
			
	// Match string as expression
	Expr exprMatch = exprFactory.asExpr(strMatch);
	
	/**
	 * 
	 * @return
	 * @throws ParseException
	 */
	private WhereBuilder whereFilterString() throws ParseException {
		
		// EXPRESSIONS
		// REGEX expressions: REGEX(str(?s), target)
		Expr regexS = new E_Regex( new E_Str(exprS), exprMatch, null);
		Expr regexP = new E_Regex( new E_Str(exprP), exprMatch, null);
		Expr regexO = new E_Regex( new E_Str(exprO), exprMatch, null);
		
		// OR expression for filter: 
		// (?matchS || ?matchP || ?matchO)
		Expr or = new E_LogicalOr(exprMatchS, new E_LogicalOr(exprMatchP, exprMatchO));
					
		WhereBuilder where = new WhereBuilder()
				.addWhere(varS, varP, varO);
		
		// Optional additional filter
		if (exprFilter != null) {
			where.addFilter(exprFilter);
		}
				
		where.addBind(regexS, matchS)
			.addBind(regexP, matchP)
			.addBind(regexO, matchO)
			.addFilter(or);
		
		return where;
	}
	
	/**
	 * Builds sparql update using Jena update builder.
	 * The update will find URIs or literals containing strMatch. 
	 * strTarget will be replaced by strReplacement in matching uri or literal.
	 * Optional: a named graph or additional filter expression can be applied.
	 * 
	 * @param graph				optional graph.	
	 * @return sparql update request
	 * @throws ParseException
	 */
	private UpdateRequest buildSparqlUpdateString(String graph) throws ParseException {
		
		// Replacement target string as expression
		Expr exprTarget = exprFactory.asExpr(strTarget);
		
		// Replacement string as expression
		Expr exprReplacement = exprFactory.asExpr(strReplacement); 
				
		// EXPRESSIONS
		
		// String replace: 
		// REPLACE(STR(?s), target, replacement)
		Expr replaceS = new E_StrReplace(new E_Str(exprS), exprTarget,  exprReplacement, null);
		Expr replaceP = new E_StrReplace(new E_Str(exprP), exprTarget,  exprReplacement, null);
		Expr replaceO = new E_StrReplace(new E_Str(exprO), exprTarget,  exprReplacement, null);
		
		// If statements: 
		// IF( ?matchS, URI(REPLACE(STR(?s), target, replacement)), ?s)
		Expr ifS = new E_Conditional(exprMatchS, new E_URI(replaceS), exprS);
		Expr ifP = new E_Conditional(exprMatchP, new E_URI(replaceP), exprP);
		Expr ifO = new E_Conditional(exprMatchO, new E_URI(replaceO), exprO);
		
		Expr ifSBlank = new E_Conditional(new E_IsBlank(exprS), exprS, ifS);
		Expr ifPBlank = new E_Conditional(new E_IsBlank(exprP), exprP, ifP);
		Expr ifOBlank = new E_Conditional(new E_IsBlank(exprO), exprO, ifO);
		
		// Build WHERE statement of the form:
		// String strWhere = "WHERE {" +
		//		  "BIND ( IF(isblank(?s), ?s, IF( ?matchS, URI(REPLACE(STR(?s), target, replacement)), ?s)) AS ?newS) ." +
		//		  "BIND ( IF(isblank(?p), ?p, IF( ?matchP, URI(REPLACE(STR(?p), target, replacement)), ?p)) AS ?newP) ." +
		//		  "BIND ( IF(isblank(?o), ?o, IF( ?matchO, URI(REPLACE(STR(?o), target, replacement)), ?o)) AS ?newO) . }"; 
		
		WhereBuilder where = whereFilterString();
				
		where.addBind(ifSBlank, newS)
			.addBind(ifPBlank, newP)
			.addBind(ifOBlank, newO);			
		
		SelectBuilder select = new SelectBuilder();
		
		select.addVar(varS)
		.addVar(varP)
		.addVar(varO)
		.addVar(newS)
		.addVar(newP)
		.addVar(newO);
		
		if(splitUpdate == true) {
			select.setLimit(stepSize);
		}
		
		// Build update
		UpdateBuilder builder = new UpdateBuilder();
		
		// Add where 
		if (graph == null) {
			select.addWhere(where);
			builder.addInsert(newS, newP, newO)
				.addDelete(varS, varP, varO)
				.addSubQuery(select);
		}else {	
			select.addGraph(graph, where);
			// Graph
			String graphURI = "<" + graph + ">";
			builder.addInsert(graphURI, newS, newP, newO)
				.addDelete(graphURI, varS, varP, varO)
				.addSubQuery(select);	
		}
		
		return builder.buildRequest();
	}
	
	/**
	 * 
	 * @param graph
	 * @return
	 * @throws ParseException
	 */
	private String countString(String graph) throws ParseException {
		
		WhereBuilder where = null;

		// Add where 
		if (graph != null) {	
			where = new WhereBuilder();
			// Graph
			String graphURI = "<" + graph + ">";
			where.addGraph(graphURI, whereFilterString());
		}else {
			where = whereFilterString();
		}
		
		String query = "SELECT (COUNT(*) AS ?"+varCount+") ";
		query += where.toString();
		
		return query;
	}
	
}
