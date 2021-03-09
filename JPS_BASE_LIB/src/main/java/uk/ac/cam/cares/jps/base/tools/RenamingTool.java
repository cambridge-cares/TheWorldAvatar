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
	 *strMatch			match string in s/p/o to be renamed. Usually the same as strTarget.
	 *strTarget 		target string to be replaced in matching s/p/o.
	 *strReplacement 	replacement string.
	 *exprFilter 		additional filter expression can be applied to the update
	 *splitUpdate 		(boolean) split the renaming sparql update into multiple smaller updates
	 *stepSize			number of triples processed per update
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
	 * in a named graph.
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
		
			exprMatch = exprFactory.asExpr(strMatch);
			exprTarget = exprFactory.asExpr(strTarget);
			
			WhereBuilder whereFilter = whereMatchString();
			WhereBuilder whereUpdate = whereUpdateString();
			
			performRename(kbClient, graph, whereFilter, whereUpdate);
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
			
			//// For URI renaming
			// targetURI
			varTargetURI = Var.alloc("targetURI");
			exprTargetURI = new ExprVar(varTargetURI);
			
			//replacementURI
			varReplacementURI = Var.alloc("replacementURI");
			exprReplacementURI = new ExprVar(varReplacementURI);
			
			WhereBuilder whereUpdate = whereUpdateURI();
			WhereBuilder whereFilter = whereMatchURI();
			
			performRename(kbClient, graph, whereFilter, whereUpdate);
		}
	}
	
	/**
	 * Perform rename 
	 * @param kbClient
	 * @param graph
	 * @param whereFilter
	 * @param whereUpdate
	 * @throws ParseException
	 */
	private void performRename(KnowledgeBaseClient kbClient, String graph, WhereBuilder whereMatch, WhereBuilder whereUpdate) throws ParseException {
		
		if(splitUpdate == true) {
			
			//count triples matching renaming criteria
			String query = countQuery(graph, whereMatch);
			
			JSONArray result = kbClient.executeQuery(query);
		    JSONObject jsonobject = result.getJSONObject(0);
		    int count = jsonobject.getInt(varCount);
			
			int steps = count/stepSize;
			if(count%stepSize > 0) {steps++;}
			
			//build sparql update 
			UpdateRequest sparqlUpdate = buildSparqlUpdate(graph, whereUpdate, stepSize);
			
			//loop over splits 
			for(int offset = 0; offset<steps; offset++) {
				 kbClient.executeUpdate(sparqlUpdate);
			}
			
			//check all triples have been renamed, if not update all
			result = kbClient.executeQuery(query);
			jsonobject = result.getJSONObject(0);
		    count = jsonobject.getInt(varCount);
		    if (count > 0) {
				sparqlUpdate = buildSparqlUpdate(graph, whereUpdate, 0);
				kbClient.executeUpdate(sparqlUpdate);
		    }
		}else {
		
			UpdateRequest sparqlUpdate = buildSparqlUpdate(graph, whereUpdate, 0);
			kbClient.executeUpdate(sparqlUpdate);
		}
		
	}
	
	//// Methods to construct sparql queries
	
	/**
	 * Variables for sparql builder
	 */
	
	// Create expression factory
	ExprFactory exprFactory = new ExprFactory();
	
	// count variable
	String varCount = "count";
		
	//// For URI renaming
	// targetURI
	Var varTargetURI;
	ExprVar exprTargetURI;
	
	//replacementURI
	Var varReplacementURI;
	ExprVar exprReplacementURI;
	
	//// For string renaming
	// Match string as expression
	Expr exprMatch;
	// Target string as expression
	Expr exprTarget;
	
	//// SPARQL expressions
	
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
	
	/**
	 * Return sparql count query as string to count number of triples matching renaming criteria 
	 * @param graph
	 * @param whereFilter
	 * @return count query
	 * @throws ParseException
	 */
	private String countQuery(String graph, WhereBuilder whereFilter) throws ParseException {
		
		WhereBuilder where = null;

		// Add where 
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

	/**
	 * Build sparql update
	 * @param named graph (optional)
	 * @param where
	 * @param limit
	 * @return
	 */
	private UpdateRequest buildSparqlUpdate(String graph, WhereBuilder where, int limit) {
		
		////	DELETE {?s ?p ?o}
		////	INSERT {?newS ?newP ?newO}
		////	WHERE { 
		////		SELECT ?s ?p ?o ?newS ?newP ?newO
		////		WHERE{
		////		} LIMIT 1000000 }
		
		// subquery selects new and old triples
		SelectBuilder select = new SelectBuilder();
		
		select.addVar(varS)
		.addVar(varP)
		.addVar(varO)
		.addVar(newS)
		.addVar(newP)
		.addVar(newO);
		
		if(limit > 0) {
			select.setLimit(limit);
		}
		
		// Build update
		UpdateBuilder builder = new UpdateBuilder();
		
		// Add select subquery and optional graph
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
	
	//// SPARQL wherebuilder for URIs
	
	/**
	 * Create sparql where to filter triples matching renaming criteria 
	 * Matches complete URIs. target and replacement are URIs.
	 * @return
	 * @throws ParseException
	 */
	private WhereBuilder whereMatchURI() throws ParseException {
		
		// Filter OR expression: FILTER( ?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI )
		Expr eqS = new E_Equals(exprS, exprTargetURI);
		Expr eqP = new E_Equals(exprP, exprTargetURI);
		Expr eqO = new E_Equals(exprO, exprTargetURI);
		Expr orSPO = new E_LogicalOr(eqS, new E_LogicalOr(eqP, eqO));
		
		////Build WHERE statement of the form:
		////"WHERE {" +
		////		  "?s ?p ?o ." +
		////		  "FILTER(...optional filter...). +
		////		  "BIND( <target> AS ?targetURI ) ." +
		////		  "BIND( <replacement> AS ?replacementURI ) ." +
		////		  "FILTER( ?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI ) ."}
		
		WhereBuilder where = new WhereBuilder();
		
		// Optional additional filter
		if (exprFilter != null) {
			where.addFilter(exprFilter);
		}
		
		where.addWhere(varS, varP, varO)
			.addBind( "<" + strTarget + ">", varTargetURI)
			.addBind( "<" + strReplacement + ">", varReplacementURI)
			.addFilter(orSPO);
						
		return where;
	}
	
	
	/**
	 * Create sparql where to filter matching triples and bind replacement URI 
	 * 
	 * @param graph
	 * @return sparql update request
	 * @throws ParseException
	 */
	private WhereBuilder whereUpdateURI() throws ParseException {
		
	////		WHERE {?s ?p ?o .
	////			FILTER(...optional filter...).
	////			BIND( <target> AS ?targetURI ) .
	////			BIND( <replacement> AS ?replacementURI ) .
	////			FILTER( ?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI ) . 
	////			BIND ( IF(isblank(?s), ?s, IF( ?s = ?targetURI, ?replacementURI, ?s)) AS ?newS) .
	////	  		BIND ( IF(isblank(?p), ?p, IF( ?p = ?targetURI, ?replacementURI, ?p)) AS ?newP) .
	////	    	BIND ( IF(isblank(?o), ?o, IF( ?o = ?targetURI, ?replacementURI, ?o)) AS ?newO) . 
	////		}
		
		// EXPRESSIONS
		// IF statements: IF(?s = ?targetURI, ?replacementURI, ?s)
		Expr ifS = new E_Conditional(new E_Equals(exprS, exprTargetURI), exprReplacementURI, exprS);
		Expr ifP = new E_Conditional(new E_Equals(exprP, exprTargetURI), exprReplacementURI, exprP);
		Expr ifO = new E_Conditional(new E_Equals(exprO, exprTargetURI), exprReplacementURI, exprO);
		
		Expr ifSBlank = new E_Conditional(new E_IsBlank(exprS), exprS, ifS);
		Expr ifPBlank = new E_Conditional(new E_IsBlank(exprP), exprP, ifP);
		Expr ifOBlank = new E_Conditional(new E_IsBlank(exprO), exprO, ifO);
		
		//// new triple
		WhereBuilder where = whereMatchURI()
				.addBind(ifSBlank, newS)
				.addBind(ifPBlank, newP)
				.addBind(ifOBlank, newO);
		
		return where;
	}
	
	///// SPARQL wherebuilder for strings
	
	/**
	 * Create sparql where to filter triples matching renaming criteria
	 * Matches uri or literals containing strMatch. 
	 * @return
	 * @throws ParseException
	 */
	private WhereBuilder whereMatchString() throws ParseException {
		
		////    "WHERE\n"+
		////	"  { ?s  ?p  ?o\n"+
		////	"    BIND((regex(str(?s), exprMatch) && regex(str(?s), exprTarget)) AS ?matchS)\n"+
		////	"    BIND((regex(str(?p), exprMatch) && regex(str(?p), exprTarget)) AS ?matchP)\n"+
		////	"    BIND((regex(str(?o), exprMatch) && regex(str(?o), exprTarget)) AS ?matchO)\n"+
		////	"    FILTER ( ?matchS || ( ?matchP || ?matchO ) )\n"+ 
		////	"  }\n";
		
		// EXPRESSIONS
		// REGEX expressions: REGEX(str(?s), match)
		Expr regexSmatch = new E_Regex( new E_Str(exprS), exprMatch, null);
		Expr regexPmatch = new E_Regex( new E_Str(exprP), exprMatch, null);
		Expr regexOmatch = new E_Regex( new E_Str(exprO), exprMatch, null);
		
		Expr regexStarget = new E_Regex( new E_Str(exprS), exprTarget, null);
		Expr regexPtarget = new E_Regex( new E_Str(exprP), exprTarget, null);
		Expr regexOtarget = new E_Regex( new E_Str(exprO), exprTarget, null);
		
		Expr regexS;
		Expr regexP;
		Expr regexO;
		if(exprMatch != exprTarget) {
			regexS = exprFactory.and(regexSmatch, regexStarget);
			regexP = exprFactory.and(regexPmatch, regexPtarget);
			regexO = exprFactory.and(regexOmatch, regexOtarget);
		}else {
			regexS = regexSmatch;
			regexP = regexPmatch;
			regexO = regexOmatch;
		}
		
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
	 * Create sparql where to filter matching triples and construct replacements 
	 * Replaces strTarget with strReplacement in matching triples
	 * @return
	 * @throws ParseException
	 */
	private WhereBuilder whereUpdateString() throws ParseException {
			
	////		WHERE{
	////			?s ?p ?o
	////			FILTER ( ... optional filter ... )
	////			BIND( REGEX(str(?s), exprMatch) AS ?matchS ) .
	////			BIND( REGEX(str(?p), exprMatch) AS ?matchP ) .
	////			BIND( REGEX(str(?o), exprMatch) AS ?matchO ) .
	////    		FILTER ( ?matchS || ( ?matchP || ?matchO ) )
	////    		BIND(if(isBlank(?s), ?s, if(?matchS, uri(replace(str(?s), exprTarget, exprReplacement)), ?s)) AS ?newS)
	////			BIND(if(isBlank(?p), ?p, if(?matchP, uri(replace(str(?p), exprTarget, exprReplacement)), ?p)) AS ?newP)
	////			BIND(if(isBlank(?o), ?o, if(?matchO, uri(replace(str(?o), exprTarget, exprReplacement)), ?o)) AS ?newO)
	////		}
				
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
		
		// where
		WhereBuilder where = whereMatchString()
				.addBind(ifSBlank, newS)
				.addBind(ifPBlank, newP)
				.addBind(ifOBlank, newO);			
		
		return where;
	}
}
