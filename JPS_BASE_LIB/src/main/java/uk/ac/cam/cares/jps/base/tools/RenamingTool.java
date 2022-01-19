package uk.ac.cam.cares.jps.base.tools;

import java.sql.SQLException;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * Renaming Tool
 * 
 * This tool renames IRIs and literals from target to replacement in the supplied StoreClient.
 * Where the target and replacement are IRIs use the method renameIRI(...). 
 * Where the target and replacement are strings/substrings use the method renameString(...).
 * In the case of renameString a match string can also be supplied. In this case, the tool will   
 * select triples containing both match and target, and replace the target with the replacement string.
 * Renaming is by default performed over multiple steps, updating a sub-set of triples in the 
 * StoreClient every step. This feature can be turned off using setSingleUpdate(...) and 
 * the number of triples updated every step can be set through setUpdateSize(...).
 * An optional filter expression can be applied through setFilter(...).
 * NOTE: If the KB is a remote triple store (rather than quad store) then "setTripleStore()" 
 * must be set for the tool to function.
 * 
 * @author Casper Lindberg
 *
 */
public class RenamingTool {
	
	String strMatch; 			//string to match in triple (s/p/o). Not used for renameURI
	String strTarget;			//target string (or IRI as string) to be replaced.
	String strReplacement;		//replacement string (or IRI as string).
	Expr exprFilter = null; 	//optional additional filer 
	boolean splitUpdate = true;	//flag: splits the renaming operation in multiple smaller updates 
	int stepSize = 1000000;		//number of triples processed per update
	boolean quads = true;		//is the KBClient a quads store?
	
	//// Constructors
	
	/**
	 * Class constructor. Initialises the target and replacement strings or IRIs
	 * @param strTarget 		target string or IRI (as string) to be replaced
	 * @param strReplacement 	replacement string or IRI (as string)
	 * */
	public RenamingTool(String strTarget, String strReplacement) {
		this.strMatch = strTarget; //set match = target
		this.strTarget = strTarget;
		this.strReplacement = strReplacement;	
	}
	
	/**
	 * Class constructor. Initialises the match, target and replacement strings. 
	 * @param strMatch			string to match in triple (s/p/o). 
	 * @param strTarget 		target string or IRI (as string)
	 * @param strReplacement 	replacement string or IRI (as string)
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
	 * @param filter
	 */ 
	public void setFilter(Expr filter) {
		exprFilter = filter;
	}
	
	/**
	 * Set the match string 
	 * @param match
	 */
	public void setMatch(String match) {
		strMatch = match;
	}
	
	/**
	 * Set the target string or IRI (as string)
	 * @param target
	 */
	public void setTarget(String target) {
		strTarget = target;
	}
	
	/**
	 * Set the replacement string or IRI (as string)
	 * @param replacement
	 */
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
	
	//// Rename string
	
	/**
	 * Replace a target string in all IRIs or literals also containing the match string 
	 * (if different/specified) in the default graph.
	 * 
	 * @param kbClient
	 */
	public void renameString(StoreClientInterface kbClient) {
		renameString(kbClient, null);
	}
	
	/**
	 * Replace a target string in all URIs or literals also containing the match string 
	 * (if different/specified) in a named graph.
	 * 
	 * @param StoreClient
	 * @param graph
	 */
	public void renameString(StoreClientInterface kbClient, String graph) {		
		
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
	 * Replaces a target IRI with the replacement IRI in the default graph.
	 * 
	 * @param StoreClient
	 */
	public void renameIRI(StoreClientInterface kbClient) {
		renameIRI(kbClient, null);
	}
	
	/**
	 * Replaces a target IRI with the replacement IRI in a named graph.
	 * 
	 * @param kbClient StoreClient
	 * @param graph
	 */
	public void renameIRI(StoreClientInterface kbClient, String graph) {
		
		if(strTarget == null || strReplacement == null) {
			throw new JPSRuntimeException("RenamingTool: target or replacement is null!");
		}else {
			
			WhereBuilder whereUpdate = whereUpdateIRI();
			WhereBuilder whereFilter = whereMatchIRI();
			
			performRename(kbClient, graph, whereFilter, whereUpdate);
		}
	}
	
	/**
	 * Perform rename 
	 * @param kbClient
	 * @param graph
	 * @param whereFilter
	 * @param whereUpdate
	 */
	private void performRename(StoreClientInterface kbClient, String graph, WhereBuilder whereMatch, WhereBuilder whereUpdate) {
		
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
			for(int i = 0; i<steps; i++) {
				try {
					kbClient.executeUpdate(sparqlUpdate);
				}catch(Exception e) {
					if (e.getCause() instanceof SQLException) {
						throw new JPSRuntimeException("RenamingTool: tagging update failed! SourceKB might not be quads. Try setTripleStore().", e);
					}else {
						throw e;
					}
				}
			}
			
			//check all triples have been renamed, if not then update all
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
	
	////////////////////////////////////////
	//// Methods to construct sparql queries
	
	/**
	 * Variables used by sparql builder
	 */
	
	// Expression factory
	ExprFactory exprFactory = new ExprFactory();
	
	// Count sparql variable
	static String varCount = "count";
		
	// For IRI renaming
	static Var varTargetURI = Var.alloc("targetURI");
	static 	ExprVar exprTargetURI = new ExprVar(varTargetURI);
	
	static Var varReplacementURI = Var.alloc("replacementURI");
	static ExprVar exprReplacementURI = new ExprVar(varReplacementURI);
	
	// For string renaming
	Expr exprMatch;		// Match string as expression
	Expr exprTarget;	// Target string as expression
	
	// new s, p, o		
	static Var newS = Var.alloc("newS");
	static Var newP = Var.alloc("newP");
	static Var newO = Var.alloc("newO");
	
	// old s, p, o
	static Var varS = Var.alloc("s");
	static Var varP = Var.alloc("p");
	static Var varO = Var.alloc("o");
	static Var varG = Var.alloc("g");
	
	//(Old) s,p,o variables accessible to calling methods in order to build optional filter expression
	public static ExprVar exprS = new ExprVar(varS);
	public static ExprVar exprP = new ExprVar(varP);
	public static ExprVar exprO = new ExprVar(varO);
	public static ExprVar exprG = new ExprVar(varG);
	
	// match s, p, o 
	static Var matchS = Var.alloc("matchS");
	static Var matchP = Var.alloc("matchP");
	static Var matchO = Var.alloc("matchO");
	static ExprVar exprMatchS = new ExprVar(matchS);
	static ExprVar exprMatchP = new ExprVar(matchP);
	static ExprVar exprMatchO = new ExprVar(matchO);
	
	/**
	 * Build sparql count query as string 
	 * Counts the number of triples matching the renaming criteria 
	 * @param graph
	 * @param whereFilter
	 * @return count query
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

	/**
	 * Build the sparql update
	 * @param named graph (optional)
	 * @param sparql where builder
	 * @param limit (number of triples to update)
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
		
		UpdateBuilder builder = new UpdateBuilder();
		
		// Add select subquery and optional graph
		if(quads == true) {
			if (graph == null) {
				select.addVar(varG);
				select.addGraph(varG,where);
				builder.addInsert(varG, newS, newP, newO)
					.addDelete(varG, varS, varP, varO)
					.addSubQuery(select);
			}else {	
				// Graph
				String graphURI = "<" + graph + ">";
				select.addGraph(graphURI, where);
				builder.addInsert(graphURI, newS, newP, newO)
					.addDelete(graphURI, varS, varP, varO)
					.addSubQuery(select);	
			}
		}else {
			select.addWhere(where);
			builder.addInsert(newS, newP, newO)
			.addDelete(varS, varP, varO)
			.addSubQuery(select);
		}
		
		return builder.buildRequest();
	}
	
	//// SPARQL wherebuilder for IRI renaming
	
	/**
	 * Create sparql where to filter triples matching renaming criteria 
	 * Matches complete IRIs. target and replacement are IRIs.
	 * @return
	 * @throws ParseException
	 */
	private WhereBuilder whereMatchIRI() {
		
		// Filter OR expression: FILTER( ?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI )
		Expr eqS = exprFactory.eq(exprS, exprTargetURI);
		Expr eqP = exprFactory.eq(exprP, exprTargetURI);
		Expr eqO = exprFactory.eq(exprO, exprTargetURI);
		Expr orSPO = exprFactory.or(eqS, exprFactory.or(eqP, eqO));
		
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
		
		try {
			where.addWhere(varS, varP, varO)
				.addBind( "<" + strTarget + ">", varTargetURI)
				.addBind( "<" + strReplacement + ">", varReplacementURI)
				.addFilter(orSPO);
		} catch (ParseException e) {
			throw new JPSRuntimeException(e);
		}
						
		return where;
	}
	
	
	/**
	 * Create sparql where to filter matching triples and bind replacement IRIs 
	 * 
	 * @param graph
	 * @return sparql update request
	 * @throws ParseException
	 */
	private WhereBuilder whereUpdateIRI(){
		
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
		Expr ifS = exprFactory.cond(exprFactory.eq(exprS, exprTargetURI), exprReplacementURI, exprS);
		Expr ifP = exprFactory.cond(exprFactory.eq(exprP, exprTargetURI), exprReplacementURI, exprP);
		Expr ifO = exprFactory.cond(exprFactory.eq(exprO, exprTargetURI), exprReplacementURI, exprO);
		
		Expr ifSBlank = exprFactory.cond(exprFactory.isBlank(exprS), exprS, ifS);
		Expr ifPBlank = exprFactory.cond(exprFactory.isBlank(exprP), exprP, ifP);
		Expr ifOBlank = exprFactory.cond(exprFactory.isBlank(exprO), exprO, ifO);
		
		//// new triple
		WhereBuilder where = whereMatchIRI()
				.addBind(ifSBlank, newS)
				.addBind(ifPBlank, newP)
				.addBind(ifOBlank, newO);
		
		return where;
	}
	
	///// SPARQL wherebuilder for renaming strings
	
	/**
	 * Create sparql where to filter triples matching renaming criteria
	 * Matches IRIs or literals containing strMatch and strTarget. 
	 * @return
	 * @throws ParseException
	 */
	private WhereBuilder whereMatchString() {
		
		////    "WHERE\n"+
		////	"  { ?s  ?p  ?o\n"+
		////	"    BIND((contains(str(?s), exprMatch) && contains(str(?s), exprTarget)) AS ?matchS)\n"+
		////	"    BIND((contains(str(?p), exprMatch) && contains(str(?p), exprTarget)) AS ?matchP)\n"+
		////	"    BIND((contains(str(?o), exprMatch) && contains(str(?o), exprTarget)) AS ?matchO)\n"+
		////	"    FILTER ( ?matchS || ( ?matchP || ?matchO ) )\n"+ 
		////	"  }\n";
		
		// EXPRESSIONS
		// Contains expressions: contains(str(?s), match)
		Expr SContainsMatch = exprFactory.contains(exprFactory.str(exprS), exprMatch);
		Expr PContainsMatch = exprFactory.contains(exprFactory.str(exprP), exprMatch);
		Expr OContainsMatch = exprFactory.contains(exprFactory.str(exprO), exprMatch);
		
		// Contains expressions: contains(str(?s), target)
		Expr SContainsTarget = exprFactory.contains(exprFactory.str(exprS), exprTarget);
		Expr PContainsTarget = exprFactory.contains(exprFactory.str(exprP), exprTarget);
		Expr OContainsTarget = exprFactory.contains(exprFactory.str(exprO), exprTarget);
		
		Expr SContains = SContainsMatch;
		Expr PContains = PContainsMatch;
		Expr OContains = OContainsMatch;
		if(exprMatch != exprTarget) {
			SContains = exprFactory.and(SContainsMatch, SContainsTarget);
			PContains = exprFactory.and(PContainsMatch, PContainsTarget);
			OContains = exprFactory.and(OContainsMatch, OContainsTarget);
		}
		
		// OR expression for filter: 
		// (?matchS || ?matchP || ?matchO)
		Expr or = exprFactory.or(exprMatchS, exprFactory.or(exprMatchP, exprMatchO));
					
		WhereBuilder where = new WhereBuilder()
				.addWhere(varS, varP, varO);
		
		// Optional additional filter
		if (exprFilter != null) {
			where.addFilter(exprFilter);
		}
				
		where.addBind(SContains, matchS)
			.addBind(PContains, matchP)
			.addBind(OContains, matchO)
			.addFilter(or);
		
		return where;
	}
	
	/**
	 * Create sparql where to filter matching triples and construct replacements 
	 * Replaces strTarget with strReplacement in matching triples
	 * @return
	 * @throws ParseException
	 */
	private WhereBuilder whereUpdateString() {
			
	////		WHERE{
	////			?s ?p ?o
	////			FILTER ( ... optional filter ... )
	////			BIND( contains(str(?s), exprMatch) AS ?matchS ) .
	////			BIND( contains(str(?p), exprMatch) AS ?matchP ) .
	////			BIND( contains(str(?o), exprMatch) AS ?matchO ) .
	////    		FILTER ( ?matchS || ( ?matchP || ?matchO ) )
	////    		BIND(if(isBlank(?s), ?s, if(?matchS, iri(replace(str(?s), exprTarget, exprReplacement)), ?s)) AS ?newS)
	////			BIND(if(isBlank(?p), ?p, if(?matchP, iri(replace(str(?p), exprTarget, exprReplacement)), ?p)) AS ?newP)
	////			BIND(if(isBlank(?o), ?o, if(?matchO, iri(replace(str(?o), exprTarget, exprReplacement)), ?o)) AS ?newO)
	////		}
				
		// Replacement string as expression
		Expr exprReplacement = exprFactory.asExpr(strReplacement); 
				
		// EXPRESSIONS
		
		// String replace: 
		// REPLACE(STR(?s), target, replacement)
		Expr replaceS = exprFactory.replace(exprFactory.str(exprS), exprTarget,  exprReplacement);
		Expr replaceP = exprFactory.replace(exprFactory.str(exprP), exprTarget,  exprReplacement);
		Expr replaceO = exprFactory.replace(exprFactory.str(exprO), exprTarget,  exprReplacement);
		
		// If statements: 
		// IF( ?matchS, URI(REPLACE(STR(?s), target, replacement)), ?s)
		Expr ifS = exprFactory.cond(exprMatchS, exprFactory.iri(replaceS), exprS);
		Expr ifP = exprFactory.cond(exprMatchP, exprFactory.iri(replaceP), exprP);
		Expr ifO = exprFactory.cond(exprMatchO, exprFactory.iri(replaceO), exprO);
		
		Expr ifSBlank = exprFactory.cond(exprFactory.isBlank(exprS), exprS, ifS);
		Expr ifPBlank = exprFactory.cond(exprFactory.isBlank(exprP), exprP, ifP);
		Expr ifOBlank = exprFactory.cond(exprFactory.isBlank(exprO), exprO, ifO);
		
		// where
		WhereBuilder where = whereMatchString()
				.addBind(ifSBlank, newS)
				.addBind(ifPBlank, newP)
				.addBind(ifOBlank, newO);			
		
		return where;
	}
}
