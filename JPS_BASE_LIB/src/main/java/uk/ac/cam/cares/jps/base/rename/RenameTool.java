package uk.ac.cam.cares.jps.base.rename;

import java.sql.SQLException;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.E_Conditional;
import org.apache.jena.sparql.expr.E_Equals;
import org.apache.jena.sparql.expr.E_LogicalOr;
import org.apache.jena.sparql.expr.E_Regex;
import org.apache.jena.sparql.expr.E_Str;
import org.apache.jena.sparql.expr.E_StrReplace;
import org.apache.jena.sparql.expr.E_URI;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.arq.querybuilder.ExprFactory;

/**
 * This class replaces a given target string in a triple store with a replacement string   
 * using a sparql update.
 * Works with Blazegraph, Fuseki, Rdf4j and local owl files.
 * 
 * @author Casper Lindberg
 *
 */
public class RenameTool {
	
	/**
	 * 
	 * 
	 * @param endpointUrl
	 * @param target
	 * @param replacement
	 * @throws SQLException
	 * @throws ParseException
	 */
	public static void renameURI(String endpointUrl, String target, String replacement) throws SQLException, ParseException {
		
		// Get sparql update as String 
		//String strSparqlUpdate = buildSparqlUpdate(target, replacement, null).toString();
		String strSparqlUpdate = buildSparqlUpdateString(target, replacement, "http://species").toString();
		
		// To Be removed
		// Local owl file uses old method
		if(!endpointUrl.startsWith("http:")) {
			// updates a locally stored owl file
			// this is executed correctly by case 1b in KnowledgeBaseClient.update
			KnowledgeBaseClient.update(null, endpointUrl, strSparqlUpdate);
			return;
		}	
		
		// For Blazegraph, Fuseki, RDF4J etc. use Jena JDBC
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient(null, endpointUrl, strSparqlUpdate);
		kbClient.executeUpdate();
		return;
	}

	/**
	 * Builds sparql update using Jena update builder
	 * target and replacement are URIs 
	 * 
	 * @param target
	 * @param replacement
	 * @return
	 * @throws ParseException
	 */
	private static UpdateRequest buildSparqlUpdateURI(String target, String replacement, String graph) throws ParseException {
				
		// VARIABLES
		// new s, p, o		
		Var newS = Var.alloc("newS");
		Var newP = Var.alloc("newP");
		Var newO = Var.alloc("newO");
		
		// old s, p, o
		Var varOldS = Var.alloc("s");
		Var varOldP = Var.alloc("p");
		Var varOldO = Var.alloc("o");
		ExprVar exprOldS = new ExprVar(varOldS);
		ExprVar exprOldP = new ExprVar(varOldP);
		ExprVar exprOldO = new ExprVar(varOldO);

		// targetURI
		Var varTarget = Var.alloc("targetURI");
		ExprVar exprTarget = new ExprVar(varTarget);
		
		//replacementURI
		Var varReplacement = Var.alloc("replacementURI");
		ExprVar exprReplacement = new ExprVar(varReplacement);
		
		// EXPRESSIONS
		// Filter OR expression: FILTER( ?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI )
		Expr eqS = new E_Equals(exprOldS, exprTarget);
		Expr eqP = new E_Equals(exprOldP, exprTarget);
		Expr eqO = new E_Equals(exprOldO, exprTarget);
		Expr orSPO = new E_LogicalOr(eqS, new E_LogicalOr(eqP, eqO));
		
		// IF statements: IF(?s = ?targetURI, ?replacementURI, ?s)
		Expr ifS = new E_Conditional(new E_Equals(exprOldS, exprTarget), exprReplacement, exprOldS);
		Expr ifP = new E_Conditional(new E_Equals(exprOldP, exprTarget), exprReplacement, exprOldP);
		Expr ifO = new E_Conditional(new E_Equals(exprOldO, exprTarget), exprReplacement, exprOldO);
		
		// Build WHERE statement of the form:
		// String strWhere = "WHERE {" +
		//		  "?s ?p ?o ." +
		//		  "BIND( <target> AS ?targetURI ) ." +
		//		  "BIND( <replacement> AS ?replacementURI ) ." +
		//		  "FILTER( ?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI ) ." +
		//		  "BIND ( IF( ?s = ?targetURI, ?replacementURI, ?s) AS ?newS) ." +
		//		  "BIND ( IF( ?p = ?targetURI, ?replacementURI, ?p) AS ?newP) ." +
		//		  "BIND ( IF( ?o = ?targetURI, ?replacementURI, ?o) AS ?newO) . }"; 
		WhereBuilder where = new WhereBuilder()
				.addWhere(varOldS, varOldP, varOldO)
				.addBind( "<" + target + ">", varTarget)
				.addBind( "<" + replacement + ">", varReplacement)
				.addFilter(orSPO)
				.addBind(ifS, newS)
				.addBind(ifP, newP)
				.addBind(ifO, newO);
				
		// Build update
		UpdateBuilder builder = new UpdateBuilder();
				
		// Add where 
		if (graph == null) {
			builder.addInsert(newS, newP, newO)
				.addDelete(varOldS, varOldP, varOldO)
				.addWhere(where);
		}else {	
			// Graph
			String graphURI = "<" + graph + ">";
			builder.addInsert(graphURI, newS, newP, newO)
				.addDelete(graphURI, varOldS, varOldP, varOldO)
				.addGraph(graphURI, where);	
		}
		
		return builder.buildRequest();
	}
	
	/**
	 * Builds sparql update using Jena update builder
	 * target and replacement are strings
	 * 
	 * @param target
	 * @param replacement
	 * @return
	 * @throws ParseException
	 */
	private static UpdateRequest buildSparqlUpdateString(String target, String replacement, String graph) {

		// Create expression factory
		ExprFactory exprFactory = new ExprFactory();
		
		// VARIABLES
		// new s, p, o		
		Var newS = Var.alloc("newS");
		Var newP = Var.alloc("newP");
		Var newO = Var.alloc("newO");
		
		// old s, p, o
		Var varOldS = Var.alloc("s");
		Var varOldP = Var.alloc("p");
		Var varOldO = Var.alloc("o");
		ExprVar exprOldS = new ExprVar(varOldS);
		ExprVar exprOldP = new ExprVar(varOldP);
		ExprVar exprOldO = new ExprVar(varOldO);

		// match s, p, o 
		Var matchS = Var.alloc("matchS");
		Var matchP = Var.alloc("matchP");
		Var matchO = Var.alloc("matchO");
		ExprVar exprMatchS = new ExprVar(matchS);
		ExprVar exprMatchP = new ExprVar(matchP);
		ExprVar exprMatchO = new ExprVar(matchO);
				
		// Target string as expression
		Expr exprTarget = exprFactory.asExpr(target);
		
		// Replacement string as expression
		Expr exprReplacement = exprFactory.asExpr(replacement); 
				
		// EXPRESSIONS
		// REGEX expressions: REGEX(str(?s), target)
		Expr regexS = new E_Regex( new E_Str(exprOldS), exprTarget, null);
		Expr regexP = new E_Regex( new E_Str(exprOldP), exprTarget, null);
		Expr regexO = new E_Regex( new E_Str(exprOldO), exprTarget, null);
		
		// OR expression for filter: 
		// (?matchS || ?matchP || ?matchO)
		Expr or = new E_LogicalOr(exprMatchS, new E_LogicalOr(exprMatchP, exprMatchO));
			
		// String replace: 
		// REPLACE(STR(?s), target, replacement)
		Expr replaceS = new E_StrReplace(new E_Str(exprOldS), exprTarget,  exprReplacement, null);
		Expr replaceP = new E_StrReplace(new E_Str(exprOldP), exprTarget,  exprReplacement, null);
		Expr replaceO = new E_StrReplace(new E_Str(exprOldO), exprTarget,  exprReplacement, null);
		
		// If statements: 
		// IF( ?matchS, URI(REPLACE(STR(?s), target, replacement)), ?s)
		Expr ifS = new E_Conditional(exprMatchS, new E_URI(replaceS), exprOldS);
		Expr ifP = new E_Conditional(exprMatchP, new E_URI(replaceP), exprOldP);
		Expr ifO = new E_Conditional(exprMatchO, new E_URI(replaceO), exprOldO);
		
		// Build WHERE statement of the form:
		// String strWhere = "WHERE {" +
		//		  "?s ?p ?o ." +
		//		  "BIND( regex(str(?p), target) AS ?matchP ) ." +
		//		  "BIND( regex(str(?p), target) AS ?matchP ) ." +
		//		  "BIND( regex(str(?o), target) AS ?matchO ) ." +
		//		  "FILTER(?matchS || ?matchP || ?matchO) ." +
		//		  "BIND ( IF( ?matchS, URI(REPLACE(STR(?s), target, replacement)), ?s) AS ?newS) ." +
		//		  "BIND ( IF( ?matchP, URI(REPLACE(STR(?p), target, replacement)), ?p) AS ?newP) ." +
		//		  "BIND ( IF( ?matchO, URI(REPLACE(STR(?o), target, replacement)), ?o) AS ?newO) . }"; 
		WhereBuilder where = new WhereBuilder()
				.addWhere(varOldS, varOldP, varOldO)
				.addBind(regexS, matchS)
				.addBind(regexP, matchP)
				.addBind(regexO, matchO)
				.addFilter(or)
				.addBind(ifS, newS)
				.addBind(ifP, newP)
				.addBind(ifO, newO);			
				
		// Build update
		UpdateBuilder builder = new UpdateBuilder();
				
		// Add where 
		if (graph == null) {
			builder.addInsert(newS, newP, newO)
				.addDelete(varOldS, varOldP, varOldO)
				.addWhere(where);
		}else {	
			// Graph
			String graphURI = "<" + graph + ">";
			builder.addInsert(graphURI, newS, newP, newO)
				.addDelete(graphURI, varOldS, varOldP, varOldO)
				.addGraph(graphURI, where);	
		}
		
		return builder.buildRequest();
	}
}