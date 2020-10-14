package uk.ac.cam.cares.jps.base.rename;

import java.sql.SQLException;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.E_Conditional;
import org.apache.jena.sparql.expr.E_Equals;
import org.apache.jena.sparql.expr.E_LogicalOr;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;

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
	 * @param type
	 * @param target
	 * @param replacement
	 * @throws SQLException
	 * @throws ParseException
	 */
	public static void renameURI(String endpointUrl, String type, String target, String replacement) throws SQLException, ParseException {
					
		//CSL TODO: add Graph
		//CSL TODO: work with prefix
		
		String strSparqlUpdate = buildSparqlUpdate(target, replacement).toString(); 
		
		// sparql update performs find and replace of subject, predicate or object that contain target string
		String sparqlUpdate = "DELETE {?s ?p ?o}" +
		"INSERT {?newS ?newP ?newO}" +
		"WHERE {" +
		  "?s ?p ?o ." +
		  "BIND( regex(str(?s), str(\"" + target + "\")) AS ?matchS ) ." +
		  "BIND( regex(str(?p), str(\"" + target + "\")) AS ?matchP ) ." +
		  "BIND( regex(str(?o), str(\"" + target + "\")) AS ?matchO ) ." +
		  "FILTER(?matchS || ?matchP || ?matchO) ." +
		  "BIND ( IF( ?matchS, URI(REPLACE(STR(?s), \"" + target + "\", \"" + replacement + "\")), ?s) AS ?newS) ." +
		  "BIND ( IF( ?matchP, URI(REPLACE(STR(?p), \"" + target + "\", \"" + replacement + "\")), ?p) AS ?newP) ." +
		  "BIND ( IF( ?matchO, URI(REPLACE(STR(?o), \"" + target + "\", \"" + replacement + "\")), ?o) AS ?newO) . }";
			
		String updateUrl = null;
		
		// Blazegraph, Fuseki and RDF4J use Jena JDBC
		// Local owl file uses old method
		if (type == RenameType.BLAZEGRAPH.type) {
			updateUrl = endpointUrl + "/update";
			
		}else if(type == RenameType.FUSEKI.type) {
			updateUrl = endpointUrl + "/update";
			
		}else if(type == RenameType.RDF4J.type) {			
			
			updateUrl = endpointUrl + "/statements";
			
		}else if(type == RenameType.OWL_FILE.type) {
			// updates a locally stored owl file
			// this is executed correctly by case 1b in KnowledgeBaseClient.update
			KnowledgeBaseClient.update(null, endpointUrl, strSparqlUpdate);
			return;
			
		}else{
			// unsupported type
			throw new UnsupportedOperationException();
		}
		
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient(null, updateUrl, strSparqlUpdate);
		int result = kbClient.executeUpdate();
		
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
	private static UpdateRequest buildSparqlUpdate(String target, String replacement) throws ParseException {

		// variables
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
		
		
		// Filter expression: FILTER(?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI )
		Expr eqS = new E_Equals(exprOldS, exprTarget);
		Expr eqP = new E_Equals(exprOldP, exprTarget);
		Expr eqO = new E_Equals(exprOldO, exprTarget);
		Expr orSPO = new E_LogicalOr(eqS, new E_LogicalOr(eqP, eqO));
		
		// If statements: IF(?s = ?targetURI, ?replacementURI, ?s)
		Expr ifS = new E_Conditional(new E_Equals(exprOldS, exprTarget), exprReplacement, exprOldS);
		Expr ifP = new E_Conditional(new E_Equals(exprOldP, exprTarget), exprReplacement, exprOldP);
		Expr ifO = new E_Conditional(new E_Equals(exprOldO, exprTarget), exprReplacement, exprOldO);
		
		// build where
		WhereBuilder where = new WhereBuilder()
				.addWhere(varOldS, varOldP, varOldO)
				.addBind( "<" + target + ">", varTarget)
				.addBind( "<" + replacement + ">", varReplacement)
				.addFilter(orSPO)
				.addBind(ifS, newS)
				.addBind(ifP, newP)
				.addBind(ifO, newO);
				
		// build update 
		UpdateBuilder builder = new UpdateBuilder()
				.addInsert(newS, newP, newO)
				.addDelete(varOldS, varOldP, varOldO)
				.addWhere(where);			
				
		return builder.buildRequest();
	}
}
