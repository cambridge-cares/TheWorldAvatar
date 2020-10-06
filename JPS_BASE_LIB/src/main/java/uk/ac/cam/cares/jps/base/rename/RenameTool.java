package uk.ac.cam.cares.jps.base.rename;

import java.sql.SQLException;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import org.apache.jena.sparql.lang.sparql_11.ParseException;

/**
 * This class replaces a given target string in a triple store with a replacement string   
 * using a sparql update.
 * Works with Blazegraph, Fuseki, Rdf4j and local owl files.
 * 
 * @author Casper Lindberg
 *
 */
public class RenameTool {
	
	// input: endpoint, store type, search target, replacement
	public static void renameURI(String endpointUrl, String type, String target, String replacement) throws SQLException, ParseException {
					
		//CSL TODO: add Graph
		
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
				
		// sparql update with URI as target
	//	"WHERE {" +
	//		  "BIND ( <" + target + " AS ?targetURI)" +
	//		  "?s ?p ?o." +
	//		  "FILTER(?s = ?targetURI || ?p = ?targetURI || ?o = ?targetURI )" +
	//		  "BIND (IF(?s = ?targetURI, URI(REPLACE(STR(?targetURI), \"" + target + "\", \"" + replacement + "\")), ?s) AS ?newS)" +
	//		  "BIND (IF(?p = ?targetURI, URI(REPLACE(STR(?targetURI), \"" + target + "\", \"" + replacement + "\")), ?p) AS ?newP)" +
	//		  "BIND (IF(?o = ?targetURI, URI(REPLACE(STR(?targetURI), \"" + target + "\", \"" + replacement + "\")), ?o) AS ?newO) . }";
		
		String updateUrl = null;
		KnowledgeBaseClient kbClient = null;
		
		// Blazegrpah, Fuseki and RDF4J use Jena JDBC
		// Local owl file uses old method
		if (type == RenameType.BLAZEGRAPH.type) {
			updateUrl = endpointUrl + "/update";
			
			kbClient = new KnowledgeBaseClient(null, updateUrl, sparqlUpdate);
			kbClient.executeUpdate();
			
		}else if(type == RenameType.FUSEKI.type) {
			updateUrl = endpointUrl + "/update";
			
			kbClient = new KnowledgeBaseClient(null, updateUrl, sparqlUpdate);
			kbClient.executeUpdate();
			
		}else if(type == RenameType.RDF4J.type) {			
			
			updateUrl = endpointUrl + "/statements";
			
			kbClient = new KnowledgeBaseClient(null, updateUrl, sparqlUpdate);
			kbClient.executeUpdate();
			
		}else if(type == RenameType.OWL_FILE.type) {
			// updates a locally stored owl file
			// this is executed correctly by case 1b in KnowledgeBaseClient.update
			KnowledgeBaseClient.update(null, endpointUrl, sparqlUpdate);
			return;
			
		}else{
			
			// unsupported type
			throw new UnsupportedOperationException();
		}
	}
}
