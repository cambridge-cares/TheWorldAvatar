package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;

/**
 * Abstract Knowledge Base Client class that declares methods for performing SPARQL 
 * queries and updates on triple stores.
 *  
 * @author Casper Lindberg
 */
public abstract class AbstractKnowledgeBaseClient {

		// SPARQL Query methods
		
		/**
		 * Executes the query supplied by the calling method and returns results<p>
		 * as a JSONArray.
		 * 
		 * @param sparql query
		 * @return JSONArray
		 */
		public abstract JSONArray executeQuery(String query);	
		
		/**
		 * Executes the query that is provided through the constructors or setter<p>
		 * method and returns the results as a JSONArray.
		 * @return JSONArray
		 */
		public abstract JSONArray executeQuery();
		

		/**
		 * Execute sparql query using the query variable.
		 * @return JSONArray as String 
		 */
		public abstract String execute();
		
		/**
		 * Execute sparql query supplied by the calling method.
		 * @param sparql query
		 * @return JSONArray as String
		 */
		public abstract String execute(String query);
		
		/**
		 * Execute sparql construct query.
		 * @param sparql
		 * @return
		 */
		public abstract Model queryConstruct(Query sparql);
		
		// SPARQL update methods
		
		/**
		 * Executes the update operation that is provided through the constructors or setter<p>
		 * method.
		 */
		public abstract int executeUpdate();

		/**
		 * Executes the update operation supplied by the calling method.
		 * @param sparql update as String
		 */
		public abstract int executeUpdate(String update);
		
		/**
		 * Executes the update operation supplied by the calling method.
		 * @param sparql update as UpdateRequest
		 */
		public abstract int executeUpdate(UpdateRequest update);
		
		// Load and write methods
		
		/**
		 * Loads data to model (required for file-based client)
		 */
		public abstract void load();
		
		/**
		 * Saves and closes connection. end must be called to save changes to file based data.
		 */
		public abstract void end();
		
		// Variable access
		
		/**
		 * Sets the query. 
		 * @param query
		 */
		public abstract String setQuery(String query);

		/**
		 * Returns the available query.
		 */
		public abstract String getQuery();
		
		/**
		 * Can return the URL of the query EndPoint if available.  
		 */
		public abstract String getQueryEndpoint();
		
		/**
		 * Sets the URL of the query EndPoint. 
		 * @param queryEndpoint
		 */
		public abstract String setQueryEndpoint(String queryEndpoint);
		
		/**
		 * Returns the URL of the update EndPoint if available.
		 */
		public abstract String getUpdateEndpoint();
		
		/**
		 * Set the URL of the update EndPoint.
		 * @param updateEndpoint
		 */
		public abstract String setUpdateEndpoint(String updateEndpoint);
	
}
