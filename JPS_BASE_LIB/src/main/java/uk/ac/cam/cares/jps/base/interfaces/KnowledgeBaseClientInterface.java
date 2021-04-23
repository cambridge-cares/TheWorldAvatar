package uk.ac.cam.cares.jps.base.interfaces;

import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;


/**
 * This interface is to be implemented by knowledge base clients 
 * that establish a connection and perform SPARQL queries and updates
 * on remote triple stores, owl files etc. 
 *  
 * @author Casper Lindberg
 */
public interface KnowledgeBaseClientInterface {

	// Get/Put methods
	
	String get(String resourceUrl, String accept);
	
	void put(String resourceUrl, String content, String contentType);
	
	// SPARQL Query methods
	
	/**
	 * Executes the query supplied by the calling method and returns results<p>
	 * as a JSONArray.
	 * 
	 * @param sparql query
	 * @return JSONArray
	 */
	JSONArray executeQuery(String query);	
	
	/**
	 * Executes the query that is provided through the constructors or setter<p>
	 * method and returns the results as a JSONArray.
	 * @return JSONArray
	 */
	JSONArray executeQuery();
	

	/**
	 * Execute sparql query using the query variable.
	 * @return JSONArray as String 
	 */
	String execute();
	
	/**
	 * Execute sparql query supplied by the calling method.
	 * @param sparql query
	 * @return JSONArray as String
	 */
	String execute(String query);
	
	// SPARQL Construct query methods
	
	/**
	 * Execute sparql construct query.
	 * @param sparql
	 * @return
	 */
	Model executeConstruct(Query sparql);
	
	/**
	 * Execute sparql construct query.
	 * @param sparql
	 * @return
	 */
	Model executeConstruct(String sparql);
	
	// SPARQL update methods
	
	/**
	 * Executes the update operation that is provided through the constructors or setter<p>
	 * method.
	 */
	int executeUpdate();

	/**
	 * Executes the update operation supplied by the calling method.
	 * @param sparql update as String
	 */
	int executeUpdate(String update);
	
	/**
	 * Executes the update operation supplied by the calling method.
	 * @param sparql update as UpdateRequest
	 */
	int executeUpdate(UpdateRequest update);
	
	
	// Set/Get varaible methods
	
	/**
	 * Sets the query. 
	 * @param query
	 */
	String setQuery(String query);

	/**
	 * Returns the available query.
	 */
	String getQuery();
	
	/**
	 * Can return the URL of the query EndPoint if available.  
	 */
	String getQueryEndpoint();
	
	/**
	 * Sets the URL of the query EndPoint. 
	 * @param queryEndpoint
	 */
	String setQueryEndpoint(String queryEndpoint);
	
	/**
	 * Returns the URL of the update EndPoint if available.
	 */
	String getUpdateEndpoint();
	
	/**
	 * Set the URL of the update EndPoint.
	 * @param updateEndpoint
	 */
	String setUpdateEndpoint(String updateEndpoint);

	// Authentication
	String getUser();

	void setUser(String userName);
	
	String getPassword();

	void setPassword(String password);
}
