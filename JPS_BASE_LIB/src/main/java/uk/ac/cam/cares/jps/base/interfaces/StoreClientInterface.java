package uk.ac.cam.cares.jps.base.interfaces;

import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;

/**
 * This interface is to be implemented by store clients 
 * that establish a connection and perform queries and updates
 * on remote triple stores, relational databases and owl files etc.
 *  
 * @author Casper Lindberg
 */
public interface StoreClientInterface {


	// Query methods

	/**
	 * Executes the query supplied by the calling method and returns results<p>
	 * as a JSONArray.
	 * @param query
	 * @return JSONArray
	 */
	JSONArray executeQuery(String query);	

	/**
	 * Execute sparql query supplied by the calling method.
	 * @param query
	 * @return JSONArray as String
	 */
	String execute(String query);

	/**
	 * Executes the query that is provided through the constructors or setter<p>
	 * method and returns the results as a JSONArray.
	 * @return JSONArray
	 */
	JSONArray executeQuery();

	/**
	 * Execute query using the query variable.
	 * @return JSONArray as String
	 */
	String execute();

	// update methods

	/**
	 * Executes the update operation that is provided through the constructors or setter<p>
	 * method.
	 */
	int executeUpdate();

	/**
	 * Executes the update operation supplied by the calling method.
	 * @param update as String
	 */
	int executeUpdate(String update);
	
	/**
	 * Executes the update operation supplied by the calling method.
	 * @param update as UpdateRequest
	 */
	int executeUpdate(UpdateRequest update);

	/**
	 * Sets the query.
	 * @param query
	 */
	String setQuery(String query);

	/**
	 * Returns the available query.
	 */
	String getQuery();

	// Authentication
	String getUser();

	void setUser(String userName);
	
	String getPassword();

	void setPassword(String password);

}
