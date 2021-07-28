package uk.ac.cam.cares.jps.base.timeseries;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * This class represents the MAIN entry point to interact with time series in The World Avatar
 * 
 * It uses the TimeSeriesRDBClient class to interact with the relational database and the
 * TimeSeriesSparql class to interact with the Triple Store.
 * 
 * @author Markus Hofmeister, Niklas Kasenburg
 * @param <T> is the class type for the time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
 */

public class TimeSeriesClient<T> {
	// Associated RDB and RDF/SPARQL clients
	private TimeSeriesRDBClient<T> rdbClient;
	private TimeSeriesSparql rdfClient;
	// Exception prefix
	private final String exceptionPrefix = this.getClass().toString() + ": ";
	
    /**
     * Constructor for pre-defined kbClient and only RDB client to be created according to properties file
     * @param kbClient: Knowledge base client used to query and update the knowledge base containing timeseries information (potentially with already specified endpoint (triplestore/owl file))
     * @param timeClass: Class type for the time values, e.g. Timestamp etc. (to initialise RDB table)
     * @param filepath: Absolute path to file with RDB configs (URL, username, password) 
     */
    public TimeSeriesClient(StoreClientInterface kbClient, Class<T> timeClass, String filepath) throws IOException {
    	// Initialise Sparql client with pre-defined kbClient
    	this.rdfClient = new TimeSeriesSparql(kbClient);
    	// Initialise RDB client according to properties file
    	this.rdbClient = new TimeSeriesRDBClient<>(timeClass);
   		loadRdbConfigs(filepath);
    }
    
    /**
     * Constructor for both RDB and Sparql clients to be created according to properties file
     * @param timeClass: Class type for the time values (to initialise RDB table)
     * @param filepath: Absolute path to file with RDB and KB configs (RDB: URL, username, password; KB: endpoints) 
     */
    public TimeSeriesClient(Class<T> timeClass, String filepath) throws IOException {
    	// Initialise Sparql client according to properties file
    	RemoteStoreClient kbClient = new RemoteStoreClient();
    	this.rdfClient = new TimeSeriesSparql(kbClient);
    	loadSparqlConfigs(filepath);
    	// Initialise RDB client according to properties file
    	this.rdbClient = new TimeSeriesRDBClient<>(timeClass);
    	loadRdbConfigs(filepath);
    }
    
    /**
     * Load properties for RDB client
     * @param filepath: Absolute path to properties file with respective information
     */
    public void loadRdbConfigs(String filepath) throws IOException {    	
    	rdbClient.loadRdbConfigs(filepath);
    }
    
    /**
     * Load properties for RDF/SPARQL client
     * @param filepath: Absolute path to properties file with respective information
     */
    public void loadSparqlConfigs(String filepath) throws IOException {    	
    	rdfClient.loadSparqlConfigs(filepath);   
    }
    
    /**
	 * Setter for knowledge base client (in Sparql client)
	 * @param kbClient: Knowledge base client used to query and update the knowledge base containing timeseries information with already specified endpoint (triplestore/owl file)
	*/
    public void setKBClient(StoreClientInterface kbClient) {    	
    	this.rdfClient.setKBClient(kbClient);
    }
    
    /**
	 * Setter for URL and credentials for the relational database (in RDB Client)
	 * @param rdbURL: URL to relational database (e.g. postgreSQL)
	 * @param user: username to access relational database
	 * @param password: password to access relational database 
	*/
    public void setRDBClient(String rdbURL, String user, String password) {    	
    	this.rdbClient.setRdbURL(rdbURL);
    	this.rdbClient.setRdbUser(user);
    	this.rdbClient.setRdbPassword(password);
    }
    
    /**
     * Initialise time series in triple store and relational database
     * @param dataIRIs: List of (full) dataIRIs as Strings
     * @param dataClass: List of data classes for each dataIRI
     * @param timeUnit: time unit as (full) IRI
     * @return Returns "Success" if initialisation was successful in both the triple store and the relational database
     * 		   <br>Returns "No changes: &lt;error description&gt;" in case there was an error; however, prior state could be maintained/recovered
     * 		   <br>Returns "Potentially inconsistent state: &lt;time series IRI&gt;" in case there was an error and NOT all changes could be reverted  
     * 		   <br>(i.e. there is an inconsistent state between triple store and database and subsequent cleanup for tsIRI is required)
     */
    public String initTimeSeries(List<String> dataIRIs, List<Class<?>> dataClass, String timeUnit) {
    	
    	// Initialise boolean flags to indicate success/failure of potentially necessary deletion (in case any errors occur)
    	boolean rdb_deletion = true;
    	boolean rdf_deletion = true;
 	
    	// Create random time series IRI in the format: <ClassName>_<UUID>
    	String tsIRI = rdfClient.ns_kb + "Timeseries_" + UUID.randomUUID().toString();
    	
    	// Step1: Initialise time series in knowledge base
    	// In case any exception occurs, nothing will be created in kb, since JPSRuntimeException will be thrown before 
    	// interacting with triple store and SPARQL query is either executed fully or not at all (no partial execution possible)
   		try {
   			rdfClient.initTS(tsIRI, dataIRIs, rdbClient.getRdbURL(), timeUnit);
   		} catch (Exception e_RdfCreate) {
   			e_RdfCreate.printStackTrace();
   			return "No change: " + e_RdfCreate.getMessage();   			
   		}
    	
    	// Step2: Try to initialise time series in relational database
    	try {
    		rdbClient.initTimeSeriesTable(dataIRIs, dataClass, tsIRI);
    	} catch (JPSRuntimeException e_RdbCreate) {
    		// For RDB exceptions thrown after interaction with relational database started, try to revert 
    		// whatever might have been instantiated in relational database
    		if (e_RdbCreate.getMessage().contains("Error while executing SQL command")) {
       			try {
        			// Try to delete potentially created time series table and corresponding entries in central lookup table
    				rdbClient.deleteTimeSeriesTable(dataIRIs.get(0));
    			} catch(Exception e_RdbDelete) { 
    				// RDB deletion potentially unsuccessful
   					rdb_deletion = false;
    			}
    		}
    		
    		// For ALL RDB exceptions, try to revert previous knowledge base instantiation
    		try {
    			rdfClient.removeTimeSeries(tsIRI);
    		} catch (Exception e_RdfDelete) {
    			// RDF deletion potentially unsuccessful
    			rdf_deletion = false;
    		}
    		
    		// Return messages depending on exceptions occurred
    		if (rdb_deletion && rdf_deletion) {
    			return "No change: " + e_RdbCreate.getCause().getMessage();
    		} else {
    			return "Potentially inconsistent state: " + tsIRI;
    		}    		
    	}    	
    	// Return success in case no exceptions occurred
    	return "Success";
    }
    



}
