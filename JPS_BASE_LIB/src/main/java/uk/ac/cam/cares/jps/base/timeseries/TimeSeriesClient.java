package uk.ac.cam.cares.jps.base.timeseries;

import java.util.List;
import java.util.UUID;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

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
     * Standard constructor
     * @param kbClient: Knowledge base client used to query and update the knowledge base containing timeseries information (potentially with already specified endpoint (triplestore/owl file))
     * @param timeClass: Class type for the time values (to initialise RDB table)
     * @param timeUnitIRI: IRI of time unit (to be instantiated in KB) - optional
     */
    public TimeSeriesClient(StoreClientInterface kbClient, Class<T> timeClass, String timeUnitIRI) {
    	this.rdfClient = new TimeSeriesSparql(kbClient);
    	this.rdbClient = new TimeSeriesRDBClient<>(timeClass);
    }
    
    /**
     * Load properties for both RDB and RDF/SPARQL client
     * @param filepath: (Relative) file path to properties file with respective information
     */
    public void loadConfigs(String filepath) {    	
    	rdbClient.loadRdbConfigs(filepath);
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
     */
    public void initTimeSeries(List<String> dataIRIs, List<Class<?>> dataClass, String timeUnit) {
    	
    	// Create random time series IRI in the format: <ClassName>_<UUID>
    	String tsIRI = rdfClient.ns_kb + "Timeseries_" + UUID.randomUUID().toString();
    	
    	// Initialise boolean flag to re-run knowledge base instantiation in case tsIRI is already taken
    	boolean not_created = true;    	    	
    	while (not_created) {
	    	// Try to initialise time series in knowledge base
	    	try {
	    		rdfClient.initTS(tsIRI, dataIRIs, rdbClient.getRdbURL(), timeUnit);
	    		// Adjust flag if successfully created
	    		not_created = false;
	    	} catch(Exception e_create) {
    			// Throw exceptions for not passed checks in TimeSeriesSparql.initTS
    			// Do not throw exception for already existing tsIRI to allow for 2nd instantiation loop
	    		if (e_create instanceof JPSRuntimeException &&
	    			 (e_create.getMessage().contains("Time series IRI does not have valid IRI format") ||
	        		  e_create.getMessage().contains("The data IRI " + tsIRI + " is already attached to time series"))) {
	    			throw new JPSRuntimeException(exceptionPrefix + "Nothing has been instantiated due to the following error: \n" +
	        									  e_create.getMessage());
	    		} else {
	    			// For all exceptions during instantiation, try to delete whatever has been created
	    			try {
	    				rdfClient.removeTimeSeries(tsIRI);
	    			} catch (Exception e_delete) {
	    				throw new JPSRuntimeException(exceptionPrefix + "An error occurred during instantiating the time series in the KB.\n" +
	    											  "Intended changes in the KB could NOT be successfully reverted.");
	    			}
	    			throw new JPSRuntimeException(exceptionPrefix + "An error occurred during instantiating the time series in the KB.\n" +
	    										  "Intended changes in the KB have been successfully reverted.");
	    		}
	    	}
    	}
    	
    	// Try to initialise time series in relational database
    	try {
    		rdbClient.initTimeSeriesTable(dataIRIs, dataClass, tsIRI);
    	} catch (Exception e_create) {
    		// Throw exceptions for not passed checks in TimeSeriesRDBCLient.initTimeSeriesTable
    		if (e_create instanceof JPSRuntimeException &&
    			 (e_create.getMessage().contains("already has a time series instance (i.e. tsIRI)") ||
        		  e_create.getMessage().contains("Length of dataClass is different from number of data IRIs"))) {
    			// Try to delete previously initialised time series in knowledge base
    			try {
    				rdfClient.removeTimeSeries(tsIRI);
    			} catch (Exception e_delete) {
    				throw new JPSRuntimeException(exceptionPrefix + "An error occurred during instantiating the time series in the RDB.\n" +
    											  "Nothing has been instantiated in the RDB, but respective changes could NOT be successfully reverted in the KB.");
    			}
    			throw new JPSRuntimeException(exceptionPrefix + "An error occurred during instantiating the time series in the RDB.\n" +
						  					  "Nothing has been instantiated in the RDB and respective changes have been successfully reverted in the KB.");
    			} else {
    				// For all exceptions during instantiation, try to delete whatever has been created
    				try {
    					rdbClient.deleteTimeSeriesTable(dataIRIs.get(0)); 
    				} catch (Exception e_delete) {
    	    			// Try to delete previously initialised time series in knowledge base
    	    			try {
    	    				rdfClient.removeTimeSeries(tsIRI);
    	    			} catch (Exception e_delete_rdf) {
    	    				throw new JPSRuntimeException(exceptionPrefix + "An error occurred during instantiating the time series in the RDB.\n" +
    	    											  "Intended changes could NOT be successfully reverted in both RDB and KB.");
    	    			}
    	    			throw new JPSRuntimeException(exceptionPrefix + "An error occurred during instantiating the time series in the RDB.\n" +
			  					  					  "Intended changes have been successfully reverted in KB, but NOT in RDB.");
    				}
        			// Try to delete previously initialised time series in knowledge base
        			try {
        				rdfClient.removeTimeSeries(tsIRI);
        			} catch (Exception e_delete_rdf) {
        				throw new JPSRuntimeException(exceptionPrefix + "An error occurred during instantiating the time series in the RDB.\n" +
        											  "Intended changes have been successfully reverted in RDB, but NOT in KB.");
        			}
        			throw new JPSRuntimeException(exceptionPrefix + "An error occurred during instantiating the time series in the RDB.\n" +
		  					  					  "Intended changes have been successfully reverted in both RDB and KB.");
    				   				
    			}    		
    	}
    	
    }


}
