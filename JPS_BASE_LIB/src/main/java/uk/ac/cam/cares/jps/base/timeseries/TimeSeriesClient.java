package uk.ac.cam.cares.jps.base.timeseries;

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
	
    /**
     * Standard constructor
     * @param kbClient: Knowledge base client used to query and update the knowledge base containing timeseries information (potentially with already specified endpoint (triplestore/owl file))
     * @param timeClass: Class type for the time values (to initialise RDB table)
     * @param timeUnitIRI: IRI of time unit (to be instantiated in KB) - optional
     */
    public TimeSeriesClient(StoreClientInterface kbClient, Class<T> timeClass, String timeUnitIRI) {
    	this.rdfClient = new TimeSeriesSparql(kbClient);
    	this.rdbClient = new TimeSeriesRDBClient<>(timeClass);
    	// mh807: Not sure if we actually need the timeUnit in the RDB client - potentially to be removed
    	this.rdbClient.setTimeUnit(timeUnitIRI);
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


}
