package uk.ac.cam.cares.jps.base.timeseries;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

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
	private final TimeSeriesRDBClient<T> rdbClient;
	private final TimeSeriesSparql rdfClient;
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
     * @param dataIRIs: List of dataIRIs as Strings
     * @param dataClass: List of data classes for each dataIRI
     * @param timeUnit: time unit as (full) IRI
     */
    public void initTimeSeries(List<String> dataIRIs, List<Class<?>> dataClass, String timeUnit) {

    	// Create random time series IRI in the format: <Namespace><ClassName>_<UUID>
    	String tsIRI = TimeSeriesSparql.ns_kb + "Timeseries_" + UUID.randomUUID();
    	
    	// Step1: Initialise time series in knowledge base
    	// In case any exception occurs, nothing will be created in kb, since JPSRuntimeException will be thrown before 
    	// interacting with triple store and SPARQL query is either executed fully or not at all (no partial execution possible)
   		try {
   			rdfClient.initTS(tsIRI, dataIRIs, rdbClient.getRdbURL(), timeUnit);
		}
		catch (Exception e_RdfCreate) {
			throw new JPSRuntimeException(exceptionPrefix + "timeseries was not created!", e_RdfCreate);
		}
    	
    	// Step2: Try to initialise time series in relational database
    	try {
    		rdbClient.initTimeSeriesTable(dataIRIs, dataClass, tsIRI);
    	} catch (JPSRuntimeException e_RdbCreate) {
    		// For exceptions thrown when initialising RDB elements in relational database,
			// try to revert previous knowledge base instantiation
    		try {
    			rdfClient.removeTimeSeries(tsIRI);
    		} catch (Exception e_RdfDelete) {
    			throw new JPSRuntimeException(exceptionPrefix + "inconsistent state created when initialising time series " + tsIRI +
						" , as database related instantiation failed but KG triples were created.");
    		}
    		throw new JPSRuntimeException(exceptionPrefix + "timeseries was not created!", e_RdbCreate);
    	}
    }
    
    /**
     * Delete individual time series in triple store and relational database (i.e. time series for one dataIRI)
     * @param dataIRI: dataIRIs as Strings
     * @return Returns "Success" if deletion was successful in both the triple store and the relational database
     * 		   <br>Returns "No changes: &lt;error description&gt;" in case there was an error; however, prior state could be maintained/recovered
     * 		   <br>Returns "Potentially inconsistent state: &lt;data IRI&gt;" in case there was an error and NOT all changes could be reverted  
     * 		   <br>(i.e. there is an inconsistent state between triple store and database and subsequent cleanup for dataIRI is required)
     */
    public void deleteIndividualTimeSeries(String dataIRI) {
    	
    	// Check whether dataIRI is associated with any time series
    	String tsIRI = rdfClient.getTimeSeries(dataIRI);
    	if (tsIRI == null) {
    		throw new JPSRuntimeException(exceptionPrefix + "dataIRI " + dataIRI + " not associated with any timeseries.");
    	}

    	// Check whether associated time series has further data associated with it and
    	if (rdfClient.getAssociatedData(tsIRI).size() == 1) {
    		// Delete entire time series
    		deleteTimeSeries(tsIRI);
    	} else {
	    	// Step2: Delete time series association in knowledge base
	    	// In case any exception occurs, nothing will be deleted in kb (no partial execution of SPARQL update - only one query)
	   		try {
	   			rdfClient.removeTimeSeriesAssociation(dataIRI);
	   		} catch (Exception e_RdfDelete) {
				throw new JPSRuntimeException(exceptionPrefix + "timeseries association for " + dataIRI + " was not deleted!", e_RdfDelete);
	   		}
	    	
	    	// Step3: Try to delete corresponding time series column and central table entry in relational database
	    	try {
	    		rdbClient.deleteTimeSeries(dataIRI);
	    	} catch (JPSRuntimeException e_RdbDelete) {
				// For exceptions thrown when deleting RDB elements in relational database,
				// try to revert previous knowledge base deletion
	    		try {
	    			rdfClient.insertTimeSeriesAssociation(dataIRI, tsIRI);
	    		} catch (Exception e_RdfCreate) {
					throw new JPSRuntimeException(exceptionPrefix + "inconsistent state created when deleting time series association for " + dataIRI +
							" , as database related deletion failed but KG triples were deleted.");
	    		}
				throw new JPSRuntimeException(exceptionPrefix + "timeseries association for " + dataIRI + " was not deleted!", e_RdbDelete);
	    	}
    	}
    }

    /**
     * Delete time series and all associated dataIRI connections from triple store and relational database 
     * @param tsIRI: time series IRI as String
     */
    public void deleteTimeSeries(String tsIRI) {
		// Check whether tsIRI exists
		if (!rdfClient.checkTimeSeriesExists(tsIRI)) {
			throw new JPSRuntimeException(exceptionPrefix + tsIRI + " does not exist.");
		}

		// Retrieve example dataIRI needed to delete RDB related information
		List<String> dataIRI = rdfClient.getAssociatedData(tsIRI);
		String timeUnit = null;
		// Step1: Delete time series with all associations in knowledge base
		// In case any exception occurs, nothing will be deleted in kb (no partial execution of SPARQL update - only one query)
		try {
			// Retrieve information about time series to be able to revert if RDB deletion fails
			timeUnit = rdfClient.getTimeUnit(tsIRI); // can be null
			rdfClient.removeTimeSeries(tsIRI);
		} catch (Exception e_RdfDelete) {
			throw new JPSRuntimeException(exceptionPrefix + "timeseries " + tsIRI + " was not deleted!", e_RdfDelete);
		}
		// Step2: Try to delete corresponding entries in central table and the time series table in relational database
		try {
			rdbClient.deleteTimeSeriesTable(dataIRI.get(0));
		} catch (JPSRuntimeException e_RdbDelete) {
			// For exceptions thrown when deleting RDB elements in relational database,
			// try to revert previous knowledge base deletion
			try {
				rdfClient.initTS(tsIRI, dataIRI, rdbClient.getRdbURL(), timeUnit);
			} catch (Exception e_RdfCreate) {
				throw new JPSRuntimeException(exceptionPrefix + "inconsistent state created when deleting time series " + tsIRI +
						" , as database related deletion failed but KG triples were deleted.");
			}
			throw new JPSRuntimeException(exceptionPrefix + "timeseries " + tsIRI + " was not deleted!", e_RdbDelete);
		}
    }

}
