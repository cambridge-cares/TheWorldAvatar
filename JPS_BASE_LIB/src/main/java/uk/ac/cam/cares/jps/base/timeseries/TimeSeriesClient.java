package uk.ac.cam.cares.jps.base.timeseries;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.interfaces.TimeSeriesClientInterface;
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

public class TimeSeriesClient<T> implements TimeSeriesClientInterface<T>{
	// Associated RDB and RDF/SPARQL clients
	private TimeSeriesRDBClient<T> rdbClient;
	private TimeSeriesSparql rdfClient;
	// Exception prefix
	private final String exceptionPrefix = this.getClass().getSimpleName() + ": ";
	
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
    private void loadRdbConfigs(String filepath) throws IOException {
    	rdbClient.loadRdbConfigs(filepath);
    }
    
    /**
     * Load properties for RDF/SPARQL client
     * @param filepath: Absolute path to properties file with respective information
     */
    private void loadSparqlConfigs(String filepath) throws IOException {
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
	 * Getter for the sparql client to be able to perform SPARQl queries related to timeseries independently.
	 * @return The sparql client used by this general client to access the knowledge graph.
	 */
	public TimeSeriesSparql getRdfClient() {
		return this.rdfClient;
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
	 * Getter for the RDB client to be able to perform database queries related to timeseries independently.
	 * @return The RDB client used by this general client to access the relational database.
	 */
	public TimeSeriesRDBClient<T> getRdbClient() {
		return this.rdbClient;
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
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", e_RdfCreate);
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
    			throw new JPSRuntimeException(exceptionPrefix + "Inconsistent state created when initialising time series " + tsIRI +
						" , as database related instantiation failed but KG triples were created.");
    		}
    		throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", e_RdbCreate);
    	}
    }
    
    /**
     * Delete individual time series in triple store and relational database (i.e. time series for one dataIRI)
     * @param dataIRI: dataIRIs as Strings
     */
    public void deleteIndividualTimeSeries(String dataIRI) {
    	
    	// Check whether dataIRI is associated with any time series and 
    	// Extract "backup" information (tsIRI) for potential later re-instantiation (in case RDB deletion fails)
    	String tsIRI = rdfClient.getTimeSeries(dataIRI);
    	if (tsIRI == null) {
    		throw new JPSRuntimeException(exceptionPrefix + "DataIRI " + dataIRI + " not associated with any timeseries.");
    	}

    	// Check whether associated time series has further data associated with it
    	// If NOT: delete entire time series (i.e. whole tsIRI), if YES: delete only dataIRI time series
    	if (rdfClient.getAssociatedData(tsIRI).size() == 1) {
    		// If not, delete entire time series
    		deleteTimeSeries(tsIRI);
    	} else {
	    	// Step1: Delete time series association in knowledge base
	    	// In case any exception occurs, nothing will be deleted in kb (no partial execution of SPARQL update - only one query)
	   		try {
	   			rdfClient.removeTimeSeriesAssociation(dataIRI);
	   		} catch (Exception e_RdfDelete) {
				throw new JPSRuntimeException(exceptionPrefix + "Timeseries association for " + dataIRI + " was not deleted!", e_RdfDelete);
	   		}
	    	
	    	// Step2: Try to delete corresponding time series column and central table entry in relational database
	    	try {
	    		rdbClient.deleteTimeSeries(dataIRI);
	    	} catch (JPSRuntimeException e_RdbDelete) {
				// For exceptions thrown when deleting RDB elements in relational database,
				// try to revert previous knowledge base deletion
	    		try {
	    			rdfClient.insertTimeSeriesAssociation(dataIRI, tsIRI);
	    		} catch (Exception e_RdfCreate) {
					throw new JPSRuntimeException(exceptionPrefix + "Inconsistent state created when deleting time series association for " + dataIRI +
							" , as database related deletion failed but KG triples were deleted.");
	    		}
				throw new JPSRuntimeException(exceptionPrefix + "Timeseries association for " + dataIRI + " was not deleted!", e_RdbDelete);
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
			throw new JPSRuntimeException(exceptionPrefix + tsIRI + " does not exist in KG");
		}
		
		// Extract "backup" information (dataIRIs, TimeUnit, DBUrl) for potential later re-instantiation (in case RDB deletion fails)
		List<String> dataIRIs = rdfClient.getAssociatedData(tsIRI);
		String timeUnit = rdfClient.getTimeUnit(tsIRI);  // can be null
		
		// Step1: Delete time series with all associations in knowledge base
		// In case any exception occurs, nothing will be deleted in kb (no partial execution of SPARQL update - only one query)
		try {
			rdfClient.removeTimeSeries(tsIRI);
		} catch (Exception e_RdfDelete) {
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries " + tsIRI + " was not deleted!", e_RdfDelete);
		}
		
		// Step2: Try to delete corresponding entries in central table and the time series table in relational database
		try {
			// Retrieve example dataIRI needed to delete RDB related information
			rdbClient.deleteTimeSeriesTable(dataIRIs.get(0));
		} catch (JPSRuntimeException e_RdbDelete) {
			// For exceptions thrown when deleting RDB elements in relational database,
			// try to revert previous knowledge base deletion
			try {
				rdfClient.initTS(tsIRI, dataIRIs, rdbClient.getRdbURL(), timeUnit);
			} catch (Exception e_RdfCreate) {
				throw new JPSRuntimeException(exceptionPrefix + "Inconsistent state created when deleting time series " + tsIRI +
						" , as database related deletion failed but KG triples were deleted.");
			}
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries " + tsIRI + " was not deleted!", e_RdbDelete);
		}
    }
    
    /**
     * Delete all time series and associated connections from triple store and relational database
	 * NOTE: When trying to delete all time series information, NO restore will be tried
	 *     		 in case any exception occurs - only errors for inconsistent states are thrown.
     */
    public void deleteAll() {
		// Step1: Delete all time series in knowledge base
		try {
			// Removing all triples is done by repetitive deletion of time series IRIs from KG
			// -> potentially not all time series could get deleted
			rdfClient.removeAllTimeSeries();
		} catch (Exception e_RdfDelete) {
			throw new JPSRuntimeException(exceptionPrefix + "Not all timeseries were deleted from KG! " +
					  "Potentially inconsistent state between KG and database", e_RdfDelete);
		}
		
		// Step2: Try to delete all time series tables and central lookup table in relational database
		try {
			rdbClient.deleteAll();
		} catch (JPSRuntimeException e_RdbDelete) {
			throw new JPSRuntimeException(exceptionPrefix + "Not all timeseries were deleted from database! " +
					  "Potentially inconsistent state between KG and database", e_RdbDelete);
		}
    }
    
    /**
     * Append time series data to an already instantiated time series
	 * @param ts: timeseries object to add
     */
    public void addTimeSeriesData(TimeSeries<T> ts) {
		
    	// Retrieve relevant dataIRIs
    	List<String> dataIRIs = ts.getDataIRIs();
    	
    	// Check whether all dataIRIs are instantiated in the KG and attached to a time series
    	for (String iri : dataIRIs) {
    		if (!rdfClient.checkDataHasTimeSeries(iri)) {
    			throw new JPSRuntimeException(exceptionPrefix + "DataIRI " + iri + 
    					  " is not attached to any time series instance in the KG");
    		}    		
    	}
    	
    	// Add time series data to respective database table
    	rdbClient.addTimeSeriesData(ts);
    }
    
    /** 
     * Retrieve time series data within given bounds (time bounds are inclusive and optional)
     * <p>Returned time series are in ascending order with respect to time (from oldest to newest)
     * <br>Returned time series contain potential duplicates (i.e. multiple entries for same time stamp)
	 * @param dataIRIs: list of data IRIs provided as string
	 * @param lowerBound: start timestamp from which to retrieve data (null if not applicable)
	 * @param upperBound: end timestamp until which to retrieve data (null if not applicable)
	 * @return Returns all data series from dataIRIs list as single time series object
	 */
	public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRIs, T lowerBound, T upperBound) {
		
    	// Check whether all dataIRIs are instantiated in the KG and attached to a time series
    	for (String iri : dataIRIs) {
    		if (!rdfClient.checkDataHasTimeSeries(iri)) {
    			throw new JPSRuntimeException(exceptionPrefix + "DataIRI " + iri + 
    					  " is not attached to any time series instance in the KG");
    		}    		
    	}
    	
    	// Retrieve time series data from respective database table
    	return rdbClient.getTimeSeriesWithinBounds(dataIRIs, lowerBound, upperBound);
    }
	
    /** 
     * Retrieve entire time series data history for given dataIRIs
     * <p>Returned time series are in ascending order with respect to time (from oldest to newest)
     * <br>Returned time series contain potential duplicates (i.e. multiple entries for same time stamp)
	 * @param dataIRIs: list of data IRIs provided as string
	 * @return Returns all data series from dataIRIs list as single time series object
	 */
	public TimeSeries<T> getTimeSeries(List<String> dataIRIs) {
		
    	return getTimeSeriesWithinBounds(dataIRIs, null, null);
    }
	
	/**
	 * Retrieve average value of an entire time series
	 * @param dataIRI: data IRI provided as string
	 * @return The average of the corresponding data series as double
	 */
	public double getAverage(String dataIRI) {
		
    	// Check whether dataIRI is instantiated in the KG and attached to a time series
		if (!rdfClient.checkDataHasTimeSeries(dataIRI)) {
			throw new JPSRuntimeException(exceptionPrefix + "DataIRI " + dataIRI + 
					  " is not attached to any time series instance in the KG");
		}    		
		
		// Retrieve wanted time series aggregate from database
		return rdbClient.getAverage(dataIRI);
	}
	
	/**
	 * Retrieve maximum value of an entire time series
	 * @param dataIRI: data IRI provided as string
	 * @return The average of the corresponding data series as double
	 */
	public double getMaxValue(String dataIRI) {
		
    	// Check whether dataIRI is instantiated in the KG and attached to a time series
		if (!rdfClient.checkDataHasTimeSeries(dataIRI)) {
			throw new JPSRuntimeException(exceptionPrefix + "DataIRI " + dataIRI + 
					  " is not attached to any time series instance in the KG");
		}    		
		
		// Retrieve wanted time series aggregate from database
		return rdbClient.getMaxValue(dataIRI);
	}
	
	/**
	 * Retrieve minimum value of an entire time series
	 * @param dataIRI: data IRI provided as string
	 * @return The average of the corresponding data series as double
	 */
	public double getMinValue(String dataIRI) {
		
    	// Check whether dataIRI is instantiated in the KG and attached to a time series
		if (!rdfClient.checkDataHasTimeSeries(dataIRI)) {
			throw new JPSRuntimeException(exceptionPrefix + "DataIRI " + dataIRI + 
					  " is not attached to any time series instance in the KG");
		}    		
		
		// Retrieve wanted time series aggregate from database
		return rdbClient.getMinValue(dataIRI);
	}
	
	/**
	 * Retrieve latest (maximum) time entry for a given dataIRI
	 * @param dataIRI: data IRI provided as string
	 * @return The maximum (latest) timestamp of the corresponding data
	 */
	public T getMaxTime(String dataIRI) {
		
    	// Check whether dataIRI is instantiated in the KG and attached to a time series
		if (!rdfClient.checkDataHasTimeSeries(dataIRI)) {
			throw new JPSRuntimeException(exceptionPrefix + "DataIRI " + dataIRI + 
					  " is not attached to any time series instance in the KG");
		}    		
		
		// Retrieve latest time entry from database
		return rdbClient.getMaxTime(dataIRI);
	}
	
	/**
	 * Retrieve earliest (minimum) time entry for a given dataIRI
	 * @param dataIRI: data IRI provided as string
	 * @return The minimum (earliest) timestamp of the corresponding data
	 */
	public T getMinTime(String dataIRI) {
		
    	// Check whether dataIRI is instantiated in the KG and attached to a time series
		if (!rdfClient.checkDataHasTimeSeries(dataIRI)) {
			throw new JPSRuntimeException(exceptionPrefix + "DataIRI " + dataIRI + 
					  " is not attached to any time series instance in the KG");
		}    		
		
		// Retrieve earliest time entry from database
		return rdbClient.getMinTime(dataIRI);
	}
	
	/**
	 * Delete time series history for given dataIRI (and all dataIRIs associated with same time series) between two time stamps
	 * @param dataIRI: data IRI provided as string
	 * @param lowerBound: start timestamp from which to delete data (inclusive)
	 * @param upperBound: end timestamp until which to delete data (inclusive)
	 */
	public void deleteTimeSeriesHistory(String dataIRI, T lowerBound, T upperBound) {
		
    	// Check whether dataIRI is instantiated in the KG and attached to a time series
		if (!rdfClient.checkDataHasTimeSeries(dataIRI)) {
			throw new JPSRuntimeException(exceptionPrefix + "DataIRI " + dataIRI + 
					  " is not attached to any time series instance in the KG");
		}    		
		
		// Delete RDB time series table rows between lower and upper Bound
		rdbClient.deleteRows(dataIRI, lowerBound, upperBound);
	}

}
