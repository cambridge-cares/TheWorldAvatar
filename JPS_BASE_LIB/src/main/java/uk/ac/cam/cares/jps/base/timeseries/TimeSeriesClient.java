package uk.ac.cam.cares.jps.base.timeseries;

import java.io.IOException;
import java.util.*;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * This class represents the MAIN interface to interact with time series in The World Avatar
 * 
 * It uses the TimeSeriesRDBClient class to interact with a relational database and the
 * TimeSeriesSparql class to interact with a Triple Store.
 * 
 * @author Markus Hofmeister, Niklas Kasenburg
 * @param <T> is the class type for the time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
 */

public class TimeSeriesClient<T> {
	// Associated RDB and RDF/SPARQL clients
	private TimeSeriesRDBClient<T> rdbClient;
	private TimeSeriesSparql rdfClient;
	// Exception prefix
	private final String exceptionPrefix = this.getClass().getSimpleName() + ": ";
	
    /**
     * Constructor with pre-defined kbClient and RDB client to be created with provided parameters
     * @param kbClient knowledge base client used to query and update the knowledge base containing timeseries information (potentially with already specified endpoint (triplestore/owl file))
     * @param timeClass class type for the time values, e.g. Timestamp etc. (to initialise RDB table)
	 * @param rdbURL URL to relational database (e.g. postgreSQL)
	 * @param user username to access relational database
	 * @param password password to access relational database 
     */
    public TimeSeriesClient(StoreClientInterface kbClient, Class<T> timeClass, String rdbURL, String user, String password) {
    	// Initialise Sparql client with pre-defined kbClient
    	this.rdfClient = new TimeSeriesSparql(kbClient);
    	// Initialise RDB client according to properties file
    	this.rdbClient = new TimeSeriesRDBClient<>(timeClass);
    	// Set RDB credentials
    	this.rdbClient.setRdbURL(rdbURL);
    	this.rdbClient.setRdbUser(user);
    	this.rdbClient.setRdbPassword(password);
    }
	
	/**
     * Constructor with pre-defined kbClient and only RDB client to be created according to properties file
     * @param kbClient knowledge base client used to query and update the knowledge base containing timeseries information (potentially with already specified endpoint (triplestore/owl file))
     * @param timeClass class type for the time values, e.g. Timestamp etc. (to initialise RDB table)
     * @param filepath absolute path to file with RDB configs (URL, username, password) 
     */
    public TimeSeriesClient(StoreClientInterface kbClient, Class<T> timeClass, String filepath) throws IOException {
    	// Initialise Sparql client with pre-defined kbClient
    	this.rdfClient = new TimeSeriesSparql(kbClient);
    	// Initialise RDB client according to properties file
    	this.rdbClient = new TimeSeriesRDBClient<>(timeClass);
   		loadRdbConfigs(filepath);
    }
    
    /**
     * Constructor with both RDB and Sparql clients to be created according to properties file
     * @param timeClass class type for the time values (to initialise RDB table)
     * @param filepath absolute path to file with RDB and KB configs (RDB: URL, username, password; KB: endpoints) 
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
     * @param filepath absolute path to properties file with respective information
     */
    private void loadRdbConfigs(String filepath) throws IOException {
    	rdbClient.loadRdbConfigs(filepath);
    }
    
    /**
     * Load properties for RDF/SPARQL client
     * @param filepath absolute path to properties file with respective information
     */
    private void loadSparqlConfigs(String filepath) throws IOException {
    	rdfClient.loadSparqlConfigs(filepath);   
    }
    
    /**
	 * Setter for knowledge base client (in Sparql client)
	 * @param kbClient knowledge base client used to query and update the knowledge base containing timeseries information with already specified endpoint (triplestore/owl file)
	*/
    public void setKBClient(StoreClientInterface kbClient) {    	
    	this.rdfClient.setKBClient(kbClient);
    }

    /**
	 * Setter for URL and credentials for the relational database (in RDB Client)
	 * @param rdbURL URL to relational database (e.g. postgreSQL)
	 * @param user username to access relational database
	 * @param password password to access relational database 
	*/
    public void setRDBClient(String rdbURL, String user, String password) {    	
    	this.rdbClient.setRdbURL(rdbURL);
    	this.rdbClient.setRdbUser(user);
    	this.rdbClient.setRdbPassword(password);
    }

    /**
     * Initialise time series in triple store and relational database
     * @param dataIRIs list of dataIRIs as Strings
     * @param dataClass list of data classes for each dataIRI
     * @param timeUnit time unit as (full) IRI
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
    		// TODO Ideally try to avoid throwing exceptions in a catch block - potential solution: have removeTimeSeries throw
    		//		a different exception depending on what the problem was, and how it should be handled
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
     * similar to initTimeSeries, but uploads triples in one connection
     */
    public void bulkInitTimeSeries(List<List<String>> dataIRIs, List<List<Class<?>>> dataClass, List<String> timeUnit) {
        // create random time series IRI
    	List<String> tsIRIs = new ArrayList<>(dataIRIs.size());
    	
    	for (int i = 0; i < dataIRIs.size(); i++) {
    		String tsIRI = TimeSeriesSparql.ns_kb + "Timeseries_" + UUID.randomUUID();
    		tsIRIs.add(i, tsIRI);
    	}
    	
    	// Step1: Initialise time series in knowledge base
    	// In case any exception occurs, nothing will be created in kb, since JPSRuntimeException will be thrown before 
    	// interacting with triple store and SPARQL query is either executed fully or not at all (no partial execution possible)
   		try {
   			rdfClient.bulkInitTS(tsIRIs, dataIRIs, rdbClient.getRdbURL(), timeUnit);
		}
		catch (Exception e_RdfCreate) {
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", e_RdfCreate);
		}
   		
   	    // Step2: Try to initialise time series in relational database
   		for (int i = 0; i < dataIRIs.size(); i++) {
   			try {
   	    		rdbClient.initTimeSeriesTable(dataIRIs.get(i), dataClass.get(i), tsIRIs.get(i));
   	    	} catch (JPSRuntimeException e_RdbCreate) {
   	    		// For exceptions thrown when initialising RDB elements in relational database,
   				// try to revert previous knowledge base instantiation
   	    		// TODO Ideally try to avoid throwing exceptions in a catch block - potential solution: have removeTimeSeries throw
   	    		//		a different exception depending on what the problem was, and how it should be handled
   	    		try {
   	    			rdfClient.removeTimeSeries(tsIRIs.get(i));
   	    		} catch (Exception e_RdfDelete) {
   	    			throw new JPSRuntimeException(exceptionPrefix + "Inconsistent state created when initialising time series " + tsIRIs.get(i) +
   							" , as database related instantiation failed but KG triples were created.");
   	    		}
   	    		throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", e_RdbCreate);
   	    	}
   		}	
    }
    
    /**
     * Append time series data to an already instantiated time series
	 * @param ts TimeSeries object to add
     */
    public void addTimeSeriesData(TimeSeries<T> ts) {
    	// Add time series data to respective database table
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		List<TimeSeries<T>> ts_list = new ArrayList<>();
		ts_list.add(ts);
    	rdbClient.addTimeSeriesData(ts_list);
    }

	/**
     * Append time series data to an already instantiated time series 
	 * (i.e. add data for several time series in a single RDB connection)
	 * @param ts_list List of TimeSeries objects to add
     */
    public void bulkaddTimeSeriesData(List<TimeSeries<T>> ts_list) {
    	// Add time series data to respective database tables
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
    	rdbClient.addTimeSeriesData(ts_list);
    }
    
	/**
	 * Delete time series history for given dataIRI (and all dataIRIs associated with same time series) between two time stamps
	 * @param dataIRI data IRI provided as string
	 * @param lowerBound start timestamp from which to delete data (inclusive)
	 * @param upperBound end timestamp until which to delete data (inclusive)
	 */
	public void deleteTimeSeriesHistory(String dataIRI, T lowerBound, T upperBound) {
		// Delete RDB time series table rows between lower and upper Bound
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		rdbClient.deleteRows(dataIRI, lowerBound, upperBound);
	}
    
    /**
     * Delete individual time series in triple store and relational database (i.e. time series for one dataIRI)
     * @param dataIRI dataIRIs as Strings
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
	    		// TODO Ideally try to avoid throwing exceptions in a catch block - potential solution: have insertTimeSeriesAssociation throw
	    		//		a different exception depending on what the problem was, and how it should be handled
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
     * @param tsIRI time series IRI as String
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
    		// TODO Ideally try to avoid throwing exceptions in a catch block - potential solution: have initTS throw
    		//		a different exception depending on what the problem was, and how it should be handled
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
	 *     	 in case any exception occurs - only errors for inconsistent states are thrown.
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
    
    public TimeSeries<T> getLatestData(String dataIRI) {
    	return rdbClient.getLatestData(dataIRI);
    }
    
    public TimeSeries<T> getOldestData(String dataIRI) {
    	return rdbClient.getOldestData(dataIRI);
    }
    
    /** 
     * Retrieve time series data within given bounds (time bounds are inclusive and optional)
     * <p>Returned time series are in ascending order with respect to time (from oldest to newest)
     * <br>Returned time series contain potential duplicates (i.e. multiple entries for same time stamp)
	 * @param dataIRIs list of data IRIs provided as string
	 * @param lowerBound start timestamp from which to retrieve data (null if not applicable)
	 * @param upperBound end timestamp until which to retrieve data (null if not applicable)
	 * @return All data series from dataIRIs list as single TimeSeries object
	 */
	public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRIs, T lowerBound, T upperBound) {
    	// Retrieve time series data from respective database table
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
    	return rdbClient.getTimeSeriesWithinBounds(dataIRIs, lowerBound, upperBound);
    }
	
    /** 
     * Retrieve entire time series data history for given dataIRIs
     * <p>Returned time series are in ascending order with respect to time (from oldest to newest)
     * <br>Returned time series contain potential duplicates (i.e. multiple entries for same time stamp)
	 * @param dataIRIs list of data IRIs provided as string
	 * @return All data series from dataIRIs list as single TimeSeries object
	 */
	public TimeSeries<T> getTimeSeries(List<String> dataIRIs) {
    	return getTimeSeriesWithinBounds(dataIRIs, null, null);
    }
	
	/**
	 * Retrieve average value of an entire time series
	 * @param dataIRI data IRI provided as string
	 * @return The average of the corresponding data series as double
	 */
	public double getAverage(String dataIRI) {
		// Retrieve wanted time series aggregate from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getAverage(dataIRI);
	}
	
	/**
	 * Retrieve maximum value of an entire time series
	 * @param dataIRI data IRI provided as string
	 * @return The average of the corresponding data series as double
	 */
	public double getMaxValue(String dataIRI) {
		// Retrieve wanted time series aggregate from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getMaxValue(dataIRI);
	}
	
	/**
	 * Retrieve minimum value of an entire time series
	 * @param dataIRI data IRI provided as string
	 * @return The average of the corresponding data series as double
	 */
	public double getMinValue(String dataIRI) {
		// Retrieve wanted time series aggregate from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getMinValue(dataIRI);
	}
	
	/**
	 * Retrieve latest (maximum) time entry for a given dataIRI
	 * @param dataIRI data IRI provided as string
	 * @return The maximum (latest) timestamp of the corresponding data series
	 */
	public T getMaxTime(String dataIRI) {
		// Retrieve latest time entry from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getMaxTime(dataIRI);
	}
	
	/**
	 * Retrieve earliest (minimum) time entry for a given dataIRI
	 * @param dataIRI data IRI provided as string
	 * @return The minimum (earliest) timestamp of the corresponding data series
	 */
	public T getMinTime(String dataIRI) {
		// Retrieve earliest time entry from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getMinTime(dataIRI);
	}
	
	/**
	 * Check whether given time series (i.e. tsIRI) exists in kb
	 * @param tsIRI timeseries IRI provided as string
	 * @return True if a time series instance with the tsIRI exists, false otherwise
	 */
    public boolean checkTimeSeriesExists(String tsIRI) {
    	return rdfClient.checkTimeSeriesExists(tsIRI);
    }
    
	/**
	 * Check whether given data IRI is attached to a time series in kb
	 * @param dataIRI data IRI provided as string
	 * @return True if dataIRI exists and is attached to a time series, false otherwise
	 */
    public boolean checkDataHasTimeSeries(String dataIRI) {
    	return rdbClient.checkDataHasTimeSeries(dataIRI);
    }
    
	/**
	 * Check whether given time series IRI has associated time unit in kb
	 * @param tsIRI timeseries IRI provided as string
	 * @return True if tsIRI exists and has a defined time unit, false otherwise
	 */
    public boolean checkTimeUnitExists(String tsIRI) {
    	return rdfClient.checkTimeUnitExists(tsIRI);
    }
    
    /**
     * Count number of time series IRIs in kb
     * @return Total number of time series instances in the knowledge base as int
     */
	public int countTimeSeries() {
		return rdfClient.countTS();
	}
	
	/**
	 * Get time series IRI associated with given data IRI in kb
	 * <p>Returns null if dataIRI does not exist or no time series is attached to dataIRI
	 * @param dataIRI data IRI provided as string
	 * @return The corresponding timeseries IRI as string
	 */
	public String getTimeSeriesIRI(String dataIRI) {
		return rdfClient.getTimeSeries(dataIRI);
	}
	
	/**
	 * Get database URL associated with given time series IRI in kb
	 * <p>Returns null if time series does not exist or does not have associated database URL
	 * @param tsIRI timeseries IRI provided as string
	 * @return The URL to the database where data from that timeseries is stored as string
	 */
	public String getDbUrl(String tsIRI) {
		return rdfClient.getDbUrl(tsIRI);
	}
	
	/**
	 * Get time unit associated with time series IRI in kb
	 * <p>Returns null if time series does not exist or does not have associated time unit
	 * @param tsIRI timeseries IRI provided as string
	 * @return The time unit of timeseries as string
	 */
	public String getTimeUnit(String tsIRI) {
		return rdfClient.getTimeUnit(tsIRI);
	}
	
	/**
	 * Get data IRIs associated with given time series IRI in kb
	 * <p>Returns empty List if time series does not exist or does not have associated data
	 * @param tsIRI timeseries IRI provided as string
	 * @return List of data IRIs attached to the time series as string
	 */
	public List<String> getAssociatedData(String tsIRI) {
		return rdfClient.getAssociatedData(tsIRI);
	}
	
    /**
     * Extract all time series IRIs from kb
     * <p>Returns empty List if no time series exist in kb
     * @return List of all time series IRI in the knowledge base provided as string
     */
	public List<String> getAllTimeSeries() {
		return rdfClient.getAllTimeSeries();
	}
	
	/**
	 * disconnects current connection to postgres
	 */
	public void disconnectRDB() {
		rdbClient.disconnect();
	}
	
	/**
	 * converts list of time series into required format for visualisation
	 * please do not modify without consulting the visualisation team at CMCL
	 * @param ts_list
	 * @param id
	 * @param units_map
	 * @param table_header_map
	 * @return
	 */
	public JSONArray convertToJSON(List<TimeSeries<T>> ts_list, List<Integer> id,
			List<Map<String,String>> units_map, List<Map<String, String>> table_header_map) {
		JSONArray ts_array = new JSONArray();
		
		for (int i = 0; i < ts_list.size(); i++) {
			TimeSeries<T> ts = ts_list.get(i);
			
			JSONObject ts_jo = new JSONObject();
			
			// to link this time series to a station
			// in this application there is only 1 data per ts
			List<String> dataIRIs = ts.getDataIRIs();
			ts_jo.put("id", id.get(i));
			
			// classes
			if (ts.getTimes().size() > 0) {
				if (ts.getTimes().get(0) instanceof Number) {
					ts_jo.put("timeClass", Number.class.getSimpleName());
				} else {
					ts_jo.put("timeClass", ts.getTimes().get(0).getClass().getSimpleName());
				}
			}
			
			// for table headers
			if (table_header_map != null) {
				List<String> table_header = new ArrayList<>();
				for (String dataIRI : dataIRIs) {
					table_header.add(table_header_map.get(i).get(dataIRI));
				}
				ts_jo.put("data", table_header);
			} else {
				ts_jo.put("data", dataIRIs);
			}
	    	
			List<String> units = new ArrayList<>();
			for (String dataIRI : dataIRIs) {
				units.add(units_map.get(i).get(dataIRI));
			}
	    	ts_jo.put("units", units);
	    	
	    	// time column
	    	ts_jo.put("time", ts.getTimes());
	    	
	    	// values columns
	    	// values columns, one array for each data
	    	JSONArray values = new JSONArray();
	    	JSONArray valuesClass = new JSONArray();
	    	for (int j = 0; j < dataIRIs.size(); j++) {
				List<?> valueslist = ts.getValues(dataIRIs.get(j));
				values.put(valueslist);
				// Initialise value class (in case no class can be determined due to missing data)
				String vClass = "Unknown";
				Iterator<?> listIterator = valueslist.iterator();
				while (listIterator.hasNext()) {
					// Get values class from first not null value
					if (listIterator.next() != null) {
						if (listIterator.next() instanceof Number) {
							vClass = Number.class.getSimpleName();
						} else {
							vClass = listIterator.next().getClass().getSimpleName();
						}
						break;
					}
				}
				valuesClass.put(vClass);
			}
	    	
	    	ts_jo.put("values", values);
	    	ts_jo.put("valuesClass", valuesClass);
			
			ts_array.put(ts_jo);
		}
		
		return ts_array;
	}
}
