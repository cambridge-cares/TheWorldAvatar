package uk.ac.cam.cares.jps.base.timeseries;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.*;

import org.jooq.tools.jdbc.MockConnection;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
/**
 * This class represents the MAIN interface to interact with time series in The World Avatar
 * 
 * It uses the TimeSeriesRDBClient class to interact with a relational database and the
 * TimeSeriesSparql class to interact with a Triple Store.
 * 
 * The methods in this class are roughly separated into two categories:
 * 1) methods that receive a connection object in the argument, e.g. initTimeSeries(List<String>, List<Class<?>>, String, Connection)
 * 2) methods that do not need a connection object initTimeSeries(List<String>, List<Class<?>>, String)
 *
 * The main motivation for the methods with the connection object is to improve performance in codes that need to interact with the RDB
 * repetitively, as closing the connection each time causes performance issues.
 * To create a connection object: create an instance of {@link uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient RemoteRDBStoreClient}
 * and use {@link RemoteRDBStoreClient#getConnection()} method to obtain the connection object.
 * Example:
 * TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<>(storeClient, Instant.class);
 * RDBStoreClient rdbStoreClient = new RDBStoreClient(url, user, password);
 * try (Connection conn = rdbStoreClient.getConnection()) {
 *     TimeSeries ts = TimeSeriesClient.getTimeSeriesWithinBounds(dataIRIs, lowerbound, upperbound, conn);
 *     TimeSeriesClient.addTimeSeriesData(ts, conn);
 *     // other methods can be called similarly in this block
 * }
 * Note: The connection object should be created using Java's try-with-resources block (https://www.baeldung.com/java-try-with-resources)
 * as shown in the example above. This is to ensure the connection is closed automatically by Java.
 *
 * To use the methods without the connection argument, you must use the constructors with the RDB endpoint in it, e.g.
 * TimeSeriesClient(TripleStoreClientInterface kbClient, Class<T> timeClass, String rdbURL, String user, String password).
 * These methods open a single connection with try-with-resources for each call.
 * @author
 * @author Markus Hofmeister, Niklas Kasenburg, Mehal Agarwal (ma988@cam.ac.uk), Kok Foong Lee
 * @param <T> is the class type for the time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
 */

public class TimeSeriesClient<T> {
	private static final Logger LOGGER = LogManager.getLogger(TimeSeriesClient.class);
	// Associated RDB and RDF/SPARQL clients
	private TimeSeriesRDBClient<T> rdbClient;
	private TimeSeriesSparql rdfClient;
	// Exception prefix
	private final String exceptionPrefix = this.getClass().getSimpleName() + ": ";
	private static final String CONNECTION_ERROR = "Failed to connect to database. If you are using the methods without the connection argument, " +
	"the RDB endpoint (URL, username and password) needs to be set in the constructor of TimeSeriesClient";

    /**
     * Constructor with pre-defined kbClient
     * @param kbClient knowledge base client used to query and update the knowledge base containing timeseries information (potentially with already specified endpoint (triplestore/owl file))
     * @param timeClass class type for the time values, e.g. Timestamp etc. (to initialise RDB table)
     */
    public TimeSeriesClient(TripleStoreClientInterface kbClient, Class<T> timeClass) {
    	// Initialise Sparql client with pre-defined kbClient
    	this.rdfClient = new TimeSeriesSparql(kbClient);
    	// Initialise RDB client
		this.rdbClient = new TimeSeriesRDBClient<>(timeClass);
    }
    
    /**
	 * Setter for knowledge base client (in Sparql client)
	 * @param kbClient knowledge base client used to query and update the knowledge base containing timeseries information with already specified endpoint (triplestore/owl file)
	*/
    public void setKBClient(TripleStoreClientInterface kbClient) {
    	this.rdfClient.setKBClient(kbClient);
    }

	/**
	 * Initialise time series in triple store and relational database
	 * @param dataIRIs list of dataIRIs as Strings
	 * @param dataClass list of data classes for each dataIRI
	 * @param timeUnit time unit as (full) IRI
	 * @param conn connection to the RDB
	 * @param type type of TimeSeries data to be instantiated. (optional)
	 *             Allowed values:
	 *             https://www.theworldavatar.com/kg/ontotimeseries/StepwiseCumulative,
	 *             https://www.theworldavatar.com/kg/ontotimeseries/CumulativeTotal,
	 *             https://www.theworldavatar.com/kg/ontotimeseries/Instantaneous,
	 *             https://www.theworldavatar.com/kg/ontotimeseries/Average
	 *             If not specified, default value: TimeSeries
	 * @param duration Required for Average Time Series. Numeric duration of the averaging period for Average TimeSeries of type Duration. Only positive values are allowed. (optional)
	 * @param unit Required for Average Time Series. Temporal unit type of the averaging period for Average TimeSeries. (optional)
	 *             Allowed values of type ChronoUnit:
	 *             ChronoUnit.SECONDS, ChronoUnit.MINUTES, ChronoUnit.HOURS, ChronoUit.DAYS, ChronoUnit.WEEKS, ChronoUnit.MONTHS, ChronoUnit.YEARS
	 *
	 */
	public void initTimeSeries(List<String> dataIRIs, List<Class<?>> dataClass, String timeUnit, Connection conn, String type, Duration duration, ChronoUnit unit) {
		String tsIRI;
		// Create random time series IRI in the format: <Namespace><ClassName>_<UUID>
		if(type.equals(TimeSeriesSparql.StepwiseCumulative)){
			tsIRI = TimeSeriesSparql.StepwiseCumulative + "Timeseries_" + UUID.randomUUID();
		}
		else if(type.equals(TimeSeriesSparql.CumulativeTotal)){
			tsIRI = TimeSeriesSparql.CumulativeTotal+  "Timeseries_" + UUID.randomUUID();
		}
		else if(type.equals(TimeSeriesSparql.Instantaneous)){
			tsIRI = TimeSeriesSparql.Instantaneous + "Timeseries_" + UUID.randomUUID();
		}
		else if(type.equals(TimeSeriesSparql.Average)){
			tsIRI = TimeSeriesSparql.Average + "Timeseries_" + UUID.randomUUID();
		}
		else {
			tsIRI = TimeSeriesSparql.ns_kb + "Timeseries_" + UUID.randomUUID();
		}

		// Step1: Initialise time series in knowledge base
		// In case any exception occurs, nothing will be created in kb, since JPSRuntimeException will be thrown before
		// interacting with triple store and SPARQL query is either executed fully or not at all (no partial execution possible)
		try {
			rdfClient.initTS(tsIRI, dataIRIs, conn.getMetaData().getURL(), timeUnit, type, duration, unit);
		}
		catch (Exception eRdfCreate) {
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", eRdfCreate);
		}
    	
    	// Step2: Try to initialise time series in relational database
    	try {
			rdbClient.initTimeSeriesTable(dataIRIs, dataClass, tsIRI, conn);
    	} catch (JPSRuntimeException eRdbCreate) {
    		// For exceptions thrown when initialising RDB elements in relational database,
			// try to revert previous knowledge base instantiation
    		// TODO Ideally try to avoid throwing exceptions in a catch block - potential solution: have removeTimeSeries throw
    		//		a different exception depending on what the problem was, and how it should be handled
    		try {
    			rdfClient.removeTimeSeries(tsIRI);
    		} catch (Exception eRdfDelete) {
    			throw new JPSRuntimeException(exceptionPrefix + "Inconsistent state created when initialising time series " + tsIRI +
						" , as database related instantiation failed but KG triples were created.", eRdfDelete);
    		}
    		throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", eRdbCreate);
    	}
    }
    
	/**
     * similar to initTimeSeries, but uploads triples in one connection
	 * @param dataIRIs
	 * @param dataClass
	 * @param timeUnit
	 * @param conn
	 * @param type
	 * @param durations
	 * @param units
     */
	public void bulkInitTimeSeries(List<List<String>> dataIRIs, List<List<Class<?>>> dataClass, List<String> timeUnit, Connection conn, List<String> type, List<Duration> durations, List<ChronoUnit> units) {
		bulkInitTimeSeries(dataIRIs, dataClass, timeUnit, null, conn, type, durations, units);
	} 

	/**
	 * similar to initTimeSeries, but uploads triples in one connection
	 * srid is used if the time series values contain geometries
	 * @param dataIRIs
	 * @param dataClass
	 * @param timeUnit
	 * @param srid
	 * @param conn
	 * @param type
	 * @param durations
	 * @param units
	 */
    public void bulkInitTimeSeries(List<List<String>> dataIRIs, List<List<Class<?>>> dataClass, List<String> timeUnit, Integer srid, Connection conn, List<String> type, List<Duration> durations, List<ChronoUnit> units) {
        // create random time series IRI
    	List<String> tsIRIs = new ArrayList<>(dataIRIs.size());
		String tsIRI;
    	for (int i = 0; i < dataIRIs.size(); i++) {
			if(type.get(i)=="StepwiseCumulative"){
				tsIRI = TimeSeriesSparql.ns_kb + "StepwiseCumulativeTimeseries_" + UUID.randomUUID();
			}
			else if(type.get(i)=="CumulativeTotal"){
				tsIRI = TimeSeriesSparql.ns_kb + "CumulativeTotalTimeseries_" + UUID.randomUUID();
			}
			else if(type.get(i) == "Instantaneous"){
				tsIRI = TimeSeriesSparql.ns_kb + "InstantaneousTimeseries_" + UUID.randomUUID();
			}
			else if(type.get(i)=="Average"){
				tsIRI = TimeSeriesSparql.ns_kb + "AverageTimeseries_" + UUID.randomUUID();
			}
			else {
				tsIRI = TimeSeriesSparql.ns_kb + "Timeseries_" + UUID.randomUUID();
			}
    		tsIRIs.add(i, tsIRI);
    	}
    	
    	// Step1: Initialise time series in knowledge base
    	// In case any exception occurs, nothing will be created in kb, since JPSRuntimeException will be thrown before 
    	// interacting with triple store and SPARQL query is either executed fully or not at all (no partial execution possible)


		// Obtain RDB URL from connection object, exception thrown when connection is down
		String rdbURL;
		try {
			rdbURL = conn.getMetaData().getURL();
		} catch (SQLException e) {
			// this ensures rdfClient.bulkInitTS always has a valid string
			LOGGER.warn(e.getMessage());
			LOGGER.warn("Failed to get RDB URL from connection object, setting RDB URL to = \"\"");
			rdbURL = "";
		}
		try {
			rdfClient.bulkInitTS(tsIRIs, dataIRIs, conn.getMetaData().getURL(), timeUnit, type, durations, units);
		}
		catch (Exception eRdfCreate) {
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", eRdfCreate);
		}
   		
   	    // Step2: Try to initialise time series in relational database
   		for (int i = 0; i < dataIRIs.size(); i++) {
   			try {
				rdbClient.initTimeSeriesTable(dataIRIs.get(i), dataClass.get(i), tsIRIs.get(i), srid, conn);
   	    	} catch (JPSRuntimeException eRdbCreate) {
   	    		// For exceptions thrown when initialising RDB elements in relational database,
   				// try to revert previous knowledge base instantiation
   	    		// TODO Ideally try to avoid throwing exceptions in a catch block - potential solution: have removeTimeSeries throw
   	    		//		a different exception depending on what the problem was, and how it should be handled
   	    		try {
   	    			rdfClient.removeTimeSeries(tsIRIs.get(i));
   	    		} catch (Exception eRdfDelete) {
   	    			throw new JPSRuntimeException(exceptionPrefix + "Inconsistent state created when initialising time series " + tsIRIs.get(i) +
   							" , as database related instantiation failed but KG triples were created.");
   	    		}
   	    		throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", eRdbCreate);
   	    	}
   		}	
    }
    
    /**
     * Append time series data to an already instantiated time series
	 * @param ts TimeSeries object to add
	 * @param conn connection to the RDB
     */
    public void addTimeSeriesData(TimeSeries<T> ts, Connection conn) {
    	// Add time series data to respective database table
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		List<TimeSeries<T>> tsList = new ArrayList<>();
		tsList.add(ts);
		rdbClient.addTimeSeriesData(tsList, conn);
    }

	/**
     * Append time series data to an already instantiated time series 
	 * (i.e. add data for several time series in a single RDB connection)
	 * @param tsList List of TimeSeries objects to add
	 * @param conn connection to the RDB
     */
    public void bulkaddTimeSeriesData(List<TimeSeries<T>> tsList, Connection conn) {
    	// Add time series data to respective database tables
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
    	rdbClient.addTimeSeriesData(tsList, conn);
    }
    
	/**
	 * Delete time series history for given dataIRI (and all dataIRIs associated with same time series) between two time stamps
	 * @param dataIRI data IRI provided as string
	 * @param lowerBound start timestamp from which to delete data (inclusive)
	 * @param upperBound end timestamp until which to delete data (inclusive)
	 * @param conn connection to the RDB
	 */
	public void deleteTimeSeriesHistory(String dataIRI, T lowerBound, T upperBound, Connection conn) {
		// Delete RDB time series table rows between lower and upper Bound
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		rdbClient.deleteRows(dataIRI, lowerBound, upperBound, conn);
	}
    
    /**
     * Delete individual time series in triple store and relational database (i.e. time series for one dataIRI)
     * @param dataIRI dataIRIs as Strings
	 * @param conn connection to the RDB
     */
    public void deleteIndividualTimeSeries(String dataIRI, Connection conn) {
    	
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
			deleteTimeSeries(tsIRI, conn);
    	} else {
	    	// Step1: Delete time series association in knowledge base
	    	// In case any exception occurs, nothing will be deleted in kb (no partial execution of SPARQL update - only one query)
	   		try {
	   			rdfClient.removeTimeSeriesAssociation(dataIRI);
	   		} catch (Exception eRdfDelete) {
				throw new JPSRuntimeException(exceptionPrefix + "Timeseries association for " + dataIRI + " was not deleted!", eRdfDelete);
	   		}
	    	
	    	// Step2: Try to delete corresponding time series column and central table entry in relational database
	    	try {
				rdbClient.deleteTimeSeries(dataIRI, conn);
	    	} catch (JPSRuntimeException eRdbDelete) {
				// For exceptions thrown when deleting RDB elements in relational database,
				// try to revert previous knowledge base deletion
	    		// TODO Ideally try to avoid throwing exceptions in a catch block - potential solution: have insertTimeSeriesAssociation throw
	    		//		a different exception depending on what the problem was, and how it should be handled
	    		try {
	    			rdfClient.insertTimeSeriesAssociation(dataIRI, tsIRI);
	    		} catch (Exception eRdfCreate) {
					throw new JPSRuntimeException(exceptionPrefix + "Inconsistent state created when deleting time series association for " + dataIRI +
							" , as database related deletion failed but KG triples were deleted.");
	    		}
				throw new JPSRuntimeException(exceptionPrefix + "Timeseries association for " + dataIRI + " was not deleted!", eRdbDelete);
	    	}
    	}
    }

    /**
     * Delete time series and all associated dataIRI connections from triple store and relational database 
     * @param tsIRI time series IRI as String
	 * @param conn connection to the RDB
     */
    public void deleteTimeSeries(String tsIRI, Connection conn) {
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
		} catch (Exception eRdfDelete) {
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries " + tsIRI + " was not deleted!", eRdfDelete);
		}

		// Obtain RDB URL from connection object, exception thrown when connection is down
		String rdbURL;
		try {
			rdbURL = conn.getMetaData().getURL();
		} catch (SQLException e) {
			// this ensures rdfClient.initTS has a valid string
			LOGGER.warn(e.getMessage());
			LOGGER.warn("Failed to get RDB URL from connection object, setting RDB URL to = \"\"");
			rdbURL = "";
		}

		// Step2: Try to delete corresponding entries in central table and the time series table in relational database
		try {
			// Retrieve example dataIRI needed to delete RDB related information
			rdbClient.deleteTimeSeriesTable(dataIRIs.get(0), conn);
		} catch (JPSRuntimeException eRdbDelete) {
			// For exceptions thrown when deleting RDB elements in relational database,
			// try to revert previous knowledge base deletion
			// TODO Ideally try to avoid throwing exceptions in a catch block - potential solution: have initTS throw
			//		a different exception depending on what the problem was, and how it should be handled
			try {
				rdfClient.initTS(tsIRI, dataIRIs, rdbURL, timeUnit);
			} catch (Exception eRdfCreate) {
				throw new JPSRuntimeException(exceptionPrefix + "Inconsistent state created when deleting time series " + tsIRI +
						" , as database related deletion failed but KG triples were deleted.", eRdfCreate);
			}
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries " + tsIRI + " was not deleted!", eRdbDelete);
		}
    }
    
    /**
     * Delete all time series and associated connections from triple store and relational database
	 * NOTE: When trying to delete all time series information, NO restore will be tried
	 *     	 in case any exception occurs - only errors for inconsistent states are thrown.
     */
    public void deleteAll(Connection conn) {
		// Step1: Delete all time series in knowledge base
		try {
			// Removing all triples is done by repetitive deletion of time series IRIs from KG
			// -> potentially not all time series could get deleted
			rdfClient.removeAllTimeSeries();
		} catch (Exception eRdfDelete) {
			throw new JPSRuntimeException(exceptionPrefix + "Not all timeseries were deleted from KG! " +
					  "Potentially inconsistent state between KG and database", eRdfDelete);
		}
		
		// Step2: Try to delete all time series tables and central lookup table in relational database
		try {
			rdbClient.deleteAll(conn);
		} catch (JPSRuntimeException eRdbDelete) {
			throw new JPSRuntimeException(exceptionPrefix + "Not all timeseries were deleted from database! " +
					  "Potentially inconsistent state between KG and database", eRdbDelete);
		}
    }
    
    public TimeSeries<T> getLatestData(String dataIRI, Connection conn) {
    	return rdbClient.getLatestData(dataIRI, conn);
    }
    
    public TimeSeries<T> getOldestData(String dataIRI, Connection conn) {
    	return rdbClient.getOldestData(dataIRI, conn);
    }
    
    /** 
     * Retrieve time series data within given bounds (time bounds are inclusive and optional)
     * <p>Returned time series are in ascending order with respect to time (from oldest to newest)
     * <br>Returned time series contain potential duplicates (i.e. multiple entries for same time stamp)
	 * @param dataIRIs list of data IRIs provided as string
	 * @param lowerBound start timestamp from which to retrieve data (null if not applicable)
	 * @param upperBound end timestamp until which to retrieve data (null if not applicable)
	 * @param conn connection to the RDB
	 * @return All data series from dataIRIs list as single TimeSeries object
	 */
	public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRIs, T lowerBound, T upperBound, Connection conn) {
    	// Retrieve time series data from respective database table
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
    	return rdbClient.getTimeSeriesWithinBounds(dataIRIs, lowerBound, upperBound, conn);
    }
	
    /** 
     * Retrieve entire time series data history for given dataIRIs
     * <p>Returned time series are in ascending order with respect to time (from oldest to newest)
     * <br>Returned time series contain potential duplicates (i.e. multiple entries for same time stamp)
	 * @param dataIRIs list of data IRIs provided as string
	 * @param conn connection to the RDB
	 * @return All data series from dataIRIs list as single TimeSeries object
	 */
	public TimeSeries<T> getTimeSeries(List<String> dataIRIs, Connection conn) {
    	return getTimeSeriesWithinBounds(dataIRIs, null, null, conn);
    }
	
	/**
	 * Retrieve average value of an entire time series
	 * @param dataIRI data IRI provided as string
	 * @param conn connection to the RDB
	 * @return The average of the corresponding data series as double
	 */
	public double getAverage(String dataIRI, Connection conn) {
		// Retrieve wanted time series aggregate from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getAverage(dataIRI, conn);
	}
	
	/**
	 * Retrieve maximum value of an entire time series
	 * @param dataIRI data IRI provided as string
	 * @param conn connectio to the RDB
	 * @return The average of the corresponding data series as double
	 */
	public double getMaxValue(String dataIRI, Connection conn) {
		// Retrieve wanted time series aggregate from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getMaxValue(dataIRI, conn);
	}
	
	/**
	 * Retrieve minimum value of an entire time series
	 * @param dataIRI data IRI provided as string
	 * @param conn connection to the RDB
	 * @return The average of the corresponding data series as double
	 */
	public double getMinValue(String dataIRI, Connection conn) {
		// Retrieve wanted time series aggregate from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getMinValue(dataIRI, conn);
	}
	
	/**
	 * Retrieve latest (maximum) time entry for a given dataIRI
	 * @param dataIRI data IRI provided as string
	 * @param conn connection to the RDB
	 * @return The maximum (latest) timestamp of the corresponding data series
	 */
	public T getMaxTime(String dataIRI, Connection conn) {
		// Retrieve latest time entry from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getMaxTime(dataIRI, conn);
	}
	
	/**
	 * Retrieve earliest (minimum) time entry for a given dataIRI
	 * @param dataIRI data IRI provided as string
	 * @param conn connection to the RDB
	 * @return The minimum (earliest) timestamp of the corresponding data series
	 */
	public T getMinTime(String dataIRI, Connection conn) {
		// Retrieve earliest time entry from database
    	// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		return rdbClient.getMinTime(dataIRI, conn);
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
	 * @param conn connection to the RDB
	 * @return True if dataIRI exists and is attached to a time series, false otherwise
	 */
    public boolean checkDataHasTimeSeries(String dataIRI, Connection conn) {
    	return rdbClient.checkDataHasTimeSeries(dataIRI, conn);
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
	 * converts list of time series into required format for visualisation
	 * please do not modify without consulting the visualisation team at CMCL
	 * @param tsList
	 * @param id
	 * @param unitsMap
	 * @param tableHeaderMap
	 * @return
	 */
	public JSONArray convertToJSON(List<TimeSeries<T>> tsList, List<Integer> id,
			List<Map<String,String>> unitsMap, List<Map<String, String>> tableHeaderMap) {
		JSONArray tsArray = new JSONArray();
		
		for (int i = 0; i < tsList.size(); i++) {
			TimeSeries<T> ts = tsList.get(i);
			
			JSONObject tsJo = new JSONObject();
			
			// to link this time series to a station
			// in this application there is only 1 data per ts
			List<String> dataIRIs = ts.getDataIRIs();
			tsJo.put("id", id.get(i));
			
			// classes
			if (!ts.getTimes().isEmpty()) {
				if (ts.getTimes().get(0) instanceof Number) {
					tsJo.put("timeClass", Number.class.getSimpleName());
				} else {
					tsJo.put("timeClass", ts.getTimes().get(0).getClass().getSimpleName());
				}
			}
			
			// for table headers
			if (tableHeaderMap != null) {
				List<String> tableHeader = new ArrayList<>();
				for (String dataIRI : dataIRIs) {
					tableHeader.add(tableHeaderMap.get(i).get(dataIRI));
				}
				tsJo.put("data", tableHeader);
			} else {
				tsJo.put("data", dataIRIs);
			}
	    	
			List<String> units = new ArrayList<>();
			for (String dataIRI : dataIRIs) {
				units.add(unitsMap.get(i).get(dataIRI));
			}
	    	tsJo.put("units", units);
	    	
	    	// time column
	    	tsJo.put("time", ts.getTimes());
	    	
	    	// values columns
	    	// values columns, one array for each data
	    	JSONArray values = new JSONArray();
	    	JSONArray valuesClass = new JSONArray();
	    	for (int j = 0; j < dataIRIs.size(); j++) {
				List<?> valueslist = ts.getValues(dataIRIs.get(j));
				values.put(valueslist);
				// Initialise value class (in case no class can be determined due to missing data)
				String vClass = "Unknown";
				for (Object value: valueslist) {
					// Get values class from first not null value
					if (value != null) {
						if (value instanceof Number) {
							vClass = Number.class.getSimpleName();
						} else {
							vClass = value.getClass().getSimpleName();
						}
						break;
					}
				}
				valuesClass.put(vClass);
			}
	    	
	    	tsJo.put("values", values);
	    	tsJo.put("valuesClass", valuesClass);
			
			tsArray.put(tsJo);
		}
		
		return tsArray;
	}

	/**
	 * Constructor with pre-defined kbClient and RDB client to be created with provided parameters
	 * @param kbClient knowledge base client used to query and update the knowledge base containing timeseries information (potentially with already specified endpoint (triplestore/owl file))
	 * @param timeClass class type for the time values, e.g. Timestamp etc. (to initialise RDB table)
	 * @param rdbURL URL to relational database (e.g. postgreSQL)
	 * @param user username to access relational database
	 * @param password password to access relational database
	 */
	public TimeSeriesClient(TripleStoreClientInterface kbClient, Class<T> timeClass, String rdbURL, String user, String password) {
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
	public TimeSeriesClient(TripleStoreClientInterface kbClient, Class<T> timeClass, String filepath) throws IOException {
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
	 * Load properties for RDF/SPARQL client
	 * @param filepath absolute path to properties file with respective information
	 */
	private void loadSparqlConfigs(String filepath) throws IOException {
		rdfClient.loadSparqlConfigs(filepath);
	}

	/**
	 * Load properties for RDB client
	 * @param filepath absolute path to properties file with respective information
	 */
	private void loadRdbConfigs(String filepath) throws IOException {
		rdbClient.loadRdbConfigs(filepath);
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
	 * @param type type of TimeSeries data to be instantiated. (optional)
	 *             Allowed values:
	 *             https://www.theworldavatar.com/kg/ontotimeseries/StepwiseCumulative,
	 *             https://www.theworldavatar.com/kg/ontotimeseries/CumulativeTotal,
	 *             https://www.theworldavatar.com/kg/ontotimeseries/Instantaneous,
	 *             https://www.theworldavatar.com/kg/ontotimeseries/Average
	 *             If not specified, default value: TimeSeries
	 * @param duration Required for Average Time Series. Numeric duration of the averaging period for Average TimeSeries of type Duration. Only positive values are allowed. (optional)
	 * @param unit Required for Average Time Series. Temporal unit type of the averaging period for Average TimeSeries. (optional)
	 *             Allowed values of type ChronoUnit:
	 *             ChronoUnit.SECONDS, ChronoUnit.MINUTES, ChronoUnit.HOURS, ChronoUit.DAYS, ChronoUnit.WEEKS, ChronoUnit.MONTHS, ChronoUnit.YEARS
	 *
     */
	public void initTimeSeries(List<String> dataIRIs, List<Class<?>> dataClass, String timeUnit, String type, Duration duration, ChronoUnit unit) {
		try (Connection conn = rdbClient.getConnection()) {
			initTimeSeries(dataIRIs, dataClass, timeUnit, conn, type, duration, unit);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * similar to initTimeSeries, but uploads triples in one connection
	 * @param dataIRIs
	 * @param dataClass
	 * @param timeUnit
	 * @param type
	 * @param durations
	 * @param units
	 */
	public void bulkInitTimeSeries(List<List<String>> dataIRIs, List<List<Class<?>>> dataClass, List<String> timeUnit, List<String> type, List<Duration> durations, List<ChronoUnit> units) {
		try (Connection conn = rdbClient.getConnection()) {
			bulkInitTimeSeries(dataIRIs, dataClass, timeUnit, (Integer) null, conn, type, durations, units);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * similar to initTimeSeries, but uploads triples in one connection#
	 * Provide SRID if time series data contains geometries
	 * @param dataIRIs
	 * @param dataClass
	 * @param timeUnit
	 * @param srid
	 * @param type
	 * @param durations
	 * @param units
	 */
	public void bulkInitTimeSeries(List<List<String>> dataIRIs, List<List<Class<?>>> dataClass, List<String> timeUnit, Integer srid, List<String> type, List<Duration> durations, List<ChronoUnit> units) {
		try (Connection conn = rdbClient.getConnection()) {
			bulkInitTimeSeries(dataIRIs, dataClass, timeUnit, srid, conn, type, durations, units);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
     * Append time series data to an already instantiated time series
	 * @param ts TimeSeries object to add
     */
	public void addTimeSeriesData(TimeSeries<T> ts) {
		try (Connection conn = rdbClient.getConnection()) {
			addTimeSeriesData(ts, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
     * Append time series data to an already instantiated time series
	 * (i.e. add data for several time series in a single RDB connection)
	 * @param tsList List of TimeSeries objects to add
     */
	public void bulkaddTimeSeriesData(List<TimeSeries<T>> tsList) {
		// Add time series data to respective database tables
		// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		try (Connection conn = rdbClient.getConnection()) {
			rdbClient.addTimeSeriesData(tsList, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
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
		try (Connection conn = rdbClient.getConnection()) {
			rdbClient.deleteRows(dataIRI, lowerBound, upperBound, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
     * Delete individual time series in triple store and relational database (i.e. time series for one dataIRI)
     * @param dataIRI dataIRIs as Strings
     */
	public void deleteIndividualTimeSeries(String dataIRI) {
		try (Connection conn = rdbClient.getConnection()) {
			deleteIndividualTimeSeries(dataIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
     * Delete time series and all associated dataIRI connections from triple store and relational database
     * @param tsIRI time series IRI as String
     */
	public void deleteTimeSeries(String tsIRI) {
		try (Connection conn = rdbClient.getConnection()) {
			deleteTimeSeries(tsIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
     * Delete all time series and associated connections from triple store and relational database
	 * NOTE: When trying to delete all time series information, NO restore will be tried
	 *     	 in case any exception occurs - only errors for inconsistent states are thrown.
     */
	public void deleteAll() {
		try (Connection conn = rdbClient.getConnection()) {
			deleteAll(conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	public TimeSeries<T> getLatestData(String dataIRI) {
		try (Connection conn = rdbClient.getConnection()) {
			return rdbClient.getLatestData(dataIRI,conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	public TimeSeries<T> getOldestData(String dataIRI) {
		try (Connection conn = rdbClient.getConnection()) {
			return rdbClient.getOldestData(dataIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
     * Retrieve entire time series data history for given dataIRIs
     * <p>Returned time series are in ascending order with respect to time (from oldest to newest)
     * <br>Returned time series contain potential duplicates (i.e. multiple entries for same time stamp)
	 * @param dataIRIs list of data IRIs provided as string
	 * @return All data series from dataIRIs list as single TimeSeries object
	 */
	public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRIs, T lowerBound, T upperBound) {
		// Retrieve time series data from respective database table
		// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		try (Connection conn = rdbClient.getConnection()) {
			return rdbClient.getTimeSeriesWithinBounds(dataIRIs, lowerBound, upperBound, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
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
		try (Connection conn = rdbClient.getConnection()) {
			return rdbClient.getAverage(dataIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * Retrieve maximum value of an entire time series
	 * @param dataIRI data IRI provided as string
	 * @return The average of the corresponding data series as double
	 */
	public double getMaxValue(String dataIRI) {
		// Retrieve wanted time series aggregate from database
		// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		try (Connection conn = rdbClient.getConnection()) {
			return rdbClient.getMaxValue(dataIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * Retrieve minimum value of an entire time series
	 * @param dataIRI data IRI provided as string
	 * @return The average of the corresponding data series as double
	 */
	public double getMinValue(String dataIRI) {
		// Retrieve wanted time series aggregate from database
		// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		try (Connection conn = rdbClient.getConnection()) {
			return rdbClient.getMinValue(dataIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * Retrieve latest (maximum) time entry for a given dataIRI
	 * @param dataIRI data IRI provided as string
	 * @return The maximum (latest) timestamp of the corresponding data series
	 */
	public T getMaxTime(String dataIRI) {
		// Retrieve latest time entry from database
		// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		try (Connection conn = rdbClient.getConnection()) {
			return rdbClient.getMaxTime(dataIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * Retrieve earliest (minimum) time entry for a given dataIRI
	 * @param dataIRI data IRI provided as string
	 * @return The minimum (earliest) timestamp of the corresponding data series
	 */
	public T getMinTime(String dataIRI) {
		// Retrieve earliest time entry from database
		// Checks whether all dataIRIs are instantiated as time series are conducted within rdb client (due to performance reasons)
		try (Connection conn = rdbClient.getConnection()) {
			return rdbClient.getMinTime(dataIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * Check whether given data IRI is attached to a time series in kb
	 * @param dataIRI data IRI provided as string
	 * @return True if dataIRI exists and is attached to a time series, false otherwise
	 */
	public boolean checkDataHasTimeSeries(String dataIRI) {
		try (Connection conn = rdbClient.getConnection()) {
			return rdbClient.checkDataHasTimeSeries(dataIRI, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}


	/**
	 * Initialise time series in triple store and relational database
	 * @param dataIRIs list of dataIRIs as Strings
	 * @param dataClass list of data classes for each dataIRI
	 * @param timeUnit time unit as (full) IRI
	 */
	@Deprecated
	public void initTimeSeries(List<String> dataIRIs, List<Class<?>> dataClass, String timeUnit) {
		try (Connection conn = rdbClient.getConnection()) {
			initTimeSeries(dataIRIs, dataClass, timeUnit, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * Initialise time series in triple store and relational database
	 * @param dataIRIs list of dataIRIs as Strings
	 * @param dataClass list of data classes for each dataIRI
	 * @param timeUnit time unit as (full) IRI
	 * @param conn connection to the RDB
	 */
	@Deprecated
	public void initTimeSeries(List<String> dataIRIs, List<Class<?>> dataClass, String timeUnit, Connection conn) {

		// Create random time series IRI in the format: <Namespace><ClassName>_<UUID>
		String tsIRI = TimeSeriesSparql.ns_kb + "Timeseries_" + UUID.randomUUID();

		// Step1: Initialise time series in knowledge base
		// In case any exception occurs, nothing will be created in kb, since JPSRuntimeException will be thrown before
		// interacting with triple store and SPARQL query is either executed fully or not at all (no partial execution possible)

		// Obtain RDB URL from connection object, exception thrown when connection is down
		String rdbURL;
		try {
			rdbURL = conn.getMetaData().getURL();
		} catch (SQLException e) {
			// this ensures rdfClient.initTS always has a valid string
			LOGGER.warn(e.getMessage());
			LOGGER.warn("Failed to get RDB URL from connection object, setting RDB URL to = \"\"");
			rdbURL = "";
		}

		try {
			rdfClient.initTS(tsIRI, dataIRIs, rdbURL, timeUnit);
		}
		catch (Exception eRdfCreate) {
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", eRdfCreate);
		}

		// Step2: Try to initialise time series in relational database
		try {
			rdbClient.initTimeSeriesTable(dataIRIs, dataClass, tsIRI, conn);
		} catch (JPSRuntimeException eRdbCreate) {
			// For exceptions thrown when initialising RDB elements in relational database,
			// try to revert previous knowledge base instantiation
			// TODO Ideally try to avoid throwing exceptions in a catch block - potential solution: have removeTimeSeries throw
			//		a different exception depending on what the problem was, and how it should be handled
			try {
				rdfClient.removeTimeSeries(tsIRI);
			} catch (Exception eRdfDelete) {
				throw new JPSRuntimeException(exceptionPrefix + "Inconsistent state created when initialising time series " + tsIRI +
						" , as database related instantiation failed but KG triples were created.", eRdfDelete);
			}
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", eRdbCreate);
		}
	}

	/**
	 * similar to initTimeSeries, but uploads triples in one connection
	 * @param dataIRIs
	 * @param dataClass
	 * @param timeUnit
	 * @param conn
	 */
	@Deprecated
	public void bulkInitTimeSeries(List<List<String>> dataIRIs, List<List<Class<?>>> dataClass, List<String> timeUnit, Connection conn) {
		bulkInitTimeSeries(dataIRIs, dataClass, timeUnit, null, conn);
	}

	/**
	 * similar to initTimeSeries, but uploads triples in one connection
	 * srid is used if the time series values contain geometries
	 * @param dataIRIs
	 * @param dataClass
	 * @param timeUnit
	 * @param srid
	 * @param conn
	 */
	@Deprecated
	public void bulkInitTimeSeries(List<List<String>> dataIRIs, List<List<Class<?>>> dataClass, List<String> timeUnit, Integer srid, Connection conn) {
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
			rdfClient.bulkInitTS(tsIRIs, dataIRIs, conn.getMetaData().getURL(), timeUnit);
		}
		catch (Exception e_RdfCreate) {
			throw new JPSRuntimeException(exceptionPrefix + "Timeseries was not created!", e_RdfCreate);
		}

		// Step2: Try to initialise time series in relational database
		for (int i = 0; i < dataIRIs.size(); i++) {
			try {
				rdbClient.initTimeSeriesTable(dataIRIs.get(i), dataClass.get(i), tsIRIs.get(i), srid, conn);
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
	 * similar to initTimeSeries, but uploads triples in one connection
	 * @param dataIRIs
	 * @param dataClass
	 * @param timeUnit
	 */
	@Deprecated
	public void bulkInitTimeSeries(List<List<String>> dataIRIs, List<List<Class<?>>> dataClass, List<String> timeUnit) {
		try (Connection conn = rdbClient.getConnection()) {
			bulkInitTimeSeries(dataIRIs, dataClass, timeUnit, (Integer) null, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}

	/**
	 * similar to initTimeSeries, but uploads triples in one connection#
	 * Provide SRID if time series data contains geometries
	 * @param dataIRIs
	 * @param dataClass
	 * @param timeUnit
	 * @param srid
	 */
	@Deprecated
	public void bulkInitTimeSeries(List<List<String>> dataIRIs, List<List<Class<?>>> dataClass, List<String> timeUnit, Integer srid) {
		try (Connection conn = rdbClient.getConnection()) {
			bulkInitTimeSeries(dataIRIs, dataClass, timeUnit, srid, conn);
		} catch (SQLException e) {
			throw new JPSRuntimeException(exceptionPrefix + CONNECTION_ERROR, e);
		}
	}
}
