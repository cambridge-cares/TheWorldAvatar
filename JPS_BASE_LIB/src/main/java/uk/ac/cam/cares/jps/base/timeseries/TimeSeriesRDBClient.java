package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.Properties;
import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.InsertValuesStep4;
import org.jooq.InsertValuesStepN;
import org.jooq.Record;
import org.jooq.Result;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.UpdateSetFirstStep;
import org.jooq.exception.DataAccessException;
import org.jooq.impl.DSL;
import org.jooq.impl.DefaultDataType;
import org.postgis.Geometry;

import static org.jooq.impl.DSL.*;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * This class uses the jooq library to interact with a relational database.
 * <T> is the class type for the time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
 * @author Kok Foong Lee
 */

public class TimeSeriesRDBClient<T> {
	/**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(TimeSeriesRDBClient.class);
	// URL and credentials for the relational database
	private String rdbURL = null; 
	private String rdbUser = null;
	private String rdbPassword = null;
	// RDB connection properties and jooq configuration 
	private Connection conn = null;
	private DSLContext context;
	// Time series column field (for RDB)
	private final Field<T> timeColumn;
	// Constants
	private static final SQLDialect dialect = SQLDialect.POSTGRES;    
    // Central database table
    private static final String dbTableName = "dbTable";
    private static final Field<String> dataIRIcolumn = DSL.field(DSL.name("dataIRI"), String.class);
    private static final Field<String> tsIRIcolumn = DSL.field(DSL.name("timeseriesIRI"), String.class);
    private static final Field<String> tsTableNameColumn = DSL.field(DSL.name("tableName"), String.class);
    private static final Field<String> columnNameColumn = DSL.field(DSL.name("columnName"), String.class);
    // Exception prefix
 	private final String exceptionPrefix = this.getClass().getSimpleName() + ": ";
 	// Allowed aggregation function
	protected enum AggregateFunction {
		AVERAGE,
		MAX,
		MIN
	}

    /**
     * Standard constructor
     * @param timeClass class of the timestamps of the time series
     */
    public TimeSeriesRDBClient(Class<T> timeClass) {
    	timeColumn = DSL.field(DSL.name("time"), timeClass);
    }
    
    /**
     * Get and set methods for private relational database properties (e.g. PostgreSQL)
     */
	public void setRdbURL(String rdbURL) {
		this.rdbURL = rdbURL;
	}
	public String getRdbURL() {
		return rdbURL;
	}
	public void setRdbUser(String user) {
		this.rdbUser = user;
	}
	public String getRdbUser() {
		return rdbUser;
	}
	public void setRdbPassword(String password) {
		this.rdbPassword = password;
	}
	
	/**
	 * Load RDB URL, username and password from properties file ("timeseries.properties") at specified path
	 * @param filepath absolute path to timeseries properties file
	 */
	protected void loadRdbConfigs(String filepath) throws IOException {
		
		// Check whether properties file exists at specified location
		File file = new File(filepath);		
		if (!file.exists()) {
			throw new JPSRuntimeException(exceptionPrefix + "No properties file found at specified filepath: " + filepath);
		}
		
		// Try-with-resource to ensure closure of input stream
		try (InputStream input = new FileInputStream(file)) {
		
			// Load properties file from specified path
	        Properties prop = new Properties();
	        prop.load(input);
	
	        // Get the property values and assign
	        if (prop.containsKey("db.url")) {
	        	setRdbURL(prop.getProperty("db.url"));
	        } else {
	        	throw new JPSRuntimeException(exceptionPrefix + "Properties file is missing \"db.url=<rdb_url>\" ");
	        }
	        if (prop.containsKey("db.user")) {
	        	setRdbUser(prop.getProperty("db.user"));
	        } else {
	        	throw new JPSRuntimeException(exceptionPrefix + "Properties file is missing \"db.user=<rdb_username>\" ");
	        }
	        if (prop.containsKey("db.password")) {
	        	setRdbPassword(prop.getProperty("db.password"));
	        } else {
	        	throw new JPSRuntimeException(exceptionPrefix + "Properties file is missing \"db.password=<rdb_password>\" ");
	        }
		}

	}

	/**
	 * Initialise RDB table for particular time series and add respective entries to central lookup table
	 * <p>For the list of supported classes, refer org.jooq.impl.SQLDataType
	 * <p>The timeseries IRI needs to be provided. A unique uuid for the corresponding table will be generated.
	 * @param dataIRI list of IRIs for the data provided as string
	 * @param dataClass list with the corresponding Java class (typical String, double or int) for each data IRI
	 * @param tsIRI IRI of the timeseries provided as string
	 */
	protected String initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI) {
		return initTimeSeriesTable(dataIRI, dataClass, tsIRI, null);
	}

	/**
	 * similar to above, but specifically to use with PostGIS
	 * the additional argument srid is to restrict any geometry columns to the srid
	 * if not needed, set to null, or use above the above method
	 * @param dataIRI
	 * @param dataClass
	 * @param tsIRI
	 * @param srid
	 * @return
	 */
	protected String initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI, Integer srid) {

		// Generate UUID as unique RDB table name
		String tsTableName = UUID.randomUUID().toString();		
		
		// Initialise connection and set jOOQ DSL context
		connect();
		
		// All database interactions in try-block to ensure closure of connection
		try {
			
			// Check if central database lookup table exists and create if not
			String condition = String.format("table_name = '%s'", dbTableName);
			if (context.select(count()).from("information_schema.tables").where(condition).fetchOne(0, int.class) == 0) {
				initCentralTable();
			}
			
			// Check if any data has already been initialised (i.e. is associated with different tsIRI)
			for (String s : dataIRI) {
				if(checkDataHasTimeSeries(s)) {
					throw new JPSRuntimeException(exceptionPrefix + "<" + s + "> already has an assigned time series instance");
				}
			}
	
			// Ensure that there is a class for each data IRI
			if (dataIRI.size() != dataClass.size()) {
				throw new JPSRuntimeException(exceptionPrefix + "Length of dataClass is different from number of data IRIs");
			}
	
			// Assign column name for each dataIRI; name for time column is fixed
			Map<String,String> dataColumnNames = new HashMap<>();
			int i = 1;
			for (String s : dataIRI) {
				dataColumnNames.put(s, "column"+i);
				i++;
			}			
					
			// Add corresponding entries in central lookup table
			populateCentralTable(tsTableName, dataIRI, dataColumnNames, tsIRI);
			
			// Initialise RDB table for storing time series data
			createEmptyTimeSeriesTable(tsTableName, dataColumnNames, dataIRI, dataClass, srid);
			
			return tsTableName;
		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
		
	}
	
    /**
     * Append time series data to an already existing RDB table
	 * If certain columns within the table are not provided, they will be nulls
	 * if a row with the equivalent time value exists, the values provided will overwrite
	 * the existing data in the table
	 * @param ts_list TimeSeries object to add
     */
	protected void addTimeSeriesData(List<TimeSeries<T>> ts_list) {
		
    	// Initialise connection and set jOOQ DSL context
    	connect();

		// Loop over all time series in list
		for (TimeSeries<T> ts : ts_list) {
		
			List<String> dataIRI = ts.getDataIRIs();
			
			// All database interactions in try-block to ensure closure of connection
			try {
				
				// Check if central database lookup table exists
				String condition = String.format("table_name = '%s'", dbTableName);
				if (context.select(count()).from("information_schema.tables").where(condition).fetchOne(0, int.class) == 0) {
					throw new JPSRuntimeException(exceptionPrefix + "Central RDB lookup table has not been initialised yet");
				}
				
				// Ensure that all provided dataIRIs/columns are located in the same RDB table (throws Exception if not)
				checkDataIsInSameTable(dataIRI);
				
				String tsTableName = getTimeseriesTableName(dataIRI.get(0));
				// Assign column name for each dataIRI; name for time column is fixed
				Map<String,String> dataColumnNames = new HashMap<>();
				for (String s : dataIRI) {
					dataColumnNames.put(s, getColumnName(s));
				}
				
				// Append time series data to time series table
				// if a row with the time value exists, that row will be updated instead of creating a new row
				populateTimeSeriesTable(tsTableName, ts, dataColumnNames);
			} catch (JPSRuntimeException e) {
				// Re-throw JPSRuntimeExceptions
				throw e;
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
				// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
				throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
			}
			
		}
	}
	
    /** 
     * Retrieve time series within bounds from RDB (time bounds are inclusive and optional)
     * <p>Returns all data series from dataIRI list as one time series object (with potentially multiple related data series);
     * <br>Returned time series are in ascending order with respect to time (from oldest to newest)
     * <br>Returned time series contain potential duplicates (i.e. multiple entries for same time stamp)
	 * @param dataIRI list of data IRIs provided as string
	 * @param lowerBound start timestamp from which to retrieve data (null if not applicable)
	 * @param upperBound end timestamp until which to retrieve data (null if not applicable)
	 */
	public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRI, T lowerBound, T upperBound) {

		// Initialise connection and set jOOQ DSL context
		connect();
		
		// All database interactions in try-block to ensure closure of connection
		try {
			
			// Check if central database lookup table exists
			String condition = String.format("table_name = '%s'", dbTableName);
			if (context.select(count()).from("information_schema.tables").where(condition).fetchOne(0, int.class) == 0) {
				throw new JPSRuntimeException(exceptionPrefix + "Central RDB lookup table has not been initialised yet");
			}
			
			// Ensure that all provided dataIRIs/columns are located in the same RDB table (throws Exception if not)
			checkDataIsInSameTable(dataIRI);

			// Retrieve table corresponding to the time series connected to the data IRIs
	    	Table<?> table = getTimeseriesTable(dataIRI.get(0));
	    	
	    	// Create map between data IRI and the corresponding column field in the table
	    	Map<String, Field<Object>> dataColumnFields = new HashMap<>();
			for (String data : dataIRI) {
				String columnName = getColumnName(data);
				Field<Object> field = DSL.field(DSL.name(columnName));
				dataColumnFields.put(data, field);
			}
			
			// Retrieve list of column fields (including fixed time column)
	    	List<Field<?>> columnList = new ArrayList<>();
	    	columnList.add(timeColumn);
	    	for (String data : dataIRI) {
	    	    columnList.add(dataColumnFields.get(data));
	    	}
	    
			// Potentially update bounds (if no bounds were provided)
			if (lowerBound == null) {
				lowerBound = context.select(min(timeColumn)).from(table).fetch(min(timeColumn)).get(0);
			} 
			if (upperBound == null) {
				upperBound = context.select(max(timeColumn)).from(table).fetch(max(timeColumn)).get(0);
			}
	    	
	    	// Perform query
	    	Result<? extends Record> queryResult = context.select(columnList).from(table).where(timeColumn.between(lowerBound, upperBound))
	    			 									  .orderBy(timeColumn.asc()).fetch();
	    	
	    	// Collect results and return a TimeSeries object
	    	List<T> timeValues = queryResult.getValues(timeColumn);
	    	List<List<?>> dataValues = new ArrayList<>();
	    	for (String data : dataIRI) {
	    		List<?> column = queryResult.getValues(dataColumnFields.get(data));
	    		dataValues.add(column);
	    	} 

	    	return new TimeSeries<>(timeValues, dataIRI, dataValues);
	    	
		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
		
	}
	
	/** 
     * Retrieve entire time series from RDB
	 * @param dataIRI list of data IRIs provided as string
	 */
	public TimeSeries<T> getTimeSeries(List<String> dataIRI) {
		return getTimeSeriesWithinBounds(dataIRI, null, null);
	}
	
	/**
	 * returns a TimeSeries object with the latest value of the given IRI
	 * @param dataIRI
	 */
	public TimeSeries<T> getLatestData(String dataIRI) {
		connect();
		
		try {
			Table<?> tsTable = getTimeseriesTable(dataIRI);
			String columnName = getColumnName(dataIRI);
			
			Field<Object> dataField = DSL.field(DSL.name(columnName));

			Result<? extends Record> queryResult = context.select(timeColumn, dataField).from(tsTable).where(dataField.isNotNull())
			.orderBy(timeColumn.desc()).limit(1).fetch();
			
			List<T> timeValues = queryResult.getValues(timeColumn);
			List<?> dataValues = queryResult.getValues(dataField);
			
			return new TimeSeries<T>(timeValues, Arrays.asList(dataIRI), Arrays.asList(dataValues));
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
	}
	
	/**
	 * returns a TimeSeries object with the oldest value of the given IRI
	 * @param dataIRI
	 */
	public TimeSeries<T> getOldestData(String dataIRI) {
		connect();
		
		try {
			Table<?> tsTable = getTimeseriesTable(dataIRI);
			String columnName = getColumnName(dataIRI);
			
			Field<Object> dataField = DSL.field(DSL.name(columnName));

			Result<? extends Record> queryResult = context.select(timeColumn, dataField).from(tsTable).where(dataField.isNotNull())
			.orderBy(timeColumn.asc()).limit(1).fetch();
			
			List<T> timeValues = queryResult.getValues(timeColumn);
			List<?> dataValues = queryResult.getValues(dataField);
			
			return new TimeSeries<T>(timeValues, Arrays.asList(dataIRI), Arrays.asList(dataValues));
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
	}
	
	/**
	 * Retrieve average value of a column; stored data should be in numerics
	 * @param dataIRI data IRI provided as string
	 * @return The average of the provided data series as double
	 */
	public double getAverage(String dataIRI) {
		return getAggregate(dataIRI, AggregateFunction.AVERAGE);
	}
	
	/**
	 * Retrieve maximum value of a column; stored data should be in numerics
	 * @param dataIRI data IRI provided as string
	 * @return The maximum of the provided data series as double
	 */
	public double getMaxValue(String dataIRI) {
		return getAggregate(dataIRI, AggregateFunction.MAX);
	}
	
	/**
	 * Retrieve minimum value of a column; stored data should be in numerics
	 * @param dataIRI data IRI provided as string
	 * @return The minimum of the provided data series as double
	 */
	public double getMinValue(String dataIRI) {
		return getAggregate(dataIRI, AggregateFunction.MIN);
	}
	
	/**
	 * Retrieve latest (maximum) time entry for a given dataIRI
	 * @param dataIRI data IRI provided as string
	 * @return The maximum (latest) timestamp of the provided data series
	 */
	public T getMaxTime(String dataIRI) {
		
		// Initialise connection and set jOOQ DSL context
		connect();
		
		// All database interactions in try-block to ensure closure of connection
		try {
			
			// Retrieve table corresponding to the time series connected to the data IRI
	    	Table<?> table = getTimeseriesTable(dataIRI);
	    	
	    	List<T> queryResult = context.select(max(timeColumn)).from(table).fetch(max(timeColumn));
	    	
	    	return queryResult.get(0);
	    	
		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
		
	}
	
	/**
	 * Retrieve earliest (minimum) time entry for a given dataIRI
	 * @param dataIRI data IRI provided as string
	 * @return The minimum (earliest) timestamp of the provided data series
	 */
	public T getMinTime(String dataIRI) {
		
		// Initialise connection and set jOOQ DSL context
		connect();
		
		// All database interactions in try-block to ensure closure of connection
		try {
			// Retrieve table corresponding to the time series connected to the data IRI
	    	Table<?> table = getTimeseriesTable(dataIRI);
	    	
	    	List<T> queryResult = context.select(min(timeColumn)).from(table).fetch(min(timeColumn));
	    	
	    	return queryResult.get(0);
	    	
		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
		
	}
	
	/**
	 * Delete RDB time series table rows between lower and upper Bound
	 * <p>Note that this will delete the entire rows in the corresponding table, i.e. all columns (in addition to the given data IRI)
	 * @param dataIRI data IRI provided as string
	 * @param lowerBound start timestamp from which to delete data
	 * @param upperBound end timestamp until which to delete data
	 */
	protected void deleteRows(String dataIRI, T lowerBound, T upperBound) {
		
		// Initialise connection and set jOOQ DSL context
		connect();
		
		// All database interactions in try-block to ensure closure of connection
		try {
			// Retrieve RDB table for dataIRI
	    	Table<?> table = getTimeseriesTable(dataIRI);
	    	
	    	// Delete rows between bounds (including bounds!)
	    	context.delete(table).where(timeColumn.between(lowerBound, upperBound)).execute();

		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
		
	}
	
	/**
	 * Delete individual time series (i.e. data for one dataIRI only)
	 * @param dataIRI data IRI provided as string
	 */
	protected void deleteTimeSeries(String dataIRI) {
		
		// Initialise connection and set jOOQ DSL context
		connect();
		
		// All database interactions in try-block to ensure closure of connection
		try {
			
			// Get time series RDB table		
			String columnName = getColumnName(dataIRI);
			String tsTableName = getTimeseriesTableName(dataIRI);
			
			// Retrieve number of columns of time series table (i.e. number of dataIRI + time column)
			String condition = String.format("table_name = '%s'", tsTableName);
			if (context.select(count()).from("information_schema.columns").where(condition).fetchOne(0, int.class) > 2) {
				
				// Delete only column for dataIRI from RDB table if further columns are present
				context.alterTable(tsTableName).drop(columnName).execute();
		    	
		    	// Delete entry in central lookup table
		    	Table<?> dbTable = DSL.table(DSL.name(dbTableName));
		    	context.delete(dbTable).where(dataIRIcolumn.equal(dataIRI)).execute();
			
			} else {
				// Delete entire RDB table for single column time series (data column + time column)
				deleteTimeSeriesTable(dataIRI);
			}
			
		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
		
	}
	
	/**
	 * Delete all time series information related to a dataIRI (i.e. entire RDB table and entries in central table)
	 * @param dataIRI data IRI provided as string
	 */
	protected void deleteTimeSeriesTable(String dataIRI) {
		
		// Initialise connection and set jOOQ DSL context
		connect();
		
		// All database interactions in try-block to ensure closure of connection
		try {

			// Retrieve RDB table for dataIRI
			String tsIRI = getTimeSeriesIRI(dataIRI);
			String tsTableName = getTimeseriesTableName(dataIRI);
	    
	    	// Delete time series RDB table
			context.dropTable(DSL.table(DSL.name(tsTableName))).execute();
	    	
	    	// Delete entries in central lookup table
	    	Table<?> dbTable = DSL.table(DSL.name(dbTableName));
	    	context.delete(dbTable).where(tsIRIcolumn.equal(tsIRI)).execute();
    	
		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
		
	}
	
	/**
	 * Delete all time series RDB tables and central lookup table
	 */
	protected void deleteAll() {

		// Initialise connection and set jOOQ DSL context
		connect();
		
		// All database interactions in try-block to ensure closure of connection
		try {
	    	
			// Check if central database lookup table exists
			String condition = String.format("table_name = '%s'", dbTableName);
			if (context.select(count()).from("information_schema.tables").where(condition).fetchOne(0, int.class) == 1) {
	    	
		    	// Retrieve all time series table names from central lookup table
		    	Table<?> dbTable = DSL.table(DSL.name(dbTableName));		
				List<String> queryResult = context.selectDistinct(tsTableNameColumn).from(dbTable).fetch(tsTableNameColumn);
				
				if (!queryResult.isEmpty()) {
					for (String table : queryResult) {						
						// Delete time series RDB table
						context.dropTable(DSL.table(DSL.name(table))).execute();		
					}					
					// Delete central lookup table
					context.dropTable(dbTable).execute();	
				}
			}
			
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
			throw new JPSRuntimeException("Error while executing SQL command", e);
		}
		
	}
	
	/**
	 * Establish connection to RDB and set DSL context
	 */
	protected void connect() {
		try {
			if (this.conn == null || this.conn.isClosed()) {
				// Load required driver
				Class.forName("org.postgresql.Driver");
				// Connect to DB (using static connection and context properties)
	        	this.conn = DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
	        	this.context = DSL.using(this.conn, dialect); 
	        	System.out.println("Connecting successful: " + this.rdbURL); 
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			System.out.println("Connecting failed: " + this.rdbURL);
			throw new JPSRuntimeException(exceptionPrefix + "Establishing database connection failed");
		}
    }
	
	/**
	 * Close existing connection to RDB
	 */
	void disconnect() {
		try {
			conn.close();
			System.out.println("Disconnecting successful"); 
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			System.out.println("Disconnecting failed");
			throw new JPSRuntimeException(exceptionPrefix + "Closing database connection failed");
		}
	}
	
	/**
	 * Initialise central database lookup table
	 * <p>Requires existing RDB connection
	 */
	private void initCentralTable() {
		// Initialise central lookup table: only creates empty table if it does not exist, otherwise it is left unchanged
		context.createTableIfNotExists(dbTableName).column(dataIRIcolumn).column(tsIRIcolumn)
			   .column(tsTableNameColumn).column(columnNameColumn).execute();
	}
	
	/**
	 * Add new entries to central RDB lookup table
	 * <p>Requires existing RDB connection
	 * @param tsTable name of the timeseries table provided as string
	 * @param dataIRI list of data IRIs provided as string
	 * @param dataColumnNames list of column names in the tsTable corresponding to the data IRIs
	 * @param tsIRI timeseries IRI provided as string
	 */
	private void populateCentralTable(String tsTable, List<String> dataIRI, Map<String, String> dataColumnNames, String tsIRI) {	
		InsertValuesStep4<Record, String, String, String, String> insertValueStep = context.insertInto(DSL.table(DSL.name(dbTableName)),
				dataIRIcolumn, tsIRIcolumn, tsTableNameColumn, columnNameColumn);
		
		// Populate columns row by row
		for (String s : dataIRI) {
			insertValueStep = insertValueStep.values(s, tsIRI, tsTable, dataColumnNames.get(s));
		}
		
		insertValueStep.execute();
	}
	
	/**
	 * Create an empty RDB table with the given data types for the respective columns
	 * <p>Requires existing RDB connection
	 * @param tsTable name of the timeseries table provided as string
	 * @param dataColumnNames list of column names in the tsTable corresponding to the data IRIs
	 * @param dataIRI list of data IRIs provided as string
	 * @param dataClass list with the corresponding Java class (typical String, double or int) for each data IRI
	 * @throws SQLException
	 */
	private void createEmptyTimeSeriesTable(String tsTable, Map<String,String> dataColumnNames, List<String> dataIRI,
											List<Class<?>> dataClass, Integer srid) throws SQLException {
		
		// Create table
		CreateTableColumnStep createStep = context.createTableIfNotExists(tsTable);
		
    	// Create time column
    	createStep = createStep.column(timeColumn);
    	
		List<String> additionalGeomColumns = new ArrayList<>();
		List<Class<?>> classForAdditionalGeomColumns = new ArrayList<>();

    	// Create 1 column for each value
    	for (int i = 0; i < dataIRI.size(); i++) {
			if (Geometry.class.isAssignableFrom(dataClass.get(i))) {
				// these columns will be added with their respective restrictions
				additionalGeomColumns.add(dataColumnNames.get(dataIRI.get(i)));
				classForAdditionalGeomColumns.add(dataClass.get(i));
			} else {
				createStep = createStep.column(dataColumnNames.get(dataIRI.get(i)), DefaultDataType.getDataType(dialect, dataClass.get(i)));
			}
    	}

    	// Send consolidated request to RDB
    	createStep.execute();

		// add remaining geometry columns with restrictions
		if (additionalGeomColumns.size() > 0) {
			addGeometryColumns(tsTable, additionalGeomColumns, classForAdditionalGeomColumns, srid);
		}	
	}
	
	/**
	 * workaround because open source jOOQ DataType class does not support geometry datatypes properly
	 * rather than creating a generic geometry column, this will restrict the column to the class provided
	 * e.g. Polygon, Point, Multipolygon, along with the srid, if provided
	 * @throws SQLException
	 */
	private void addGeometryColumns(String tsTable, List<String> columnNames, List<Class<?>> dataTypes, Integer srid) throws SQLException {
		String sql = "alter table \"" + tsTable + "\" ";
		for (int i = 0; i < columnNames.size(); i++) {
			if (i != columnNames.size() - 1) {
				sql += "add " + columnNames.get(i) + " geometry(" +  dataTypes.get(i).getSimpleName();
				if (srid != null) {
					sql += "," + String.valueOf(srid) + "), ";
				} else {
					sql += "), ";
				}
			} else {
				sql += "add " + columnNames.get(i) + " geometry(" +  dataTypes.get(i).getSimpleName();
				if (srid != null) {
					sql += "," + String.valueOf(srid) + ");";
				} else {
					sql += ");";
				}
			}
		}
		conn.prepareStatement(sql).executeUpdate();
	}

	/**
	 * Append time series data from TimeSeries object to (existing) RDB table
	 * <p>Requires existing RDB connection
	 * @param tsTable name of the timeseries table provided as string
	 * @param ts time series to write into the table
	 * @param dataColumnNames list of column names in the tsTable corresponding to the data in the ts
	 * @throws SQLException
	 */
	private void populateTimeSeriesTable(String tsTable, TimeSeries<T> ts, Map<String,String> dataColumnNames) throws SQLException {
		List<String> dataIRIs = ts.getDataIRIs();

		// Retrieve RDB table from table name
    	Table<?> table = DSL.table(DSL.name(tsTable));
    	
    	List<Field<?>> columnList = new ArrayList<>();
    	// Retrieve list of corresponding column names for dataIRIs
    	columnList.add(timeColumn);
    	for (String data : dataIRIs) {
    		columnList.add(DSL.field(DSL.name(dataColumnNames.get(data))));
    	}
    	
    	// collect the list of time values that exist in the table
    	// these rows are treated specially to avoid duplicates
    	List<Integer> rowsWithMatchingTime = new ArrayList<>();
    	// Populate columns row by row
        InsertValuesStepN<?> insertValueStep = context.insertInto(table, columnList);
		int num_rows_without_matching_time = 0;
        for (int i=0; i<ts.getTimes().size(); i++) {
        	// newValues is the row elements
        	if (!checkTimeRowExists(tsTable, ts.getTimes().get(i))) {
        		Object[] newValues = new Object[dataIRIs.size()+1];
    			newValues[0] = ts.getTimes().get(i); 
    			for (int j = 0; j < ts.getDataIRIs().size(); j++) {
    				newValues[j+1] = (ts.getValues(dataIRIs.get(j)).get(i));
    			}
    			insertValueStep = insertValueStep.values(newValues);
				num_rows_without_matching_time += 1;
        	} else {
        		rowsWithMatchingTime.add(i);
        	}
		}
		// open source jOOQ does not support postgis, hence not using execute() directly
		if (num_rows_without_matching_time != 0) {
			// if this gets executed when it's 0, null values will be added
			conn.prepareStatement(insertValueStep.toString()).executeUpdate();
		}
		
		// update existing rows with matching time value
		// only one row can be updated in a single query
		for (int rowIndex : rowsWithMatchingTime) {
			UpdateSetFirstStep<?> updateStep = context.update(table);
			
			for (int i = 0; i < ts.getDataIRIs().size(); i++) {
				String dataIRI = ts.getDataIRIs().get(i);
				
				if (i == (ts.getDataIRIs().size()-1)) {
					updateStep.set(DSL.field(DSL.name(dataColumnNames.get(dataIRI))), ts.getValues(dataIRI).get(rowIndex))
					.where(timeColumn.eq(ts.getTimes().get(rowIndex)));

					// open source jOOQ does not support postgis geometries, hence not using execute() directly
					conn.prepareStatement(updateStep.toString()).executeUpdate();
				} else {
					updateStep.set(DSL.field(DSL.name(dataColumnNames.get(dataIRI))), ts.getValues(dataIRI).get(rowIndex));
				}	
			}
		}
	}
	
	/**
	 * Check whether dataIRI has a tsIRI associated with it (i.e. dataIRI exists in central lookup table)
	 * <p>Requires existing RDB connection
	 * @param dataIRI data IRI provided as string
	 * @return True if the data IRI exists in central lookup table's dataIRI column, false otherwise
	 */
	boolean checkDataHasTimeSeries(String dataIRI) {
		connect();
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));
		return context.fetchExists(selectFrom(table).where(dataIRIcolumn.eq(dataIRI)));
	}
	
	/**
	 * Ensure that all dataIRIs are associated with same RDB table (i.e. have same time series IRI)
	 * <br>Throws JPSRuntime Exception if not all dataIRIs are attached to same table in the database
	 * <p>Requires existing RDB connection;
	 * @param dataIRI list of data IRIs provided as string
	 */
	private void checkDataIsInSameTable(List<String> dataIRI) {
		// Get time series IRI of first dataIRI
    	String tsIRI = getTimeSeriesIRI(dataIRI.get(0));
    	// Check that all further dataIRI share this time series IRI
    	if (dataIRI.size() > 1) {
    		for (int i = 1; i < dataIRI.size(); i++) {
    			String curTsIRI = getTimeSeriesIRI(dataIRI.get(i));
    			if (!curTsIRI.contentEquals(tsIRI)) {
    				throw new JPSRuntimeException(exceptionPrefix + "Provided data is not within the same RDB table");
    			}
    		}
    	}
	}
	
	/**
	 * Retrieve tsIRI for provided dataIRI from central database lookup table (if it exists)
	 * <p>Requires existing RDB connection
	 * @param dataIRI data IRI provided as string
	 * @return The attached time series IRI as string
	 */
	private String getTimeSeriesIRI(String dataIRI) {
		try {
			// Look for the entry dataIRI in dbTable
			Table<?> table = DSL.table(DSL.name(dbTableName));
			List<String> queryResult = context.select(tsIRIcolumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(tsIRIcolumn);
			// Throws IndexOutOfBoundsException if dataIRI is not present in central lookup table (i.e. queryResult is empty)
			return queryResult.get(0);
		} catch (IndexOutOfBoundsException e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + "<" + dataIRI + "> does not have an assigned time series instance"); 
		}
	}
		
	/**
	 * Retrieve column name for provided dataIRI from central database lookup table (if it exists)
	 * <p>Requires existing RDB connection
	 * @param dataIRI data IRI provided as string
	 * @return Corresponding column name in the RDB table related to the data IRI
	 */
	private String getColumnName(String dataIRI) {
		try {
			// Look for the entry dataIRI in dbTable
			Table<?> table = DSL.table(DSL.name(dbTableName));		
			List<String> queryResult = context.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(columnNameColumn);
			// Throws IndexOutOfBoundsException if dataIRI is not present in central lookup table (i.e. queryResult is empty)
			return queryResult.get(0);
		} catch (IndexOutOfBoundsException e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + "<" + dataIRI + "> does not have an assigned time series instance"); 
		}
	}

	/**
	 * Retrieve table name for provided dataIRI from central database lookup table (if it exists)
	 * <p>Requires existing RDB connection
	 * @param dataIRI data IRI provided as string
	 * @return Corresponding table name as string
	 */
	private String getTimeseriesTableName(String dataIRI) {
		try {
			// Look for the entry dataIRI in dbTable
			Table<?> table = DSL.table(DSL.name(dbTableName));
			List<String> queryResult = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(tsTableNameColumn);
			// Throws IndexOutOfBoundsException if dataIRI is not present in central lookup table (i.e. queryResult is empty)
			return queryResult.get(0);
		} catch (IndexOutOfBoundsException e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + "<" + dataIRI + "> does not have an assigned time series instance"); 
		}
	}

	/**
	 * Retrieve time series table for provided dataIRI in database
	 * <p>Requires existing RDB connection
	 * @param dataIRI data IRI provided as string
	 * @return Table object corresponding to the time series
	 */
	private Table<?> getTimeseriesTable(String dataIRI) {
		// Retrieve the table name attached to the data IRI
		String tableName = getTimeseriesTableName(dataIRI);

		return DSL.table(DSL.name(tableName));
	}
	
	/**
	 * check if a row exists to prevent duplicate rows with the same time value
	 * @param tsTable
	 * @param time
	 * @return
	 */
	private boolean checkTimeRowExists(String tsTableName, T time) {
		try {
			return context.fetchExists(selectFrom(DSL.table(DSL.name(tsTableName))).where(timeColumn.eq(time)));
		} catch (DataAccessException e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(exceptionPrefix + "Error in checking if a row exists for a given time value"); 
		}
	}

	/**
	 * Retrieve aggregate value of a column; stored data should be in numerics
	 * @param dataIRI data IRI provided as string
	 * @param aggregateFunction enumerator for the wanted type of aggregation (AVERAGE, MAX, MIN)   
	 * @return The aggregate value of the whole time series corresponding to the dataIRI.
	 */
	protected double getAggregate(String dataIRI, AggregateFunction aggregateFunction) {

		// Initialise connection and set jOOQ DSL context
		connect();
		
		// All database interactions in try-block to ensure closure of connection
		try {
			// Retrieve table corresponding to the time series connected to the data IRI
			Table<?> table = getTimeseriesTable(dataIRI);

			// Create map between the data IRI and the corresponding column field in the table
			String columnName = getColumnName(dataIRI);
			Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);

			switch (aggregateFunction) {
				case AVERAGE:
					return context.select(avg(columnField)).from(table).fetch(avg(columnField)).get(0).doubleValue();
				case MAX:
					return context.select(max(columnField)).from(table).fetch(max(columnField)).get(0);
				case MIN:
					return context.select(min(columnField)).from(table).fetch(min(columnField)).get(0);
				default:
					throw new JPSRuntimeException(exceptionPrefix + "Aggregate function "+aggregateFunction.name()+" not valid!");
			}

		} catch (JPSRuntimeException e) {
			// Re-throw JPSRuntimeExceptions
			throw e;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			// Throw all exceptions incurred by jooq (i.e. by SQL interactions with database) as JPSRuntimeException with respective message
			throw new JPSRuntimeException(exceptionPrefix + "Error while executing SQL command", e);
		}
		
	}

}
