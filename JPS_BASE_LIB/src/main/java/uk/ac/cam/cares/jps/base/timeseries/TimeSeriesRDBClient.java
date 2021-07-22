package uk.ac.cam.cares.jps.base.timeseries;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.InsertValuesStep4;
import org.jooq.InsertValuesStepN;
import org.jooq.Record;
import org.jooq.Result;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;
import org.jooq.impl.DefaultDataType;
import static org.jooq.impl.DSL.*;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TimeSeriesClientInterface;

/**
 * This class uses the jooq library to interact with the relational database.
 * <T> is the class type for the time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
 * @author Kok Foong Lee
 */

public class TimeSeriesRDBClient<T> implements TimeSeriesClientInterface<T>{
	
	// URL and credentials for the relational database
	private String rdbURL = null; 
	private String rdbUser = null;
	private String rdbPassword = null;
	// Time unit (in IRI)
	private String timeUnit = null;
	// RDB connection properties and jooq configuration 
	private Connection conn;
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

	private enum AggregateFunction {
		AVERAGE,
		MAX,
		MIN
	}

    /**
     * Standard constructor
     * @param timeClass: class of the timestamps of the time series
     */
    public TimeSeriesRDBClient(Class<T> timeClass) {
    	timeColumn = DSL.field(DSL.name("time"), timeClass);
    }
    
    /**
     * Get and set methods for private attributes
     */
	public void setTimeUnit(String timeUnit) {
		this.timeUnit = timeUnit;
	}
	public String getTimeUnit() {
		return timeUnit;
	}
	
	// Relational database properties (e.g. PostgreSQL)
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
	 * Initialise central database lookup table
	 */
	private void initCentralTable() {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
			
			// Initialise central lookup table: only creates empty table if it does not exist, otherwise it is left unchanged
			context.createTableIfNotExists(dbTableName).column(dataIRIcolumn).column(tsIRIcolumn)
				  .column(tsTableNameColumn).column(columnNameColumn).execute();
			
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Initialise RDB table for particular time series and add respective entries to central lookup table
	 * <p>For the list of supported classes, refer org.jooq.impl.SQLDataType
	 * <p>The timeseries IRI needs to be provided. A unique uuid for the corresponding table will be generated.
	 * @param dataIRI: list of IRIs for the data provided as string
	 * @param dataClass: list with the corresponding Java class (typical String, double or int) for each data IRI
	 * @param tsIRI: IRI of the timeseries provided as string
	 */
	public void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI) {
		
		// Generate UUID as unique RDB table name
		String tsTableName = UUID.randomUUID().toString();
		
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
			
			// Check if central database lookup table exists and create if not
			if (context.meta().getTables(dbTableName).size() == 0) {
				initCentralTable();
				// Reconnect, as initCentralTable closes connection
				connect();
			}
			
			// Check if any data has already been initialised (i.e. is associated with different tsIRI)
			for (String s : dataIRI) {
				if(checkDataHasTimeSeries(s)) {
					throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> already has a time series instance (i.e. tsIRI)");
				}
			}

			// Ensure that there is a class for each data IRI
			if (dataIRI.size() != dataClass.size()) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: Length of dataClass is different from number of data IRIs.");
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
			createEmptyTimeSeriesTable(tsTableName, dataColumnNames, dataIRI, dataClass);
			
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
		
	}
	
    /**
     * Append time series data to an already existing RDB table
	 * If certain columns within the table are not provided, they will be nulls
	 * @param ts: timeseries object to add
     */
	public void addTimeSeriesData(TimeSeries<T> ts) {
    	List<String> dataIRI = ts.getDataIRIs();
    	
    	try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
	    	// Check if all data IRIs have an entry in the central table, i.e. are attached to a timeseries
			for (String s : dataIRI) {
				if(!checkDataHasTimeSeries(s)) {
					throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> does not have a time series instance (i.e. tsIRI)"); 
				}
			}
	    	
			// Ensure that all provided dataIRIs/columns are located in the same RDB table (throws Exception if not)
			checkDataIsInSameTable(dataIRI);
	    	
			String tsIRI = getTimeSeriesIRI(dataIRI.get(0));
	    	String tsTableName = getTimeseriesTableName(tsIRI);
	    	// Assign column name for each dataIRI; name for time column is fixed
			Map<String,String> dataColumnNames = new HashMap<>();
			for (String s : dataIRI) {
				dataColumnNames.put(s, getColumnName(s));
			}
			
			// Append time series data to time series table
			populateTimeSeriesTable(tsTableName, ts, dataColumnNames);
			
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
    /** 
     * Retrieve time series within bounds from RDB (time bounds are inclusive and optional)
     * <p>Returns all data series from dataIRI list as one time series object (with potentially multiple related data series);
     * <br>Returned time series are in ascending order with respect to time (from oldest to newest)
     * <br>Returned time series contain potential duplicates (i.e. multiple entries for same time stamp)
	 * @param dataIRI: list of data IRIs provided as string
	 * @param lowerBound: start timestamp from which to retrieve data (null if not applicable)
	 * @param upperBound: end timestamp until which to retrieve data (null if not applicable)
	 */
	public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRI, T lowerBound, T upperBound) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();

			// Check if all data IRIs have an entry in the central table, i.e. are attached to a timeseries
			for (String s : dataIRI) {
				if(!checkDataHasTimeSeries(s)) {
					throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> does not have a time series instance (i.e. tsIRI)");
				}
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
	    	
	    	// Perform query (including potential time duplicates)
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
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/** 
     * Retrieve entire time series from RDB
	 * @param dataIRI: list of data IRIs provided as string
	 */
	public TimeSeries<T> getTimeSeries(List<String> dataIRI) {
		return getTimeSeriesWithinBounds(dataIRI, null, null);
	}
	
	/**
	 * Retrieve average value of a column; stored data should be in numerics
	 * @param dataIRI: data IRI provided as string
	 * @return The average of the corresponding data as double
	 */
	public double getAverage(String dataIRI) {
		return getAggregate(dataIRI, AggregateFunction.AVERAGE);
	}
	
	/**
	 * Retrieve maximum value of a column; stored data should be in numerics
	 * @param dataIRI: data IRI provided as string
	 * @return The maximum of the corresponding data as double
	 */
	public double getMaxValue(String dataIRI) {
		return getAggregate(dataIRI, AggregateFunction.MAX);
	}
	
	/**
	 * Retrieve minimum value of a column; stored data should be in numerics
	 * @param dataIRI: data IRI provided as string
	 * @return The minimum of the corresponding data as double
	 */
	public double getMinValue(String dataIRI) {
		return getAggregate(dataIRI, AggregateFunction.MIN);
	}
	
	/**
	 * Retrieve latest (maximum) time entry for a given dataIRI
	 * @param dataIRI: data IRI provided as string
	 * @return The maximum (latest) timestamp of the corresponding data
	 */
	public T getMaxTime(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();

			// Check that the data IRI has an entry in the central table, i.e. is attached to a timeseries
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}

			// Retrieve table corresponding to the time series connected to the data IRI
	    	Table<?> table = getTimeseriesTable(dataIRI);
	    	
	    	List<T> queryResult = context.select(max(timeColumn)).from(table).fetch(max(timeColumn));
	    	
	    	return queryResult.get(0);
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Retrieve earliest (minimum) time entry for a given dataIRI
	 * @param dataIRI: data IRI provided as string
	 * @return The minimum (earliest) timestamp of the corresponding data
	 */
	public T getMinTime(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();

			// Check that the data IRI has an entry in the central table, i.e. is attached to a timeseries
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}

			// Retrieve table corresponding to the time series connected to the data IRI
	    	Table<?> table = getTimeseriesTable(dataIRI);
	    	
	    	List<T> queryResult = context.select(min(timeColumn)).from(table).fetch(min(timeColumn));
	    	
	    	return queryResult.get(0);
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Delete RDB time series table rows between lower and upper Bound
	 * <p>Note that this will delete the entire rows in the corresponding table, i.e. all columns (in addition to the given data IRI)
	 * @param dataIRI: data IRI provided as string
	 * @param lowerBound: start timestamp from which to delete data
	 * @param upperBound: end timestamp until which to delete data
	 */
	public void deleteRows(String dataIRI, T lowerBound, T upperBound) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();

			// Check that the data IRI has an entry in the central table, i.e. is attached to a timeseries
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance  (i.e. tsIRI)");
			}
			
			// Retrieve RDB table for dataIRI
	    	Table<?> table = getTimeseriesTable(dataIRI);
	    	
	    	// Delete rows between bounds (including bounds!)
	    	context.delete(table).where(timeColumn.between(lowerBound, upperBound)).execute();

		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Delete individual time series (i.e. data for one dataIRI only)
	 * @param dataIRI: data IRI provided as string
	 */
	public void deleteTimeSeries(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();

			// Check that the data IRI has an entry in the central table, i.e. is attached to a timeseries
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}
			
			// Get time series RDB table		
			String tsIRI = getTimeSeriesIRI(dataIRI);
			String columnName = getColumnName(dataIRI);
			String tsTableName = getTimeseriesTableName(tsIRI);
			
			// Get meta information for RDB table (column fields, etc.)
			Table<?> tsTable = context.meta().getTables(tsTableName).get(0);
			
			if (tsTable.fields().length > 2) {

				// Delete only column for dataIRI from RDB table if further columns are present
				context.alterTable(tsTableName).drop(columnName).execute();
		    	
		    	// Delete entry in central lookup table
		    	Table<?> dbTable = DSL.table(DSL.name(dbTableName));
		    	context.delete(dbTable).where(dataIRIcolumn.equal(dataIRI)).execute();
			
			} else {
				// Delete entire RDB table for single column time series (data column + time column)
				deleteTimeSeriesTable(dataIRI);
			}
			
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Delete all time series information related to a dataIRI (i.e. entire RDB table and entries in central table)
	 * @param dataIRI: data IRI provided as string
	 */
	public void deleteTimeSeriesTable(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();

			// Check that the data IRI has an entry in the central table, i.e. is attached to a timeseries
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}

			// Retrieve RDB table for dataIRI
			String tsIRI = getTimeSeriesIRI(dataIRI);
			String tsTableName = getTimeseriesTableName(tsIRI);
	    
	    	// Delete time series RDB table
			context.dropTable(DSL.table(DSL.name(tsTableName))).execute();
	    	
	    	// Delete entries in central lookup table
	    	Table<?> dbTable = DSL.table(DSL.name(dbTableName));
	    	context.delete(dbTable).where(tsIRIcolumn.equal(tsIRI)).execute();
    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Delete all time series RDB tables and central lookup table
	 */
	public void deleteAll() {
		try {
			// Initialise connection and context to RDB
	    	connect(); 
	    	
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
			
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Establish connection to RDB and set DSL context
	 */
	private void connect() {
		try {
			// Load required driver
			Class.forName("org.postgresql.Driver");
			// Connect to DB (using static connection and context properties)
        	this.conn = DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
        	System.out.println("Connected to: " + this.rdbURL);
        	this.context = DSL.using(this.conn, dialect); 
		} catch (Exception e) {
			System.out.println("Connection failed to: " + this.rdbURL);
			throw new JPSRuntimeException(e);
		}
    }
	
	/**
	 * Close existing connection to RDB
	 */
	private void disconnect() {
		try {
			conn.close();
		} catch (SQLException e) {
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * Add new entries to central RDB lookup table
	 * <p>Requires existing RDB connection
	 * @param tsTable: name of the timeseries table provided as string
	 * @param dataIRI: list of data IRIs provided as string
	 * @param dataColumnNames: list of column names in the tsTable corresponding to the data IRIs
	 * @param tsIRI: timeseries IRI provided as string
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
	 * @param tsTable: name of the timeseries table provided as string
	 * @param dataColumnNames: list of column names in the tsTable corresponding to the data IRIs
	 * @param dataIRI: list of data IRIs provided as string
	 * @param dataClass: list with the corresponding Java class (typical String, double or int) for each data IRI
	 */
	private void createEmptyTimeSeriesTable(String tsTable, Map<String,String> dataColumnNames, List<String> dataIRI,
											List<Class<?>> dataClass) {
		
		// Create table
		CreateTableColumnStep createStep = context.createTableIfNotExists(tsTable);
		
    	// Create time column
    	createStep = createStep.column(timeColumn);
    	
    	// Create 1 column for each value
    	for (int i = 0; i < dataIRI.size(); i++) {
    		createStep = createStep.column(dataColumnNames.get(dataIRI.get(i)), DefaultDataType.getDataType(dialect, dataClass.get(i)));
    	}

    	// Send consolidated request to RDB
    	createStep.execute();
	}
	
	/**
	 * Append time series data from ts Object to (existing) RDB table
	 * <p>Requires existing RDB connection
	 * @param tsTable: name of the timeseries table provided as string
	 * @param ts: time series to write into the table
	 * @param dataColumnNames: list of column names in the tsTable corresponding to the data in the ts
	 */
	private void populateTimeSeriesTable(String tsTable, TimeSeries<T> ts, Map<String,String> dataColumnNames) {
		List<String> dataIRIs = ts.getDataIRIs();

		// Retrieve RDB table from table name
    	Table<?> table = DSL.table(DSL.name(tsTable));
    	
    	List<Field<?>> columnList = new ArrayList<>();
    	// Retrieve list of corresponding column names for dataIRIs
    	columnList.add(timeColumn);
    	for (String data : dataIRIs) {
    		columnList.add(DSL.field(DSL.name(dataColumnNames.get(data))));
    	}
    	
    	// Populate columns row by row
        InsertValuesStepN<?> insertValueStep = context.insertInto(table, columnList);
        for (int i=0; i<ts.getTimes().size(); i++) {
        	// newValues is the row elements
			Object[] newValues = new Object[dataIRIs.size()+1];
			newValues[0] = ts.getTimes().get(i); 
			for (int j = 0; j < ts.getDataIRIs().size(); j++) {
				newValues[j+1] = (ts.getValues(dataIRIs.get(j)).get(i));
			}
			insertValueStep = insertValueStep.values(newValues);
		}
		insertValueStep.execute();
	}
	
	/**
	 * Check whether dataIRI has a tsIRI associated with it (i.e. dataIRI exists in central lookup table)
	 * <p>Requires existing RDB connection
	 * @param dataIRI: data IRI provided as string
	 * @return True if the data IRI is attached to a time series, false otherwise
	 */
	private boolean checkDataHasTimeSeries(String dataIRI) {
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));
		return context.fetchExists(selectFrom(table).where(dataIRIcolumn.eq(dataIRI)));
	}
	
	/**
	 * Ensure that all dataIRIs are associated with same RDB table (i.e. have same time series IRI)
	 * <p>Throws JPSRuntime Exception if not
	 * <p>Requires existing RDB connection;
	 * @param dataIRI: list of data IRIs provided as string
	 */
	private void checkDataIsInSameTable(List<String> dataIRI) {
		// Get time series IRI of first dataIRI
    	String tsIRI = getTimeSeriesIRI(dataIRI.get(0));
    	// Check that all further dataIRI share this time series IRI
    	if (dataIRI.size() > 1) {
    		for (int i = 1; i < dataIRI.size(); i++) {
    			String curTsIRI = getTimeSeriesIRI(dataIRI.get(i));
    			if (!curTsIRI.contentEquals(tsIRI)) {
    				throw new JPSRuntimeException("TimeSeriesRDBClient: Provided data is not within the same RDB table");
    			}
    		}
    	}
	}
	
	/**
	 * Retrieve tsIRI for provided dataIRI from central database lookup table (if it exists)
	 * <p>Requires existing RDB connection
	 * @param dataIRI: data IRI provided as string
	 * @return The attached time series IRI as string
	 */
	private String getTimeSeriesIRI(String dataIRI) {
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));
		List<String> queryresult = context.select(tsIRIcolumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(tsIRIcolumn);
		
	    return queryresult.get(0);
	}
		
	/**
	 * Retrieve column name for provided dataIRI from central database lookup table (if it exists)
	 * <p>Requires existing RDB connection
	 * @param dataIRI: data IRI provided as string
	 * @return The corresponding column name in the table related to the data IRI
	 */
	private String getColumnName(String dataIRI) {
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));		
		List<String> queryResult = context.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(columnNameColumn);
		
		return queryResult.get(0);
	}

	/**
	 * Retrieve table name for provided timeseries IRI from central database lookup table (if it exists)
	 * <p>Requires existing RDB connection
	 * @param tsIRI: IRI of the timeseries
	 * @return Corresponding table name as string
	 */
	private String getTimeseriesTableName(String tsIRI) {
		// Look for the entry tsIRI in dbTable
		Table<?> globalTable = DSL.table(DSL.name(dbTableName));
		List<String> queryResult = context.select(tsTableNameColumn).from(globalTable).where(tsIRIcolumn.eq(tsIRI)).fetch(tsTableNameColumn);

		return queryResult.get(0);
	}

	/**
	 * Retrieve table for provided timeseries IRI from central database lookup table (if it exists)
	 * <p>Requires existing RDB connection
	 * @param dataIRI: data IRI provided as string
	 * @return Table object corresponding to the time series
	 */
	private Table<?> getTimeseriesTable(String dataIRI) {
		// Retrieve the time series IRI attached ot the data IRI
		String tsIRI = getTimeSeriesIRI(dataIRI);
		// Retrieve the table name
		String tableName = getTimeseriesTableName(tsIRI);

		return DSL.table(DSL.name(tableName));
	}

	/**
	 * Retrieve aggregate value of a column; stored data should be in numerics
	 * @param dataIRI: data IRI provided as string
	 * @param aggregateFunction: enumerator for the wanted type of aggregation (AVERAGE, MAX, MIN)   
	 * @return
	 */
	private double getAggregate(String dataIRI, AggregateFunction aggregateFunction) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();

			// Check that the data IRI has an entry in the central table, i.e. is attached to a timeseries
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}

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
					throw new JPSRuntimeException("Aggregate function "+aggregateFunction.name()+" not valid!");
			}

		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {
			disconnect();
		}
	}

}
