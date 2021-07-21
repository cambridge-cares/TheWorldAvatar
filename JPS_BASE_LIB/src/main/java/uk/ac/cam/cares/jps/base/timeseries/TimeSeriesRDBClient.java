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
    
    /**
     * Standard constructor
     * @param timeClass
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
	public void initCentralTable() {
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
	 * @param dataIRI
	 * @param dataClass
	 * @param tsIRI
	 */
	public void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass, String tsIRI) {
		
		// Generate UUID as unique RDB table name
		String tsTableName = UUID.randomUUID().toString();
		
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
			
			// Check if central database lookup table exists
			if (context.meta().getTables(dbTableName).size() == 0) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: Central RDB lookup table needs to be initialised first");
			}
			
			// Check if any data has already been initialised (i.e. is associated with different tsIRI)
			for (String s : dataIRI) {
				if(checkDataHasTimeSeries(s)) {
					throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> already has a time series instance (i.e. tsIRI)");
				}
			}
			
			// Assign column name for each dataIRI; name for time column is fixed
			Map<String,String> dataColumnNames = new HashMap<String,String>();
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
	 * @param ts
     */
	public void addTimeSeriesData(TimeSeries<T> ts) {
    	List<String> dataIRI = ts.getDataIRIs();
    	
    	try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
	    	// Check if all required time series are initialised
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
			Map<String,String> dataColumnNames = new HashMap<String,String>();
			for (String s : dataIRI) {
				dataColumnNames.put(s, getColumnName(s));
			}
			
			// Append time series data
			populateTimeSeriesTable(tsTableName, ts, dataColumnNames);
			
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
    /** 
     * Retrieve entire time series from RDB
     * <p>Returns all data series from dataIRI list as one time series object (with potentially multiple related data series);
     * time series are in ascending order with respect to time (from oldest to newest)
     * @param dataIRI
     */
	public TimeSeries<T> getTimeSeries(List<String> dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
	    	// Check if all required time series are initialised
			for (String s : dataIRI) {
				if(!checkDataHasTimeSeries(s)) {
					throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> does not have a time series instance (i.e. tsIRI)");
				}
			}
	    	
			// Ensure that all provided dataIRIs/columns are located in the same RDB table (throws Exception if not)
			checkDataIsInSameTable(dataIRI);
	    	
			String tsIRI = getTimeSeriesIRI(dataIRI.get(0));
	    	Table<?> table = getTimeseriesTable(tsIRI);
	    	
	    	// Create map between data IRI and the corresponding column field in the table
	    	Map<String, Field<Object>> dataColumnFields = new HashMap<String,Field<Object>>();
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
	    	
	    	// Perform query (including potential time duplicates)
	    	Result<? extends Record> queryResult = context.select(columnList).from(table).orderBy(timeColumn.asc()).fetch();    	
	    	// mh807: Should a distinct query be preferred to account for potentially duplicate time steps? e.g.
	    	// Perform query excluding time duplicates (returns first row match for potentially duplicate time entries)
	    	//Result<? extends Record> queryResult = dsl.select(columnList).distinctOn(timeColumn).from(table).orderBy(timeColumn.asc()).fetch();
	    	
   	    	// Collect results and return a TimeSeries object
	    	List<T> timeValues = queryResult.getValues(timeColumn);
	    	List<List<?>> dataValues = new ArrayList<>();
	    	for (String data : dataIRI) {
	    		List<?> column = queryResult.getValues(dataColumnFields.get(data));
	    		dataValues.add(column);
	    	} 
	    	TimeSeries<T> ts = new TimeSeries<T>(timeValues, dataIRI, dataValues);
	    	
	    	return ts;
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
    }
    
    /** 
     * Retrieve time series within the given time bounds from RDB
     * <p>Returns all data series from dataIRI list as one time series object (with potentially multiple related data series);
     * time series are in ascending order with respect to time (from oldest to newest)
	 * @param dataIRI
	 * @param lowerBound
	 * @param upperBound
	 * @return
	 */
	public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRI, T lowerBound, T upperBound) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
	    	// mh807: Necessary to check whether bounds and timeColumn are of same class? (per method definition should be of same class)
	    	// Check whether lowerBound and upperBound are of correct type to be used in ".between"
	    	if (!lowerBound.getClass().equals(timeColumn.getType()) || !upperBound.getClass().equals(timeColumn.getType())) {
	    		throw new JPSRuntimeException("TimeSeriesRDBClient: Lower or upper bound are not of same class as time series entries");
	    	}
	
	    	// Check if all required time series are initialised
			for (String s : dataIRI) {
				if(!checkDataHasTimeSeries(s)) {
					throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> does not have a time series instance (i.e. tsIRI)");
				}
			}
	    	
			// Ensure that all provided dataIRIs/columns are located in the same RDB table (throws Exception if not)
			checkDataIsInSameTable(dataIRI);
	    	
			String tsIRI = getTimeSeriesIRI(dataIRI.get(0));
	    	Table<?> table = getTimeseriesTable(tsIRI);
	    	
	    	// Create map between data IRI and the corresponding column field in the table
	    	Map<String, Field<Object>> dataColumnFields = new HashMap<String,Field<Object>>();
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
	    	
	    	// Perform query (including potential time duplicates)
	    	Result<? extends Record> queryResult = context.select(columnList).from(table).where(timeColumn.between(lowerBound, upperBound))
	    			 .orderBy(timeColumn.asc()).fetch();
	    	// mh807: Should a distinct query be preferred to account for potentially duplicate time steps? e.g.
	    	// Perform query excluding time duplicates (returns first row match for potentially duplicate time entries)
	    	//Result<? extends Record> queryResult = dsl.select(columnList).distinctOn(timeColumn).from(table).where(timeColumn.between(lowerBound, upperBound))
	    	//		 .orderBy(timeColumn.asc()).fetch();
	    	
	    	// Collect results and return a TimeSeries object
	    	List<T> timeValues = queryResult.getValues(timeColumn);
	    	List<List<?>> dataValues = new ArrayList<>();
	    	for (String data : dataIRI) {
	    		List<?> column = queryResult.getValues(dataColumnFields.get(data));
	    		dataValues.add(column);
	    	} 
	    	TimeSeries<T> ts = new TimeSeries<>(timeValues, dataIRI, dataValues);
	    	
	    	return ts;
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Retrieve average value of a column; stored data should be in numerics
	 * @param dataIRI
	 * @return
	 */
	public double getAverage(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}
	    	
	    	String tsIRI = getTimeSeriesIRI(dataIRI);
	    	Table<?> table = getTimeseriesTable(tsIRI);
	    	
	    	// create map between dataIRI and the corresponding column field in the table
			String columnName = getColumnName(dataIRI);
			Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);
	    	
	    	List<BigDecimal> queryResult = context.select(avg(columnField)).from(table).fetch(avg(columnField));
	    	
	    	return queryResult.get(0).doubleValue();
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Retrieve maximum value of a column; stored data should be in numerics
	 * @param dataIRI
	 * @return
	 */
	public double getMaxValue(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
			
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}
	    	
	    	String tsIRI = getTimeSeriesIRI(dataIRI);
	    	Table<?> table = getTimeseriesTable(tsIRI);
	    	
	    	// create map between dataIRI and the corresponding column field in the table
			String columnName = getColumnName(dataIRI);
			Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);
	    	
	    	List<Double> queryResult = context.select(max(columnField)).from(table).fetch(max(columnField));
	    	
	    	return queryResult.get(0).doubleValue();
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Retrieve minimum value of a column; stored data should be in numerics
	 * @param dataIRI
	 * @return
	 */
	public double getMinValue(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}
			
	    	String tsIRI = getTimeSeriesIRI(dataIRI);
	    	Table<?> table = getTimeseriesTable(tsIRI);
	    	
	    	// create map between dataIRI and the corresponding column field in the table
			String columnName = getColumnName(dataIRI);
			Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);
	    	
	    	List<Double> queryResult = context.select(min(columnField)).from(table).fetch(min(columnField));
	    	
	    	return queryResult.get(0).doubleValue();
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Retrieve latest (maximum) time entry for a given dataIRI
	 * @param dataIRI
	 * @return
	 */
	public T getMaxTime(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}
	    	
	    	String tsIRI = getTimeSeriesIRI(dataIRI);
	    	Table<?> table = getTimeseriesTable(tsIRI);
	    	
	    	List<T> queryResult = context.select(max(timeColumn)).from(table).fetch(max(timeColumn));
	    	
	    	T maxTime = queryResult.get(0);
	    	
	    	return maxTime;
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Retrieve earliest (minimum) time entry for a given dataIRI
	 * @param dataIRI
	 * @return
	 */
	public T getMinTime(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}
	    	
	    	String tsIRI = getTimeSeriesIRI(dataIRI);
	    	Table<?> table = getTimeseriesTable(tsIRI);
	    	
	    	List<T> queryResult = context.select(min(timeColumn)).from(table).fetch(min(timeColumn));
	    	
	    	T minTime = queryResult.get(0);
	    	
	    	return minTime;
	    	
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Delete RDB time series table rows between lower and upper Bound
	 * <p>Note that this will delete the entire rows linked to this data (in addition to the given dataIRI)
	 * @param dataIRI
	 * @param lowerBound
	 * @param upperBound
	 */
	public void deleteRows(String dataIRI, T lowerBound, T upperBound) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
	    	// Check if time series is initialised
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance  (i.e. tsIRI)");
			}
			
			// Retrieve RDB table for dataIRI
			String tsIRI = getTimeSeriesIRI(dataIRI);
	    	Table<?> table = getTimeseriesTable(tsIRI);
	    	
	    	// Delete rows between bound (including bounds!)
	    	context.delete(table).where(timeColumn.between(lowerBound, upperBound)).execute();

		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		} finally {			
			disconnect();
		}
	}
	
	/**
	 * Delete individual time series (i.e. data for one dataIRI only)
	 * @param dataIRI
	 */
	public void deleteTimeSeries(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
	    	// Check if time series is initialised
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
				
				// Drop column from time series RDB table
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
	 * Delete all time series information related to a dataIRI (i.e. entire RDB table)
	 * @param dataIRI
	 */
	public void deleteTimeSeriesTable(String dataIRI) {
		try {
			// Initialise connection and set jOOQ DSL context
			connect();
	    	
	    	// Check if time series is initialised
			if(!checkDataHasTimeSeries(dataIRI)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
			}
			
			// Get time series RDB table
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
	 * @return
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
	 * @param tsTable
	 * @param dataIRI
	 * @param dataColumnNames
	 * @param tsIRI
	 */
	private void populateCentralTable(String tsTable, List<String> dataIRI, Map<String, String> dataColumnNames, String tsIRI) {	
		InsertValuesStep4<Record, String, String, String, String> insertValueStep = context.insertInto(DSL.table(DSL.name(dbTableName)),
				dataIRIcolumn, tsIRIcolumn, tsTableNameColumn, columnNameColumn);
		
		// mh807: Necessary to check whether dataIRI is already present in central table?
		//		  Necessary to check whether dataIRI and dataColumnNames have same length?
		
		// Populate columns row by row
		for (int i = 0; i < dataIRI.size(); i++) {
			insertValueStep = insertValueStep.values(dataIRI.get(i), tsIRI, tsTable, dataColumnNames.get(dataIRI.get(i)));
		}
		
		insertValueStep.execute();
	}
	
	/**
	 * Create an empty RDB table with the given data types for the respective columns
	 * @param tablename
	 * @param dataColumnNames
	 * @param dataIRI
	 * @param dataClass
	 */
	private void createEmptyTimeSeriesTable(String tablename, Map<String,String> dataColumnNames, List<String> dataIRI, 
											List<Class<?>> dataClass) {   	
		// mh807: Necessary to check whether dataColumnNames, dataIRI, and dataClass have same length?
		//		  How to ensure that dataIRI and dataClass have same order?
		
		// Create table
		CreateTableColumnStep createStep = context.createTableIfNotExists(tablename);
		
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
	 * @param tablename
	 * @param ts
	 * @param dataColumnNames
	 */
	private void populateTimeSeriesTable(String tablename, TimeSeries<T> ts, Map<String,String> dataColumnNames) {
		List<String> dataIRIs = ts.getDataIRIs();

    	Table<?> table = DSL.table(DSL.name(tablename));
    	
    	List<Field<?>> columnList = new ArrayList<>();
    	// Retrieve list of corresponding column names for dataIRIs
    	columnList.add(timeColumn);
    	for (String data : dataIRIs) {
    		// mh807: Necessary to check whether dataIRI from ts Object is present in table and to throw Exception if not?  
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
	 * @param dataIRI
	 * @return
	 */
	private boolean checkDataHasTimeSeries(String dataIRI) {
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));
		return context.fetchExists(selectFrom(table).where(dataIRIcolumn.eq(dataIRI)));
	}
	
	/**
	 * Ensure that all dataIRIs are associated with same RDB table (i.e. have same time series IRI)
	 * <p>Throws JPSRuntime Exception if not
	 * @param dataIRI
	 */
	private void checkDataIsInSameTable(List<String> dataIRI) {
		// Get time series IRI of first dataIRI
    	String tsIRI = getTimeSeriesIRI(dataIRI.get(0));
    	// Check that all further dataIRI share this time series IRI
    	if (dataIRI.size() > 1) {
    		for (int i = 1; i < dataIRI.size(); i++) {
    			String tsIRItmp = getTimeSeriesIRI(dataIRI.get(i));
    			if (!tsIRItmp.contentEquals(tsIRI)) {
    				throw new JPSRuntimeException("TimeSeriesRDBClient: Provided data is not within the same RDB table");
    			}
    		}
    	}
	}
	
	/**
	 * Retrieve tsIRI for provided dataIRI from central database lookup table (if it exists)
	 * @param dataIRI
	 * @return
	 */
	private String getTimeSeriesIRI(String dataIRI) {
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));
		List<String> queryresult = context.select(tsIRIcolumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(tsIRIcolumn);
		String tsIRI = queryresult.get(0);
		
	    return tsIRI;
	}
		
	/**
	 * Retrieve column name for provided dataIRI from central database lookup table (if it exists)
	 * @param dataIRI
	 * @return
	 */
	private String getColumnName(String dataIRI) {
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));		
		List<String> queryResult = context.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(columnNameColumn);
		String columnName = queryResult.get(0);
		
		return columnName;
	}

	/**
	 * Retrieve table name for provided timeseries IRI from central database lookup table (if it exists)
	 * @param tsIRI: IRI of the timeseries
	 * @return String
	 */
	private String getTimeseriesTableName(String tsIRI) {
		// Look for the entry tsIRI in dbTable
		Table<?> globalTable = DSL.table(DSL.name(dbTableName));
		List<String> queryResult = context.select(tsTableNameColumn).from(globalTable).where(tsIRIcolumn.eq(tsIRI)).fetch(tsTableNameColumn);

		return queryResult.get(0);
	}

	/**
	 * Retrieve table for provided timeseries IRI from central database lookup table (if it exists)
	 * @param tsIRI: IRI of the timeseries
	 * @return Table
	 */
	private Table<?> getTimeseriesTable(String tsIRI) {
		// Look for the entry tsIRI in dbTable
		String tableName = getTimeseriesTableName(tsIRI);

		return DSL.table(DSL.name(tableName));
	}
}
