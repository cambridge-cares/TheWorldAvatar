package uk.ac.cam.cares.jps.base.timeseries;

import java.io.UnsupportedEncodingException;
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
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;
import uk.ac.cam.cares.jps.base.interfaces.TimeSeriesClientInterface;

/**
 * This class uses the jooq library to interact with the relational database.
 * <T> is the class type for the time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
 * @author Kok Foong Lee
 */

public class TimeSeriesRDBClient<T> implements TimeSeriesClientInterface<T>{
	/* mh807: move
	// User defined inputs
	// kbClient with the endpoint (triplestore/owl file) specified
	private KnowledgeBaseClientInterface kbClient = null; 
	*/
	// URL and credentials for the relational database
	private String rdbURL = null; 
	private String rdbUser = null;
	private String rdbPassword = null;
	// Time unit (in IRI)
	private String timeUnit = null;
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
    
    /* mh807: move
	public void setKBClient(KnowledgeBaseClientInterface kbClient) {
        this.kbClient = kbClient;
	}
	*/
	public void setTimeUnit(String timeUnit) {
		this.timeUnit = timeUnit;
	}
	
	// Relational database properties (e.g. PostgreSQL)
	public void setRdbURL(String rdbURL) {
		this.rdbURL = rdbURL;
	}
	public void setRdbUser(String user) {
		this.rdbUser = user;
	}
	public void setRdbPassword(String password) {
		this.rdbPassword = password;
	}
	
	/**
	 * mh807: remove
	 * Initialise Time Series in RDF and RDB
	 * For the list of supported classes, refer org.jooq.impl.SQLDataType
	 */
	// Niklas: Should be split into initializing the global table and initializing the table for the specific time series
	// and corresponding entries in the global table.
	public void init(List<String> dataIRI, List<Class<?>> dataClass) {
		// initialise connection
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect); 
		
		// check if database (look up table) exists and create it
		createDatabaseTable(create);
		
		// check if data already exists
		// mh807: throwing an error prevents tests if dbTable already exists
		for (String s : dataIRI) {
			if(checkDataHasTimeSeries(create, s)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> already has a time series instance");
			}
		}
		
		//Generate IRI for time series
		UUID uuid = UUID.randomUUID();
		String tsIRI = TimeSeriesSparql.ns_kb + "TimeSeries_" + uuid.toString();
		
		/* mh807: move
		// instantiate in KG
		TimeSeriesSparql.initTS(this.kbClient, tsIRI, dataIRI, this.rdbURL, this.timeUnit);
		*/
		
		// generate unique table name for this time series, cannot use data IRI as table names directly
		String tsTableName = generateUniqueTableName(tsIRI);
		
		// assign column name for each value, name for time column is fixed
		Map<String,String> dataColumnNames = new HashMap<String,String>();
		int i = 1;
		for (String s : dataIRI) {
			dataColumnNames.put(s, "column"+i);
			i++;
		}
		
		populateCentralTable(create, tsTableName, dataIRI, dataColumnNames, tsIRI);
		
		// create table for storing time series data
		createEmptyTimeSeriesTable(create, tsTableName, dataColumnNames, dataIRI, dataClass);
		
		closeConnection(conn);
	}
	
	/**
	 * Initialise central database lookup table
	 */
	public void initCentralTable() {
		// Initialise connection
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect); 
		
		// Initialise central lookup table: only creates empty table if it does not exist, otherwise it is left unchanged
		create.createTableIfNotExists(dbTableName).column(dataIRIcolumn).column(tsIRIcolumn)
			  .column(tsTableNameColumn).column(columnNameColumn).execute();
		
		closeConnection(conn);
	}
	
	/**
	 * Initialise RDB table for particular time series and add respective entries to central lookup table
	 * For the list of supported classes, refer org.jooq.impl.SQLDataType
	 * @param dataIRI
	 * @param dataClass
	 */
	public void initTimeSeriesTable(List<String> dataIRI, List<Class<?>> dataClass) {
		// Initialise connection
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect); 
		
		// Check if central database lookup table exists
		if (create.meta().getTables(dbTableName).size() == 0) {
			throw new JPSRuntimeException("TimeSeriesRDBClient: Central RDB lookup table needs to be initialised first");
		}
		
		// Check if any data has already been initialised
		for (String s : dataIRI) {
			if(checkDataHasTimeSeries(create, s)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> already has a time series instance (i.e. tsIRI)");
			}
		}
		
		// Generate UUID as unique RDB table name and tsIRI suffix
		String uuid = UUID.randomUUID().toString();
		String tsTableName = uuid;
		String tsIRI = TimeSeriesSparql.ns_kb + "TimeSeries_" + uuid;
				
		/* mh807: move
		// instantiate in KG
		TimeSeriesSparql.initTS(this.kbClient, tsIRI, dataIRI, this.rdbURL, this.timeUnit);
		*/
		
		/* mh807: old implementation of unique table new
		// generate unique table name for this time series, cannot use data IRI as table names directly
		String tsTableName = generateUniqueTableName(tsIRI);
		*/
		
		// Assign column name for each dataIRI; name for time column is fixed
		Map<String,String> dataColumnNames = new HashMap<String,String>();
		int i = 1;
		for (String s : dataIRI) {
			dataColumnNames.put(s, "column"+i);
			i++;
		}
		
		// Add corresponding entries in central lookup table
		populateCentralTable(create, tsTableName, dataIRI, dataColumnNames, tsIRI);
		
		// Initialise RDB table for storing time series data
		createEmptyTimeSeriesTable(create, tsTableName, dataColumnNames, dataIRI, dataClass);
		
		closeConnection(conn);
		
	}
	
	/**
	 * appends data to the already existing table
	 * If certain columns within the table are not provided, they will be nulls
	 */
    public void addTimeSeries(TimeSeries<T> ts) {
    	List<String> dataIRI = ts.getDataIRI();
    	
    	// initialise connection
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
    	// check if time series is initialised
		for (String s : dataIRI) {
			if(!checkDataHasTimeSeries(dsl, s)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> does not have a time series instance");
			}
		}
    	
		// first ensure that each provided column is located in the same table by checking its time series IRI
    	String tsIRI = getTimeSeriesIRI(dsl, dataIRI.get(0));
    	if (dataIRI.size() > 1) {
    		for (int i = 1; i < dataIRI.size(); i++) {
    			String tsIRItmp = getTimeSeriesIRI(dsl, dataIRI.get(i));
    			if (!tsIRItmp.contentEquals(tsIRI)) {
    				throw new JPSRuntimeException("TimeSeriesSparql: Provided data is not within the same table");
    			}
    		}
    	}
    	
    	String tsTableName = getTableName(dsl, tsIRI);
    	// assign column name for each value, name for time column is fixed
		Map<String,String> dataColumnNames = new HashMap<String,String>();
		for (String s : dataIRI) {
			dataColumnNames.put(s, getColumnName(dsl, s));
		}
		
		populateTimeSeriesTable(dsl, tsTableName, ts, dataColumnNames);
		closeConnection(conn);
	}
	
    /** 
     * returns the entire time series
     * @param dataIRI
     */
	public TimeSeries<T> getTimeSeries(List<String> dataIRI) {
		// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
    	// check if time series is initialised
		for (String s : dataIRI) {
			if(!checkDataHasTimeSeries(dsl, s)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> does not have a time series instance");
			}
		}
    	
    	// make sure they are in the same table
    	String tsIRI = getTimeSeriesIRI(dsl, dataIRI.get(0));
    	if (dataIRI.size() > 1) {
    		for (int i = 1; i < dataIRI.size(); i++) {
    			String tsIRItmp = getTimeSeriesIRI(dsl, dataIRI.get(i));
    			if (!tsIRItmp.contentEquals(tsIRI)) {
    				throw new JPSRuntimeException("TimeSeriesSparql: Provided data is not within the same table");
    			}
    		}
    	}
    	
    	String tsTableName = getTableName(dsl, tsIRI);
    	Table<?> table = DSL.table(DSL.name(tsTableName));
    	
    	// create map between data IRI and the corresponding column field in the table
    	Map<String, Field<Object>> dataColumnFields = new HashMap<String,Field<Object>>();
		for (String data : dataIRI) {
			String columnName = getColumnName(dsl, data);
			Field<Object> field = DSL.field(DSL.name(columnName));
			dataColumnFields.put(data, field);
		}
    	
    	List<Field<?>> columnList = new ArrayList<>();
    	columnList.add(timeColumn);
    	for (String data : dataIRI) {
    	    columnList.add(dataColumnFields.get(data));
    	}
    	
    	// perform query
    	Result<? extends Record> queryResult = dsl.select(columnList).from(table).orderBy(timeColumn.asc()).fetch();
    	closeConnection(conn);
    	
    	// collect results and return a TimeSeries object
    	List<T> timeValues = queryResult.getValues(timeColumn);
    	List<List<?>> dataValues = new ArrayList<>();
    	for (String data : dataIRI) {
    		List<?> column = queryResult.getValues(dataColumnFields.get(data));
    		dataValues.add(column);
    	} 
    	TimeSeries<T> ts = new TimeSeries<T>(timeValues, dataIRI, dataValues);
    	
    	return ts;
    }
    
	/**
	 * returns time series within the given time bounds
	 * @param dataIRI
	 * @param lowerBound
	 * @param upperBound
	 * @return
	 */
	public TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRI, T lowerBound, T upperBound) {
		// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
		
		// check if time series is initialised
		for (String s : dataIRI) {
			if(!checkDataHasTimeSeries(dsl, s)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> does not have a time series instance");
			}
		}
    	
    	// make sure they are in the same table
    	String tsIRI = getTimeSeriesIRI(dsl, dataIRI.get(0));
    	if (dataIRI.size() > 1) {
    		for (int i = 1; i < dataIRI.size(); i++) {
    			String tsIRItmp = getTimeSeriesIRI(dsl, dataIRI.get(i));
    			if (!tsIRItmp.contentEquals(tsIRI)) {
    				throw new JPSRuntimeException("TimeSeriesSparql: Provided data is not within the same table");
    			}
    		}
    	}
    	
    	String tsTableName = getTableName(dsl, tsIRI);
    	Table<?> table = DSL.table(DSL.name(tsTableName));
    	
    	// create map between data IRI and the corresponding column field in the table
    	Map<String, Field<Object>> dataColumnFields = new HashMap<String,Field<Object>>();
		for (String data : dataIRI) {
			String columnName = getColumnName(dsl, data);
			Field<Object> field = DSL.field(DSL.name(columnName));
			dataColumnFields.put(data, field);
		}
    	
    	List<Field<?>> columnList = new ArrayList<>();
    	columnList.add(timeColumn);
    	for (String data : dataIRI) {
    	    columnList.add(dataColumnFields.get(data));
    	}
    	
    	// perform query
    	Result<? extends Record> queryResult = dsl.select(columnList).from(table).where(timeColumn.between(lowerBound, upperBound))
    			.orderBy(timeColumn.asc()).fetch();
    	closeConnection(conn);
    	
    	// collect results and return a TimeSeries object
    	List<T> timeValues = queryResult.getValues(timeColumn);
    	List<List<?>> dataValues = new ArrayList<>();
    	for (String data : dataIRI) {
    		List<?> column = queryResult.getValues(dataColumnFields.get(data));
    		dataValues.add(column);
    	} 
    	TimeSeries<T> ts = new TimeSeries<>(timeValues, dataIRI, dataValues);
    	
    	return ts;
	}
	
	/**
	 * Retrieve average value of a column; stored data should be in numerics
	 * @param dataIRI
	 * @return
	 */
	public double getAverage(String dataIRI) {
		// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
		if(!checkDataHasTimeSeries(dsl, dataIRI)) {
			throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
		}
    	
    	String tsIRI = getTimeSeriesIRI(dsl, dataIRI);
    	String tsTableName = getTableName(dsl, tsIRI);
    	Table<?> table = DSL.table(DSL.name(tsTableName));
    	
    	// create map between dataIRI and the corresponding column field in the table
		String columnName = getColumnName(dsl, dataIRI);
		Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);
    	
    	List<BigDecimal> queryResult = dsl.select(avg(columnField)).from(table).fetch(avg(columnField));
    	closeConnection(conn);
    	
    	return queryResult.get(0).doubleValue();
	}
	
	/**
	 * Retrieve maximum value of a column; stored data should be in numerics
	 * @param dataIRI
	 * @return
	 */
	public double getMaxValue(String dataIRI) {
		// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
		
		if(!checkDataHasTimeSeries(dsl, dataIRI)) {
			throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
		}
    	
    	String tsIRI = getTimeSeriesIRI(dsl, dataIRI);
    	String tsTableName = getTableName(dsl, tsIRI);
    	Table<?> table = DSL.table(DSL.name(tsTableName));
    	
    	// create map between dataIRI and the corresponding column field in the table
		String columnName = getColumnName(dsl, dataIRI);
		Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);
    	
    	List<Double> queryResult = dsl.select(max(columnField)).from(table).fetch(max(columnField));
    	closeConnection(conn);
    	
    	return queryResult.get(0).doubleValue();
	}
	
	/**
	 * Retrieve minimum value of a column; stored data should be in numerics
	 * @param dataIRI
	 * @return
	 */
	public double getMinValue(String dataIRI) {
		// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
		if(!checkDataHasTimeSeries(dsl, dataIRI)) {
			throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
		}
		
    	String tsIRI = getTimeSeriesIRI(dsl, dataIRI);
    	String tsTableName = getTableName(dsl, tsIRI);
    	Table<?> table = DSL.table(DSL.name(tsTableName));
    	
    	// create map between dataIRI and the corresponding column field in the table
		String columnName = getColumnName(dsl, dataIRI);
		Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);
    	
    	List<Double> queryResult = dsl.select(min(columnField)).from(table).fetch(min(columnField));
    	closeConnection(conn);
    	
    	return queryResult.get(0).doubleValue();
	}
	
	/**
	 * Retrieve latest (maximum) time entry for a given dataIRI
	 * @param dataIRI
	 * @return
	 */
	public T getMaxTime(String dataIRI) {
		// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
		if(!checkDataHasTimeSeries(dsl, dataIRI)) {
			throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
		}
    	
    	String tsIRI = getTimeSeriesIRI(dsl, dataIRI);
    	String tsTableName = getTableName(dsl, tsIRI);
    	Table<?> table = DSL.table(DSL.name(tsTableName));
    	
    	List<T> queryResult = dsl.select(max(timeColumn)).from(table).fetch(max(timeColumn));
    	closeConnection(conn);
    	
    	T maxTime = queryResult.get(0);
    	
    	return maxTime;
	}
	
	/**
	 * Retrieve earliest (minimum) time entry for a given dataIRI
	 * @param dataIRI
	 * @return
	 */
	public T getMinTime(String dataIRI) {
		// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
		if(!checkDataHasTimeSeries(dsl, dataIRI)) {
			throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
		}
    	
    	String tsIRI = getTimeSeriesIRI(dsl, dataIRI);
    	String tsTableName = getTableName(dsl, tsIRI);
    	Table<?> table = DSL.table(DSL.name(tsTableName));
    	
    	List<T> queryResult = dsl.select(min(timeColumn)).from(table).fetch(min(timeColumn));
    	closeConnection(conn);
    	
    	T minTime = queryResult.get(0);
    	
    	return minTime;
	}
	
	/**
	 * note that this will delete the entire row linked to this data (in addition to the given dataIRI)
	 * @param dataIRI
	 * @param lowerBound
	 * @param upperBound
	 */
	public void deleteRows(String dataIRI, T lowerBound, T upperBound) {
		// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
		if(!checkDataHasTimeSeries(dsl, dataIRI)) {
			throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
		}
		
		String tsIRI = getTimeSeriesIRI(dsl, dataIRI);
    	
    	String tsTableName = getTableName(dsl, tsIRI);
    	Table<?> table = DSL.table(DSL.name(tsTableName));

    	dsl.delete(table).where(timeColumn.between(lowerBound, upperBound)).execute();
    	closeConnection(conn);
	}
	
	/**
	 * note that this will delete all time series information related to this data IRI, including other data in the same table
	 * @param dataIRI
	 */
	public void deleteTimeSeries(String dataIRI) {
		// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
		if(!checkDataHasTimeSeries(dsl, dataIRI)) {
			throw new JPSRuntimeException("TimeSeriesRDBClient: <" + dataIRI + "> does not have a time series instance");
		}
		
		String tsIRI = getTimeSeriesIRI(dsl, dataIRI);
		/* mh807: move
		TimeSeriesSparql.removeTimeSeries(kbClient, tsIRI);
		*/
    
    	//delete time series table
    	String tsTableName = getTableName(dsl, tsIRI);
    	dsl.dropTable(DSL.table(DSL.name(tsTableName))).execute();
    	
    	//delete entry in the main table
    	Table<?> dbTable = DSL.table(DSL.name(dbTableName));
    	dsl.delete(dbTable).where(tsIRIcolumn.equal(tsIRI)).execute();
    	closeConnection(conn);
	}
	
	/**
	 * deletes everything related to time series
	 */
	public void deleteAll() {
		/* mh807: move
		List<String> tsIRI = TimeSeriesSparql.getAllTimeSeries(kbClient);
		
		if (!tsIRI.isEmpty()) {
			Connection conn = connect();
	    	DSLContext dsl = DSL.using(conn, dialect); 
	    	Table<?> dbTable = DSL.table(DSL.name(dbTableName));
			// remove triples in KG and the time series table
			for (String ts : tsIRI) {
				TimeSeriesSparql.removeTimeSeries(kbClient, ts);
				
		    	//delete time series table
		    	String tsTableName = getTableName(dsl, ts);
		    	dsl.dropTable(DSL.table(DSL.name(tsTableName))).execute();
		    	
		    	//delete entry in the main table
		    	dsl.delete(dbTable).where(tsIRIcolumn.equal(ts)).execute();
			}
			
			// delete lookup table
			dsl.dropTable(dbTable).execute();
		}
		*/
	}
	
	/**
	 * Establish connection to RDB
	 * @return
	 */
	private Connection connect() {
		Connection conn = null;
		try {
			// Load required driver
			Class.forName("org.postgresql.Driver");
			// Connect to DB
        	conn = DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
        	System.out.println("Connected to " + this.rdbURL);
			return conn;
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		}
    }
	
	/**
	 * Close existing connection to RDB
	 * @param conn
	 */
	private void closeConnection(Connection conn) {
		try {
			conn.close();
		} catch (SQLException e) {
			throw new JPSRuntimeException(e);
		}
	}
	
	// mh807: to be removed
	private void createDatabaseTable(DSLContext create) {
		create.createTableIfNotExists(dbTableName).column(dataIRIcolumn).column(tsIRIcolumn)
		.column(tsTableNameColumn).column(columnNameColumn).execute();
	}
	
	/**
	 * Add new entries to central RDB lookup table
	 * @param dsl
	 * @param tsTable
	 * @param dataIRI
	 * @param dataColumnNames
	 * @param tsIRI
	 */
	private void populateCentralTable(DSLContext dsl, String tsTable, List<String> dataIRI, Map<String, String> dataColumnNames, String tsIRI) {	
		InsertValuesStep4<Record, String, String, String, String> insertValueStep = dsl.insertInto(DSL.table(DSL.name(dbTableName)),
				dataIRIcolumn, tsIRIcolumn, tsTableNameColumn, columnNameColumn);
		
		// mh807: Necessary to check whether dataIRI is already present in central table?
		//		  Necessary to check whether dataIRI and dataColumnNames have same length?
		
		// Populate columns row by row
		for (int i = 0; i < dataIRI.size(); i++) {
			insertValueStep = insertValueStep.values(dataIRI.get(i), tsIRI, tsTable, dataColumnNames.get(dataIRI.get(i)));
		}
		
		insertValueStep.execute();
	}
	
	// mh807: potentially remove, as UUID.randomUUID() seems easier
	/**
	 * Generate unique (RDB) table name based on time series IRI
	 * @param tsIRI
	 * @return  
	 */
	private static String generateUniqueTableName(String tsIRI) {
		String source = tsIRI;

		byte[] bytes = null;
		try {
			bytes = source.getBytes("UTF-8");
		}
		catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}

		UUID uuid = java.util.UUID.nameUUIDFromBytes(bytes);
		
		return uuid.toString();
	}
	
	/**
	 * Create an empty RDB table with the given data types for the respective columns
	 * @param dsl
	 * @param tablename
	 * @param dataColumnNames
	 * @param dataIRI
	 * @param dataClass
	 */
	private void createEmptyTimeSeriesTable(DSLContext dsl, String tablename, Map<String,String> dataColumnNames, List<String> dataIRI, 
											List<Class<?>> dataClass) {   	
		// mh807: Necessary to check whether dataColumnNames, dataIRI, and dataClass have same length?
		//		  How to ensure that dataIRI and dataClass have same order?
		
		// Create table
		CreateTableColumnStep createStep = dsl.createTableIfNotExists(tablename);
		
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
	 * @param dsl
	 * @param tablename
	 * @param ts
	 * @param dataColumnNames
	 */
	private void populateTimeSeriesTable(DSLContext dsl, String tablename, TimeSeries<T> ts, Map<String,String> dataColumnNames) {
		List<String> dataIRIs = ts.getDataIRI();

    	Table<?> table = DSL.table(DSL.name(tablename));
    	
    	List<Field<?>> columnList = new ArrayList<>();
    	// Retrieve list of corresponding column names for dataIRIs
    	columnList.add(timeColumn);
    	for (String data : dataIRIs) {
    		// mh807: Necessary to check whether dataIRI from ts Object is present in table and to throw Exception if not?  
    		columnList.add(DSL.field(DSL.name(dataColumnNames.get(data))));
    	}
    	
    	// Populate columns row by row
        InsertValuesStepN<?> insertValueStep = dsl.insertInto(table, columnList);
        for (int i=0; i<ts.getTimes().size(); i++) {
        	// newValues is the row elements
			Object[] newValues = new Object[dataIRIs.size()+1];
			newValues[0] = ts.getTimes().get(i); 
			for (int j = 0; j < ts.getDataIRI().size(); j++) {
				newValues[j+1] = (ts.getValues(dataIRIs.get(j)).get(i));
			}
			insertValueStep = insertValueStep.values(newValues);
		}
		insertValueStep.execute();
	}
	
	/**
	 * Check whether dataIRI has a tsIRI associated with it (i.e. respective tsIRI entry not null)
	 * @param dsl
	 * @param dataIRI
	 * @return
	 */
	private boolean checkDataHasTimeSeries(DSLContext dsl, String dataIRI) {
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));
		return dsl.fetchExists(selectFrom(table).where(dataIRIcolumn.eq(dataIRI)));
	}
	
	/**
	 * Retrieve tsIRI for provided dataIRI from central database lookup table (if it exists)
	 * @param dsl
	 * @param dataIRI
	 * @return
	 */
	private String getTimeSeriesIRI(DSLContext dsl, String dataIRI) {
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));
		List<String> queryresult = dsl.select(tsIRIcolumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(tsIRIcolumn);
		String tsIRI = queryresult.get(0);
		
	    return tsIRI;
	}
		
	/**
	 * Retrieve column name for provided dataIRI from central database lookup table (if it exists)
	 * @param dsl
	 * @param dataIRI
	 * @return
	 */
	private String getColumnName(DSLContext dsl, String dataIRI) {
		// Look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));		
		List<String> queryResult = dsl.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(columnNameColumn);
		String columnName = queryResult.get(0);
		
		return columnName;
	}
	
	/**
	 * Retrieve table name for provided timeseriesIRI from central database lookup table (if it exists)
	 * @param dsl
	 * @param tsIRI
	 * @return
	 */
	private String getTableName(DSLContext dsl, String tsIRI) {
		// Look for the entry tsIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));		
		List<String> queryResult = dsl.select(tsTableNameColumn).from(table).where(tsIRIcolumn.eq(tsIRI)).fetch(tsTableNameColumn);
		String tableName = queryResult.get(0);
		
		return tableName;
	}
}
