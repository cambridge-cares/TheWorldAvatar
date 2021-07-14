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
 * T is the class type for the time values
 * @author Kok Foong Lee
 *
 */

public class TimeSeriesRDBClient<T> implements TimeSeriesClientInterface<T>{
	// User defined inputs
	// kbClient with the endpoint (triplestore/owl file) specified
	private KnowledgeBaseClientInterface kbClient = null; 
	// url and credentials for the relational database
	private String rdbURL = null; 
	private String rdbUser = null;
	private String rdbPassword = null;
	// time unit (in IRI)
	private String timeUnit = null;
	
	private final Field<T> timeColumn;
	// constants
	private static final SQLDialect dialect = SQLDialect.POSTGRES;
    
    // central database table
    private static final String dbTableName = "dbTable";
    private static final Field<String> dataIRIcolumn = DSL.field(DSL.name("dataIRI"), String.class);
    private static final Field<String> tsIRIcolumn = DSL.field(DSL.name("timeseriesIRI"), String.class);
    private static final Field<String> tsTableNameColumn = DSL.field(DSL.name("tableName"), String.class);
    private static final Field<String> columnNameColumn = DSL.field(DSL.name("columnName"), String.class);
    
    public TimeSeriesRDBClient(Class<T> timeClass) {
    	timeColumn = DSL.field(DSL.name("time"), timeClass);
    }
    
	public void setKBClient(KnowledgeBaseClientInterface kbClient) {
        this.kbClient = kbClient;
	}
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
		
		//generate IRI for time series
		UUID uuid = UUID.randomUUID();
		String tsIRI = TimeSeriesSparql.ns_kb + "TimeSeries_" + uuid.toString();
		
		/* old:
		int numTS = TimeSeriesSparql.countTS(kbClient);
		String tsIRI = TimeSeriesSparql.ns_kb + "ts" + (numTS+1);
		
		int i = 2;
		// ensure generated IRI is unique in the endpoint
		while (TimeSeriesSparql.checkTimeSeriesExists(kbClient, tsIRI)) {
			tsIRI = TimeSeriesSparql.ns_kb + "ts" + (numTS+i);
			i++;
		}
		*/
		
		// instantiate in KG
		TimeSeriesSparql.initTS(this.kbClient, tsIRI, dataIRI, this.rdbURL, this.timeUnit);
		
		// generate unique table name for this time series, cannot use data IRI as table names directly
		String tsTableName = generateUniqueTableName(tsIRI);
		
		// assign column name for each value, name for time column is fixed
		Map<String,String> dataColumnNames = new HashMap<String,String>();
		int i = 1;
		for (String s : dataIRI) {
			dataColumnNames.put(s, "column"+i);
			i++;
		}
		
		populateDatabaseTable(create, tsTableName, dataIRI, dataColumnNames, tsIRI);
		
		// create table for storing time series data
		initTimeSeriesTable(create, tsTableName, dataColumnNames, dataIRI, dataClass);
		
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
	 * queries the average value of a column, stored data should be in numerics
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
    	
    	// create map between data IRI and the corresponding column field in the table
		String columnName = getColumnName(dsl, dataIRI);
		Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);
    	
    	List<BigDecimal> queryResult = dsl.select(avg(columnField)).from(table).fetch(avg(columnField));
    	closeConnection(conn);
    	
    	return queryResult.get(0).doubleValue();
	}
	
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
    	
    	// create map between data IRI and the corresponding column field in the table
		String columnName = getColumnName(dsl, dataIRI);
		Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);
    	
    	List<Double> queryResult = dsl.select(max(columnField)).from(table).fetch(max(columnField));
    	closeConnection(conn);
    	
    	return queryResult.get(0).doubleValue();
	}
	
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
    	
    	// create map between data IRI and the corresponding column field in the table
		String columnName = getColumnName(dsl, dataIRI);
		Field<Double> columnField = DSL.field(DSL.name(columnName), Double.class);
    	
    	List<Double> queryResult = dsl.select(min(columnField)).from(table).fetch(min(columnField));
    	closeConnection(conn);
    	
    	return queryResult.get(0).doubleValue();
	}
	
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
		TimeSeriesSparql.removeTimeSeries(kbClient, tsIRI);
    
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
	}
	
	/**
	 * returns the table name containing the time series given the data IRI, if it exists
	 * @param dataIRI
	 */
	private String getTableName(DSLContext dsl, String tsIRI) {
		// table
		Table<?> table = DSL.table(DSL.name(dbTableName));
		
		List<String> queryResult = dsl.select(tsTableNameColumn).from(table).where(tsIRIcolumn.eq(tsIRI)).fetch(tsTableNameColumn);
		String tableName = queryResult.get(0);
		
		return tableName;
	}
	
	/**
	 * get column name in the time series table for the given data IRI
	 * @param dsl
	 * @param dataIRI
	 */
	private String getColumnName(DSLContext dsl, String dataIRI) {
		// table
		Table<?> table = DSL.table(DSL.name(dbTableName));
		
		List<String> queryResult = dsl.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(columnNameColumn);
		String columnName = queryResult.get(0);
		
		return columnName;
	}
	
	private Connection connect() {
		Connection conn = null;
		try {
			Class.forName("org.postgresql.Driver");
        	conn = DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
        	System.out.println("Connected to " + this.rdbURL);
			return conn;
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		}
    }
	
	private void closeConnection(Connection conn) {
		// close connection to RDB
		try {
			conn.close();
		} catch (SQLException e) {
			throw new JPSRuntimeException(e);
		}
	}
	
	private void createDatabaseTable(DSLContext create) {
		create.createTableIfNotExists(dbTableName).column(dataIRIcolumn).column(tsIRIcolumn)
		.column(tsTableNameColumn).column(columnNameColumn).execute();
	}
	
	private void populateDatabaseTable(DSLContext create, String tsTable, List<String> dataIRI, Map<String, String> dataColumnNames, String tsIRI) {	
		InsertValuesStep4<Record, String, String, String, String> insertValueStep = create.insertInto(DSL.table(DSL.name(dbTableName)), 
				dataIRIcolumn, tsIRIcolumn, tsTableNameColumn, columnNameColumn);

		for (int i = 0; i < dataIRI.size(); i++) {
			insertValueStep = insertValueStep.values(dataIRI.get(i),tsIRI,tsTable,dataColumnNames.get(dataIRI.get(i)));
		}
		
		insertValueStep.execute();
	}
	
	private static String generateUniqueTableName(String tsIRI) {
		String source = tsIRI;

		byte[] bytes = null;
		try {
			bytes = source.getBytes("UTF-8");
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		UUID uuid = java.util.UUID.nameUUIDFromBytes(bytes);
		
		return uuid.toString();
	}
	
	/**
	 * Creates an empty table with the given data types
	 * @param tablename
	 * @param timeClass
	 * @param valueClassList
	 */
	private void initTimeSeriesTable(DSLContext create, String tablename, Map<String,String> dataColumnNames, List<String> dataIRI,
			List<Class<?>> dataClass) {   	
		CreateTableColumnStep createStep = create.createTableIfNotExists(tablename);
		
    	// create time column
    	createStep = createStep.column(timeColumn);
    	
    	// create 1 column for each value
    	for (int i = 0; i < dataIRI.size(); i++) {
    		createStep = createStep.column(dataColumnNames.get(dataIRI.get(i)), DefaultDataType.getDataType(dialect, dataClass.get(i)));
    	}

    	// send request to db
    	createStep.execute();
	}
	
	private void populateTimeSeriesTable(DSLContext dsl, String tablename, TimeSeries<T> ts, Map<String,String> dataColumnNames) {
		List<String> dataIRIs = ts.getDataIRI();

    	Table<?> table = DSL.table(DSL.name(tablename));
    	
    	List<Field<?>> columnList = new ArrayList<>();
    	columnList.add(timeColumn);
    	for (String data : dataIRIs) {
    		columnList.add(DSL.field(DSL.name(dataColumnNames.get(data))));
    	}
    	
    	// column
        InsertValuesStepN<?> insertValueStep = dsl.insertInto(table, columnList);
        for (int i=0; i<ts.getTimes().size(); i++) {
        	// values are inserted row by row
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
	
	private boolean checkDataHasTimeSeries(DSLContext dsl, String dataIRI) {
		// look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));
		return dsl.fetchExists(selectFrom(table).where(dataIRIcolumn.eq(dataIRI)));
	}
	
	private String getTimeSeriesIRI(DSLContext dsl, String dataIRI) {
		// look for the entry dataIRI in dbTable
		Table<?> table = DSL.table(DSL.name(dbTableName));
		List<String> queryresult = dsl.select(tsIRIcolumn).from(table).where(dataIRIcolumn.eq(dataIRI)).fetch(tsIRIcolumn);
	    return queryresult.get(0);
	}
}
