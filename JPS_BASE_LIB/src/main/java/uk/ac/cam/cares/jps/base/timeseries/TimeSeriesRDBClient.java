package uk.ac.cam.cares.jps.base.timeseries;

import java.io.UnsupportedEncodingException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.DataType;
import org.jooq.Field;
import org.jooq.InsertValuesStep4;
import org.jooq.InsertValuesStepN;
import org.jooq.Record;
import org.jooq.Record1;
import org.jooq.Record2;
import org.jooq.Result;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;
import org.jooq.impl.DefaultDataType;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;
import uk.ac.cam.cares.jps.base.interfaces.TimeSeriesClientInterface;

/**
 * This class uses the jooq library to interact with the relational database.
 * @author Kok Foong Lee
 *
 */

public class TimeSeriesRDBClient implements TimeSeriesClientInterface{
	// User defined inputs
	// kbClient with the endpoint (triplestore/owl file) specified
	private KnowledgeBaseClientInterface kbClient; 
	// url and credentials for the relational database
	private String rdbURL; 
	private String rdbUser;
	private String rdbPassword;
	// time unit (in IRI)
	private String timeUnit = null;
	
	// constants
	private static final SQLDialect dialect = SQLDialect.POSTGRES;
    private static final String timeColumnName = "time";
    private static final Field<Object> timeColumn = DSL.field(DSL.name(timeColumnName));
	
    // central database table
    private static final String dbTableName = "dbTable";
    private static final String dataIRIcolumn = "dataIRI";
    private static final String tsIRIcolumn = "timeseriesIRI";
    private static final String tableRefName = "tableName";
    private static final String columnRefName = "columnName";
    
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
	 */
	public void init(Class<?> timeClass, List<String> dataIRI, List<Class<?>> dataClass) {
		// check if data already exists
		for (String s : dataIRI) {
			if(TimeSeriesSparql.checkDataHasTimeSeries(kbClient, s)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> already has a time series instance");
			}
		}
		
		//generate IRI for time series
		int numTS = TimeSeriesSparql.countTS(kbClient);
		String tsIRI = TimeSeriesSparql.namespace + "ts" + (numTS+1);
		
		int i = 2;
		// ensure generated IRI is unique in the endpoint
		while (TimeSeriesSparql.checkTimeSeriesExists(kbClient, tsIRI)) {
			tsIRI = TimeSeriesSparql.namespace + "ts" + (numTS+i);
			i++;
		}
		
		// instantiate in KG
		TimeSeriesSparql.initTS(this.kbClient, tsIRI, dataIRI, this.rdbURL, this.timeUnit);
		
		// initialise connection
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect); 
		
		// generate unique table name for this time series, cannot use data IRI as table names directly
		String tsTableName = generateUniqueTableName(tsIRI);
		
		// assign column name for each value, name for time column is fixed
		Map<String,String> dataColumnNames = new HashMap<String,String>();
		i = 1;
		for (String s : dataIRI) {
			dataColumnNames.put(s, "column"+i);
			i++;
		}
		
		// check if database exists and create it
		createDatabaseTable(create);
		populateDatabaseTable(create, tsTableName, dataIRI, dataColumnNames, tsIRI);
		
		// create table for storing time series data
		initTimeSeriesTable(create, tsTableName, dataColumnNames, dataIRI, dataClass, timeClass);
		
		closeConnection(conn);
	}
	
    public void addData(TimeSeries ts) {
    	List<String> dataIRI = ts.getDataIRI();
    	
    	// check if time series is initialised
		for (String s : dataIRI) {
			if(!TimeSeriesSparql.checkDataHasTimeSeries(kbClient, s)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> does not have a time series instance");
			}
		}
    	
		// first ensure that each provided column is located in the same table by checking its time series IRI
    	String tsIRI = TimeSeriesSparql.getTimeSeriesIRI(kbClient, dataIRI.get(0));
    	if (dataIRI.size() > 1) {
    		for (int i = 1; i < dataIRI.size(); i++) {
    			String tsIRItmp = TimeSeriesSparql.getTimeSeriesIRI(kbClient, dataIRI.get(i));
    			if (!tsIRItmp.contentEquals(tsIRI)) {
    				throw new JPSRuntimeException("TimeSeriesSparql: Provided data is not within the same table");
    			}
    		}
    	}
    	
    	// initialise connection
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
    	String tsTableName = getTableName(dsl, tsIRI);
    	// assign column name for each value, name for time column is fixed
		Map<String,String> dataColumnNames = new HashMap<String,String>();
		for (String s : dataIRI) {
			dataColumnNames.put(s, getColumnName(dsl, s));
		}
		
		populateTimeSeriesTable(dsl, tsTableName, ts, dataColumnNames);
	}
	
    /** 
     * returns the entire time series
     * @param dataIRI
     */
	public TimeSeries getTimeSeries(List<String> dataIRI) {
    	// check if time series is initialised
		for (String s : dataIRI) {
			if(!TimeSeriesSparql.checkDataHasTimeSeries(kbClient, s)) {
				throw new JPSRuntimeException("TimeSeriesRDBClient: <" + s + "> does not have a time series instance");
			}
		}
    	
    	// make sure they are in the same table
    	String tsIRI = TimeSeriesSparql.getTimeSeriesIRI(kbClient, dataIRI.get(0));
    	if (dataIRI.size() > 1) {
    		for (int i = 1; i < dataIRI.size(); i++) {
    			String tsIRItmp = TimeSeriesSparql.getTimeSeriesIRI(kbClient, dataIRI.get(i));
    			if (!tsIRItmp.contentEquals(tsIRI)) {
    				throw new JPSRuntimeException("TimeSeriesSparql: Provided data is not within the same table");
    			}
    		}
    	}
    	
    	// initialise connection and query from RDB
    	Connection conn = connect();
    	DSLContext dsl = DSL.using(conn, dialect); 
    	
    	String tsTableName = getTableName(dsl, tsIRI);
    	Table<?> table = DSL.table(DSL.name(tsTableName));
    	
    	// create map between data IRI and the corresponding column field in the table
    	Map<String, Field<Object>> dataColumnFields = new HashMap<String,Field<Object>>();
		for (String data : dataIRI) {
			String columnName = getColumnName(dsl, data);
			Field<Object> field = DSL.field(DSL.name(columnName));
			dataColumnFields.put(data, field);
		}
    	
    	List<Field<Object>> columnList = new ArrayList<>();
    	columnList.add(timeColumn);
    	for (String data : dataIRI) {
    	    columnList.add(dataColumnFields.get(data));
    	}
    	
    	// perform query
    	Result<? extends Record> queryResult = dsl.select(columnList).from(table).fetch();
    	
    	// collect results and return a TimeSeries object
    	List<Object> timeValues = queryResult.getValues(timeColumn);
    	List<List<?>> dataValues = new ArrayList<>();
    	for (String data : dataIRI) {
    		List<?> column = queryResult.getValues(dataColumnFields.get(data));
    		dataValues.add(column);
    	} 
    	TimeSeries ts = new TimeSeries(timeValues, dataIRI, dataValues);
    	
    	return ts;
    }
    
	/**
	 * returns the table name containing the time series given the data IRI, if it exists
	 * @param dataIRI
	 */
	private String getTableName(DSLContext dsl, String tsIRI) {
		// table
		Table<?> table = DSL.table(DSL.name(dbTableName));
		// column
		Field<Object> tableColumn = DSL.field(DSL.name(tableRefName));
		Field<Object> tsColumn =  DSL.field(DSL.name(tsIRIcolumn));
		
		Result<Record1<Object>> queryResult = dsl.select(tableColumn).from(table).where(tsColumn.eq(tsIRI)).fetch();
		String tableName = (String) queryResult.getValue(0, tableColumn);
		
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
		// column
		Field<Object> dataColumn = DSL.field(DSL.name(dataIRIcolumn));
		Field<Object> columnRef =  DSL.field(DSL.name(columnRefName));
		
		Result<Record1<Object>> queryResult = dsl.select(columnRef).from(table).where(dataColumn.eq(dataIRI)).fetch();
		String columnName = (String) queryResult.getValue(0, columnRef);
		
		return columnName;
	}
	
	private Connection connect() {
		Connection conn = null;
		try {
        	conn = DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
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
	
	public void createDatabaseTable(DSLContext create) {
		DataType<String> dataType = DefaultDataType.getDataType(dialect,String.class);
		create.createTableIfNotExists(dbTableName).column(dataIRIcolumn,dataType).column(tsIRIcolumn,dataType)
		.column(tableRefName,dataType).column(columnRefName,dataType).execute();
	}
	
	public void populateDatabaseTable(DSLContext create, String tsTable, List<String> dataIRI, Map<String, String> dataColumnNames, String tsIRI) {	
		InsertValuesStep4<?,Object,Object,Object,Object> insertValueStep = create.insertInto(DSL.table(DSL.name(dbTableName)), 
				DSL.field(DSL.name(dataIRIcolumn)), DSL.field(DSL.name(tsIRIcolumn)), DSL.field(DSL.name(tableRefName)), DSL.field(DSL.name(columnRefName)));

		for (int i = 0; i < dataIRI.size(); i++) {
			insertValueStep = insertValueStep.values(dataIRI.get(i),tsIRI,tsTable,dataColumnNames.get(dataIRI.get(i)));
		}
		
		insertValueStep.execute();
	}
	
	public static String generateUniqueTableName(String tsIRI) {
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
	public void initTimeSeriesTable(DSLContext create, String tablename, Map<String,String> dataColumnNames, List<String> dataIRI,
			List<Class<?>> dataClass, Class<?> timeClass) {   	
		CreateTableColumnStep createStep = create.createTableIfNotExists(tablename);
		
    	// create time column
    	createStep = createStep.column(timeColumnName, DefaultDataType.getDataType(dialect,timeClass));
    	
    	// create 1 column for each value
    	for (int i = 0; i < dataIRI.size(); i++) {
    		createStep = createStep.column(dataColumnNames.get(dataIRI.get(i)), DefaultDataType.getDataType(dialect, dataClass.get(i)));
    	}

    	// send request to db
    	createStep.execute();
	}
	
	public void populateTimeSeriesTable(DSLContext dsl, String tablename, TimeSeries ts, Map<String,String> dataColumnNames) {
		List<String> dataIRIs = ts.getDataIRI();

    	Table<?> table = DSL.table(DSL.name(tablename));
    	
    	Field<Object>[] columnArray = new Field[dataIRIs.size()+1];
    	columnArray[0] = timeColumn;
    	for (int i = 0; i < dataIRIs.size(); i++) {
    	    columnArray[i+1] = DSL.field(DSL.name(dataColumnNames.get(dataIRIs.get(i))));
    	}
    	
    	// column
        InsertValuesStepN<?> insertValueStep = dsl.insertInto(table, columnArray);
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
	
	public void queryTimeSeries(String tablename, long lowerBound, long upperBound) {
		Connection conn = connect();
		Table<?> table = DSL.table(tablename);
		Field<Object> valueColumn = DSL.field("value");
		DSLContext create = DSL.using(conn, dialect);
		Result<Record2<Object, Object>> result = create.select(timeColumn, valueColumn).from(table).where(timeColumn.between(lowerBound, upperBound)).fetch();
	    
		List<Long> timeList = result.getValues(timeColumn).stream().map(t->(Long)t).collect(Collectors.toList()); 
		List<Double> valueList = result.getValues(valueColumn).stream().map(t->(Double)t).collect(Collectors.toList()); 
//		TimeSeries ts = new TimeSeries(timeList,valueList);
	}
	
	public void deleteRows(String tablename, long lowerBound, long upperBound) {
		Connection conn = connect();
		Table<?> table = DSL.table(tablename);
		DSLContext create = DSL.using(conn, dialect);
		Field<Object> timeColumn = DSL.field("time");
		create.delete(table).where(timeColumn.between(lowerBound, upperBound)).execute();
	}
	
	public void clearTable(String tablename) {
		Connection conn = connect();
		Table<?> table = DSL.table(tablename);
		DSLContext create = DSL.using(conn, dialect);
		create.delete(table).execute();
	}
	
	public void dropTable(String tablename) {
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect);
		create.dropTable(tablename).execute();
	}
}
