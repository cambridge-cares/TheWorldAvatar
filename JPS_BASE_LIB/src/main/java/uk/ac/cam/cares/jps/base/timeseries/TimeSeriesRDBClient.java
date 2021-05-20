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
import org.jooq.InsertValuesStep3;
import org.jooq.InsertValuesStepN;
import org.jooq.Record2;
import org.jooq.Result;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;
import org.jooq.impl.DefaultDataType;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;
import uk.ac.cam.cares.jps.base.interfaces.TimeSeriesClientInterface;

public class TimeSeriesRDBClient implements TimeSeriesClientInterface{
	// User defined inputs
	// kbClient with the endpoint (triplestore/owl file) specified
	private KnowledgeBaseClientInterface kbClient;
	// optional input if users want the time series to be instantiated within a named graph
	private String namedGraph = null; 
	// url and credentials for the relational database
	private String rdbURL; 
	private String rdbUser;
	private String rdbPassword;
	// time unit (in IRI)
	private String timeUnit = null;
	
	// constants
	// this class has been only tested with postgres
	private static final SQLDialect dialect = SQLDialect.POSTGRES;
    private static final String timeColumnName = "time";
    private static final Field<Object> timeColumn = DSL.field(timeColumnName);
	
	public void setKBClient(KnowledgeBaseClientInterface kbClient) {
        this.kbClient = kbClient;
	}
	public void setNamedGraph(String namedGraph) {
        this.namedGraph = namedGraph;
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
	public void init(TimeSeries<?,?> ts) {
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
		TimeSeriesSparql.initTS(this.kbClient, this.namedGraph, tsIRI, ts.getDataIRI(), this.rdbURL, this.timeUnit);
		
		// initialise connection
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect); 
		
		// generate unique table name for this time series, cannot use data IRI as table names directly
		String tsTableName = generateUniqueTableName(tsIRI);
		
		// assign column name for each value, name for time column is fixed
		Map<String,String> dataColumnNames = new HashMap<String,String>();
		i = 1;
		for (String dataIRI : ts.getDataIRI()) {
			dataColumnNames.put(dataIRI, "Column"+i);
			i++;
		}
		
		// create table for storing time series data
		initTimeSeriesTable(create, tsTableName, ts, dataColumnNames);
		
		// check if database exists and create it, maps data IRI to the table name
		createDatabaseTable(create, tsTableName, ts.getDataIRI(), dataColumnNames);
		
		closeConnection(conn);
	}
	
	public void addData(TimeSeries<?,?> ts) {
		
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
	
	public void createDatabaseTable(DSLContext create, String tsTable, List<String> dataIRI, Map<String,String> dataColumnNames) {
		String dbTableName = "DataIRITable";
		String dataIRIcolumn = "DataIRI";
		String tableRefName = "TableName";
		String columnRefName = "ColumnName";
		
		DataType<String> dataType = DefaultDataType.getDataType(dialect,String.class);
		create.createTableIfNotExists(dbTableName).column(dataIRIcolumn,dataType).column(tableRefName,dataType).column(columnRefName,dataType).execute();
		
        Table<?> table = DSL.table(dbTableName);
		
		InsertValuesStep3<?, Object, Object, Object> insertValueStep = create.insertInto(table, DSL.field(dataIRIcolumn), DSL.field(tableRefName), DSL.field(columnRefName));
		
		for (int i = 0; i < dataIRI.size(); i++) {
			List<String> row = new ArrayList<String>();
			row.add(dataIRI.get(i));row.add(tsTable);row.add(dataColumnNames.get(dataIRI.get(i)));
			insertValueStep = insertValueStep.values(row);
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
	public void initTimeSeriesTable(DSLContext create, String tablename, TimeSeries<?,?> ts, Map<String,String> dataColumnNames) {   	
		CreateTableColumnStep createStep = create.createTableIfNotExists(tablename);
    	
		Class<?> timeClass = ts.getTimeClass();
		if (timeClass == Object.class) {
			throw new JPSRuntimeException("TimeSeriesRDBClient: You must specify a suitable class for the time column");
		}
		
    	// create time column
    	createStep = createStep.column(timeColumnName, DefaultDataType.getDataType(dialect,timeClass));
    	
    	// create 1 column for each value
    	for (String dataIRI : ts.getDataIRI()) {
    		createStep = createStep.column(dataColumnNames.get(dataIRI), DefaultDataType.getDataType(dialect,ts.getValueClass(dataIRI)));
    	}

    	// send request to db
    	createStep.execute();
	}
	
	public void insertValues(String tablename, TimeSeries<?,?> ts) {
		// check time and value have the same length
//		if (ts.getTime().size() != ts.getValues().size()) {
//			throw new JPSRuntimeException("Array size of time and values are not the same");
//		}
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect);
		
		Table<?> table = DSL.table(tablename);
		
		Field<Object> value1Column = DSL.field("value1");
		Field<Object> value2Column = DSL.field("value2");
		
		Field<Object>[] columnArray = new Field[3];
		columnArray[0] = timeColumn;
		columnArray[1] = value1Column;
		columnArray[2] = value2Column;
		
		// column
//		InsertValuesStepN<?> insertValueStep = create.insertInto(table, columnArray);
//		for (int i=0; i<ts.getTime().size(); i++) {
//			List<Object> newValues = new ArrayList<>();
//			newValues.add(ts.getTime().get(i)); 
//			
//			for (int j=0; j<2; j++) {
//				newValues.add(ts.getValues().get(j).get(i));
//			}
//			insertValueStep = insertValueStep.values(newValues);
//		}
//		insertValueStep.execute();
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
