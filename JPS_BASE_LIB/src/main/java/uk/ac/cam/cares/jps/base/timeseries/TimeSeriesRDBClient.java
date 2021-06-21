package uk.ac.cam.cares.jps.base.timeseries;

import java.io.UnsupportedEncodingException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
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
    private static final String dataIRIcolumn = "dataIri";
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
		TimeSeriesSparql.initTS(this.kbClient, tsIRI, ts.getDataIRI(), this.rdbURL, this.timeUnit);
		
		// initialise connection
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect); 
		
		// generate unique table name for this time series, cannot use data IRI as table names directly
		String tsTableName = generateUniqueTableName(tsIRI);
		
		// assign column name for each value, name for time column is fixed
		Map<String,String> dataColumnNames = new HashMap<String,String>();
		i = 1;
		for (String dataIRI : ts.getDataIRI()) {
			dataColumnNames.put(dataIRI, "column"+i);
			i++;
		}
		
		// check if database exists and create it
		createDatabaseTable(create);
		populateDatabaseTable(create, tsTableName, ts.getDataIRI(), dataColumnNames);
		
		// create table for storing time series data
		initTimeSeriesTable(create, tsTableName, ts, dataColumnNames);
		
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
	
	public void createDatabaseTable(DSLContext create) {
		DataType<String> dataType = DefaultDataType.getDataType(dialect,String.class);
		create.createTableIfNotExists(dbTableName).column(dataIRIcolumn,dataType).column(tableRefName,dataType).column(columnRefName,dataType).execute();
	}
	
	public void populateDatabaseTable(DSLContext create, String tsTable, List<String> dataIRI, Map<String, String> dataColumnNames) {	
		InsertValuesStep3<?,Object,Object,Object> insertValueStep = create.insertInto(DSL.table(DSL.name(dbTableName)), DSL.field(DSL.name(dataIRIcolumn)), DSL.field(DSL.name(tableRefName)), DSL.field(DSL.name(columnRefName)));

		for (int i = 0; i < dataIRI.size(); i++) {
			insertValueStep = insertValueStep.values(dataIRI.get(i),tsTable,dataColumnNames.get(dataIRI.get(i)));
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
		List<String> dataIRIs = ts.getDataIRI();
		
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
    	
    	// add values into the newly created table
    	Table<?> table = DSL.table(DSL.name(tablename));
    	
    	Field<Object>[] columnArray = new Field[dataIRIs.size()+1];
    	columnArray[0] = timeColumn;
    	for (int i = 0; i < dataIRIs.size(); i++) {
    	    columnArray[i+1] = DSL.field(DSL.name(dataColumnNames.get(dataIRIs.get(i))));
    	}
    	
    	// column
        InsertValuesStepN<?> insertValueStep = create.insertInto(table, columnArray);
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
