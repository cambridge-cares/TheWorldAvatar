package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.DataType;

import static org.jooq.impl.SQLDataType.*;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.Field;
import org.jooq.InsertValuesStepN;
import org.jooq.Record2;
import org.jooq.Result;
import org.jooq.impl.*;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TimeSeriesClientInterface;

public class TimeSeriesClient implements TimeSeriesClientInterface{
	private static final String url = "jdbc:postgresql:timeseries";
    private static final String user = "postgres";
    private static final String password = "postgres";
    private static final String dbTableName = "database";
    private static final String dataIRIColumn = "DataIRI";
    private static final String timeColumnName = "time";
    private static final Field<Object> timeColumn = DSL.field(timeColumnName);
    private static final SQLDialect dialect = SQLDialect.POSTGRES;
    
	public static Connection connect() {
		Connection conn = null;
		try {
        	Class.forName("org.postgresql.Driver");
        	conn = DriverManager.getConnection(url, user, password);
			return conn;
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		}
    }
	
	public static void createDatabaseTable() {
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect);
		DataType<String> dataType = DefaultDataType.getDataType(dialect,String.class);
		create.createTableIfNotExists(dbTableName).column(dataIRIColumn,dataType).column("Table",dataType).column("Column",dataType).execute();
	}
	
	public static void updateDatabaseTable(String[] dataIRIs) {
		if (tablename.length() > 63) {
			throw new JPSRuntimeException("Table name exceeded limit (63)");
		}
	}
	
	/**
	 * Creates an empty table with the given data types
	 * @param tablename
	 * @param timeClass
	 * @param valueClassList
	 */
	public static void createTimeSeriesTable(String tablename, Class<?> timeClass, List<Class<?>> valueClassList) {
		// maximum length for table name is 63 in postgresql
		if (tablename.length() > 63) {
			throw new JPSRuntimeException("Table name exceeded limit (63)");
		}
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect);    	
		CreateTableColumnStep createStep = create.createTable(tablename);
    	
    	// create time column
    	createStep = createStep.column(timeColumnName, DefaultDataType.getDataType(dialect,timeClass));
    	
    	// create 1 column for each value
    	for (int i=0; i<valueClassList.size(); i++) {
    		String columnname = "value" + i;
    		createStep = createStep.column(columnname, DefaultDataType.getDataType(dialect,valueClassList.get(i)));
    	}

    	// send request to db
    	createStep.execute();
	}
	
	public static void insertValues(String tablename, TimeSeries ts) {
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
		InsertValuesStepN<?> insertValueStep = create.insertInto(table, columnArray);
		for (int i=0; i<ts.getTime().size(); i++) {
			List<Object> newValues = new ArrayList<>();
			newValues.add(ts.getTime().get(i)); 
			
			for (int j=0; j<2; j++) {
				newValues.add(ts.getValues().get(j).get(i));
			}
			insertValueStep = insertValueStep.values(newValues);
		}
		insertValueStep.execute();
	}
	
	public static void queryTimeSeries(String tablename, long lowerBound, long upperBound) {
		Connection conn = connect();
		Table<?> table = DSL.table(tablename);
		Field<Object> valueColumn = DSL.field("value");
		DSLContext create = DSL.using(conn, dialect);
		Result<Record2<Object, Object>> result = create.select(timeColumn, valueColumn).from(table).where(timeColumn.between(lowerBound, upperBound)).fetch();
	    
		List<Long> timeList = result.getValues(timeColumn).stream().map(t->(Long)t).collect(Collectors.toList()); 
		List<Double> valueList = result.getValues(valueColumn).stream().map(t->(Double)t).collect(Collectors.toList()); 
//		TimeSeries ts = new TimeSeries(timeList,valueList);
	}
	
	public static void deleteRows(String tablename, long lowerBound, long upperBound) {
		Connection conn = connect();
		Table<?> table = DSL.table(tablename);
		DSLContext create = DSL.using(conn, dialect);
		Field<Object> timeColumn = DSL.field("time");
		create.delete(table).where(timeColumn.between(lowerBound, upperBound)).execute();
	}
	
	public static void clearTable(String tablename) {
		Connection conn = connect();
		Table<?> table = DSL.table(tablename);
		DSLContext create = DSL.using(conn, dialect);
		create.delete(table).execute();
	}
	
	public static void dropTable(String tablename) {
		Connection conn = connect();
		DSLContext create = DSL.using(conn, dialect);
		create.dropTable(tablename).execute();
	}
}
