package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.Connection;
import java.sql.DriverManager;
import java.util.List;
import java.util.stream.Collectors;

import org.jooq.DSLContext;

import static org.jooq.impl.SQLDataType.*;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.Field;
import org.jooq.Record2;
import org.jooq.Result;
import org.jooq.impl.*;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TimeSeriesClientInterface;

public class TimeSeriesClient implements TimeSeriesClientInterface{
	private static final String url = "jdbc:postgresql:timeseries";
    private static final String user = "postgres";
    private static final String password = "postgres";
    private static final Field<Object> timeColumn = DSL.field("time");
	private static final Field<Object> valueColumn = DSL.field("value");
    
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
	
	public static void createTable(String tablename) {
		// maximum length for table name is 63 in postgresql
		if (tablename.length() > 63) {
			throw new JPSRuntimeException("Table name exceeds limit (63)");
		}
		Connection conn = connect();
		DSLContext create = DSL.using(conn, SQLDialect.POSTGRES);
		create.createTable(tablename).column("time", BIGINT).column("value", DOUBLE).execute();
	}
	
	public static void insertValues(String tablename, TimeSeries ts) {
		// check time and value have the same length
		if (ts.getTime().size() != ts.getValues().size()) {
			throw new JPSRuntimeException("Array size of time and values are not the same");
		}
		
		Connection conn = connect();
		DSLContext create = DSL.using(conn, SQLDialect.POSTGRES);
		
		Table<?> table = DSL.table(tablename);
		
		for (int i=0; i<ts.getTime().size(); i++) {
			create.insertInto(table, timeColumn, valueColumn).values(ts.getTime(),ts.getValues()).execute();
		}
	}
	
	public static void queryTimeSeries(String tablename, long lowerBound, long upperBound) {
		Connection conn = connect();
		Table<?> table = DSL.table(tablename);
		DSLContext create = DSL.using(conn, SQLDialect.POSTGRES);
		Result<Record2<Object, Object>> result = create.select(timeColumn, valueColumn).from(table).where(timeColumn.between(lowerBound, upperBound)).fetch();
	    
		List<Long> timeList = result.getValues(timeColumn).stream().map(t->(Long)t).collect(Collectors.toList()); 
		List<Double> valueList = result.getValues(valueColumn).stream().map(t->(Double)t).collect(Collectors.toList()); 
		TimeSeries ts = new TimeSeries(timeList,valueList);
	}
	
	public static void deleteRows(String tablename, long lowerBound, long upperBound) {
		Connection conn = connect();
		Table<?> table = DSL.table(tablename);
		DSLContext create = DSL.using(conn, SQLDialect.POSTGRES);
		Field<Object> timeColumn = DSL.field("time");
		create.delete(table).where(timeColumn.between(lowerBound, upperBound)).execute();
	}
	
	public static void clearTable(String tablename) {
		Connection conn = connect();
		Table<?> table = DSL.table(tablename);
		DSLContext create = DSL.using(conn, SQLDialect.POSTGRES);
		create.delete(table).execute();
	}
	
	public static void dropTable(String tablename) {
		Connection conn = connect();
		DSLContext create = DSL.using(conn, SQLDialect.POSTGRES);
		create.dropTable(tablename).execute();
	}
}
