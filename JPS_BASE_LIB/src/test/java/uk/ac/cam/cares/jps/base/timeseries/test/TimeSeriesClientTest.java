package uk.ac.cam.cares.jps.base.timeseries.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class TimeSeriesClientTest extends TestCase{
    public void testConnection() {
    	TimeSeriesClient.connect();
    }
    
    public void testCreateTable() {
    	String tablename = "table1";
    	TimeSeriesClient.createTable(tablename);
    }
    
    public void testDropTable() {
    	String tablename = "table1";
    	TimeSeriesClient.dropTable(tablename);
    }
    
    public void testInsertValues() {
    	String tablename = "table1";
    	
    	double[] values = {4,5,6};
    	List<Object> time = new ArrayList<>(); 
    	time.add("string");
    	time.add(123);
    }
    
    public void testClearTable() {
    	String tablename = "table1";
    	TimeSeriesClient.clearTable(tablename);
    }
    
    public void testDeleteRows() {
    	String tablename = "table1";
    	TimeSeriesClient.deleteRows(tablename, 2, 2);
    }
    
    public void testQueryTimeSeries() {
    	String tablename = "table1";
    	TimeSeriesClient.queryTimeSeries(tablename, 1, 3);
    }
}
