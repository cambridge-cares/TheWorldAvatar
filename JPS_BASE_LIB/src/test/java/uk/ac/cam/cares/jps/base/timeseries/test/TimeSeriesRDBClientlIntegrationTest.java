package uk.ac.cam.cares.jps.base.timeseries.test;

import java.lang.reflect.Field;
import java.time.Instant;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

import org.jooq.impl.DSL;

import org.junit.Assert;
import org.junit.Test;
import org.junit.Ignore;
import org.junit.Before;
import org.junit.After;
import org.junit.BeforeClass;

import uk.ac.cam.cares.jps.base.query.FileBasedKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

@Ignore("Work in progress")
public class TimeSeriesRDBClientlIntegrationTest {
	
	// Define RDB database setup (analogous to a triple-store endpoint)
	// Database named "timeseries" needs to be created beforehand
	// For more info (especially regarding URL setting) see https://jdbc.postgresql.org/documentation/80/connect.html
	private static String dbURL = "jdbc:postgresql:timeseries"; 
	private static String user = "postgres";
	private static String password = "postgres";
	
	// Time series attributes
	private static List<Instant> timeList;
	private static String tsIRI = "http://tsIRI1";
	private static String timeUnit = "http://s";
	private static List<String> dataIRI;
	private static List<Class<?>> dataClass;
	
	
	public static TimeSeriesRDBClient<Instant> initialiseRDBClient() {
    	// Set up TimeSeriesRDBClient to interact with RDB (PostgreSQL)
    	// One must specify the class of the time values, these tests uses the Instant class
    	// One can use classes such as LocalDateTime, Timestamp, Integer, Double, etc.
    	// Once you initialise it with a certain class, you should stick to it
    	// If the class is not supported, the Jooq API should throw an exception
    	TimeSeriesRDBClient<Instant> rdbClient = new TimeSeriesRDBClient<>(Instant.class);
    	rdbClient.setRdbURL(dbURL);
    	rdbClient.setRdbUser(user);
    	rdbClient.setRdbPassword(password);
			
		return rdbClient;
	}
	
	public static TimeSeries<Instant> initialiseTimeSeries(int length) {		
    	
		// Initialise 1 time series with 3 associated data series			
		dataIRI = new ArrayList<>();
    	dataIRI.add("http://data1"); dataIRI.add("http://data2"); dataIRI.add("http://data3"); 
    	// Specify type of data for each column (most data will be in doubles, but one can specify different data types)
    	dataClass = new ArrayList<>();
    	dataClass.add(Double.class); dataClass.add(String.class); dataClass.add(Integer.class);
    	timeList = new ArrayList<>();
    	List<Double> data1 = new ArrayList<>();
    	List<String> data2 = new ArrayList<>();
    	List<Integer> data3 = new ArrayList<>();
    	
    	// Add time steps and data
    	for (int i = 0; i < length; i++) {
   			timeList.add(Instant.now().plusSeconds(i));
    		data1.add(Double.valueOf(i));
    		data2.add(String.valueOf(i));
    		data3.add(Integer.valueOf(i));
    	}
    	
    	List<List<?>> dataToAdd = new ArrayList<>();
    	dataToAdd.add(data1); dataToAdd.add(data2); dataToAdd.add(data3);
    	// To add data to the RDB, one needs to create a TimeSeries object
    	// Constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
    	TimeSeries<Instant> tsToAdd = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
    	
    	return tsToAdd;
	}
	
	@BeforeClass
	@After
	public void clearRDB() {
		TimeSeriesRDBClient<Instant> rdbClient = initialiseRDBClient();
		// Clear entire relational database
		rdbClient.deleteAll();
	}
	
	@Test
	public void testInitCentralTable() {
		TimeSeriesRDBClient<Instant> rdbClient = initialiseRDBClient();
		// Initialise central lookup table
		rdbClient.initCentralTable();
		// mh807: How / what to assert best?

	}
	
}

