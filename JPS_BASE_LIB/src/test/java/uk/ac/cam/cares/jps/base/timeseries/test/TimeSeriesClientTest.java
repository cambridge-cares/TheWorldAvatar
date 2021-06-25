package uk.ac.cam.cares.jps.base.timeseries.test;

import java.lang.reflect.Field;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import org.jooq.impl.DSL;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.FileBasedKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;

public class TimeSeriesClientTest extends TestCase{
	String dbURL = "jdbc:postgresql:timeseries";
	String user = "postgres";
	String password = "postgres";
	
	/**
	 * example code on how to use the TimeSeriesClient
	 */
	public void testExample() {
//		// set up a kb client that points to the location of your instance
//		// this can be the a RemoteKnowledgeBaseClient or the FileBasedKnowledgeBaseClient
//		String endpoint = "http://localhost:8080/blazegraph/namespace/timeseries/sparql";
//    	RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
//    	kbClient.setUpdateEndpoint(endpoint);
//    	kbClient.setQueryEndpoint(endpoint);
//    	
//    	// set up TimeSeriesRDBClient to interact with RDB (PostgreSQL)
//    	// you must specify the class of the time values, this example uses the Instant class
//    	// you can use classes such as LocalDateTime, Timestamp, Integer, Double, etc.
//    	// once you initialise it with a certain class, you should stick to it
//    	// if the class is not supported, the Jooq API should throw an exception
//    	TimeSeriesRDBClient<Instant> tsClient = new TimeSeriesRDBClient<>(Instant.class);
//    	tsClient.setRdbURL(dbURL);
//    	tsClient.setRdbUser(user);
//    	tsClient.setRdbPassword(password);
//    	tsClient.setKBClient(kbClient);
//    	
//    	// next step is to initialise the time series instance in RDF and RDB
//    	// in this example I have three instances that share the same timestamp, so they are initialised together
//    	List<String> dataIRI = List.of("http://data1", "http://data2", "http://data3");
//    	// next specify the type of data for each column, I expect most data will be in doubles, but you can specify
//    	// different data types if you wish
//    	List<Class<?>> dataClass = List.of(Double.class,String.class,Integer.class);
//    	
//    	// calling init will link the provided IRIs to a time series instance in your knowledge graph that points to postgres
//    	// at the same time, the tables will be created in RDB according to the class specified
//    	tsClient.init(dataIRI, dataClass);
//    	
//    	// to add data, you need to create a TimeSeries object
//    	List<Instant> timeList = new ArrayList<>();
//    	List<Double> data1 = new ArrayList<>();
//    	List<String> data2 = new ArrayList<>();
//    	List<Integer> data3 = new ArrayList<>();
//    	
//    	for (int i = 0; i < 10; i++) {
//    		timeList.add(Instant.now().plusSeconds(i));
//    		data1.add(Double.valueOf(i));
//    		data2.add(String.valueOf(i));
//    		data3.add(Integer.valueOf(i));
//    	}
//    	List<List<?>> dataToAdd = List.of(data1,data2,data3);
//    	// the constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
//    	TimeSeries<Instant> tsToAdd = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
//    	
//    	// supply the TimeSeries object as an argument to the Time series client
//    	tsClient.addTimeSeries(tsToAdd);
//    	
//    	// you can query the entire TimeSeries table by providing the data IRIs
//    	TimeSeries<Instant> ts1 = tsClient.getTimeSeries(dataIRI);
//    	// or you can specify the time range you want
//    	TimeSeries<Instant> ts2 = tsClient.getTimeSeriesWithinBounds(dataIRI,timeList.get(2),timeList.get(4));
//    	
//    	// the time column
//    	List<Instant> timeColumn = ts1.getTimes();
//    	
//    	// you can then obtain the values associated with a data IRI by
//    	List<Double> column1 = ts1.getValuesAsDouble(dataIRI.get(0));
//    	List<String> column2 = ts1.getValuesAsString(dataIRI.get(1));
//    	List<Integer> column3 = ts1.getValuesAsInteger(dataIRI.get(2));
//    	
//    	// getting the maximum time, providing any data IRI within the set will yield the same answer
//    	Instant maxTime = tsClient.getMaxTime(dataIRI.get(0));
//    	
//    	// similarly for min time
//    	Instant minTime = tsClient.getMinTime(dataIRI.get(1));
//    	
//    	// currently we have min/max/average, contact KFL if you need additional properties
//    	double average = tsClient.getAverage(dataIRI.get(0));
//    	double maxValue = tsClient.getMaxValue(dataIRI.get(0));
//    	double minValue = tsClient.getMinValue(dataIRI.get(0));
//    	
//    	// you can remove specific rows from your table by specifying the time range
//    	tsClient.deleteRows(dataIRI.get(0), timeList.get(2), timeList.get(4));
//    	
//    	// or delete all time series related data, this will remove the data in the same table, even if it's not included
//    	tsClient.deleteTimeSeries(dataIRI.get(0));
	}
	
	public void testConstructorAndSetters() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// test constructor
		Class<?> timeClass = Instant.class;
		TimeSeriesRDBClient<Instant> tsClient = new TimeSeriesRDBClient<>(Instant.class);
		Field timeColumnField = tsClient.getClass().getDeclaredField("timeColumn");
		timeColumnField.setAccessible(true);
		// this only compares the query string - "time", the class is not checked here
		assertNotNull(timeColumnField.get(tsClient));
		assertEquals(timeColumnField.get(tsClient), DSL.field(DSL.name("time"),timeClass));
		
		// check that kbClient is set correctly
		RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
		Field kbClientField = tsClient.getClass().getDeclaredField("kbClient");
		kbClientField.setAccessible(true);
		assertNull(kbClientField.get(tsClient));
		tsClient.setKBClient(kbClient);
		assertNotNull(kbClientField.get(tsClient));
		assertEquals(kbClientField.get(tsClient), kbClient);
		
		FileBasedKnowledgeBaseClient kbClient2 = new FileBasedKnowledgeBaseClient();
		tsClient.setKBClient(kbClient2);
		assertEquals(kbClientField.get(tsClient), kbClient2);
		
		// rdb password
		Field rdbPasswordField = tsClient.getClass().getDeclaredField("rdbPassword");
		rdbPasswordField.setAccessible(true);
		assertNull(rdbPasswordField.get(tsClient));
		tsClient.setRdbPassword(password);
		assertNotNull(rdbPasswordField.get(tsClient));
		assertEquals(rdbPasswordField.get(tsClient), password);
		
		// rdb user
		Field rdbUserField = tsClient.getClass().getDeclaredField("rdbUser");
		rdbUserField.setAccessible(true);
		assertNull(rdbUserField.get(tsClient));
		tsClient.setRdbUser(user);
		assertNotNull(rdbUserField.get(tsClient));
		assertEquals(rdbUserField.get(tsClient), user);
		
		// rdb url
		Field rdbUrlField = tsClient.getClass().getDeclaredField("rdbURL");
		rdbUrlField.setAccessible(true);
		assertNull(rdbUrlField.get(tsClient));
		tsClient.setRdbURL(dbURL);
		assertNotNull(rdbUrlField.get(tsClient));
		assertEquals(rdbUrlField.get(tsClient),dbURL);
		
		// time unit
		Field timeUnitField = tsClient.getClass().getDeclaredField("timeUnit");
		timeUnitField.setAccessible(true);
		assertNull(timeUnitField.get(tsClient));
		tsClient.setTimeUnit("http://s");
		assertNotNull(timeUnitField.get(tsClient));
		assertEquals(timeUnitField.get(tsClient),"http://s");
	}
}
