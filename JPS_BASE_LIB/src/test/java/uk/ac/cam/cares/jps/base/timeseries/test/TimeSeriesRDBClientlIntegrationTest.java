package uk.ac.cam.cares.jps.base.timeseries.test;


import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.OptionalDouble;

import org.junit.*;
import org.jooq.DSLContext;
import org.jooq.Record;
import org.jooq.Result;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;
import static org.jooq.impl.DSL.selectFrom;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;


@Ignore("Requires postgreSQL database set up and running (using testcontainers)\n" + 
		"Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
public class TimeSeriesRDBClientlIntegrationTest {
	
	// Define RDB database setup (analogous to a triple-store endpoint)
	// Using special testcontainers URL that will spin up a Postgres Docker container when accessed by a driver
	// (see: https://www.testcontainers.org/modules/databases/jdbc/). Note: requires Docker to be installed!
	private static final String dbURL = "jdbc:tc:postgresql:13.3:///timeseries";
	// For easier local debugging, use the following dbURL instead of the testcontainer dbURL
	// NOTE: Requires local postgreSQL database "timeseries" to be set up beforehand
	//private static final String dbURL = "jdbc:postgresql:timeseries";
	private static final String user = "postgres";
	private static final String password = "postgres";
	
	private static Connection conn;
	private static DSLContext context;

	// RDB client
	private TimeSeriesRDBClient<Instant> client;

	// Time series test data
	private static String tsIRI_1, tsIRI_3;
	private static List<String> dataIRI_1, dataIRI_3;
	private static List<Class<?>> dataClass_1, dataClass_3;
	private static List<Instant> timeList_1;
	private static List<Instant> timeList_2;
	private static List<Double> data1_1;
	private static List<String> data2_1;
	private static List<Integer> data3_1;
	private static TimeSeries<Instant> ts1, ts2, ts3;	
	private static List<List<?>> dataToAdd_1;
	private static List<List<?>> dataToAdd_2;

	@BeforeClass
	// Connect to the database before any test (will spin up the Docker container for the database)
	public static void connect() throws SQLException, ClassNotFoundException {
		// Load required driver
		Class.forName("org.postgresql.Driver");
		// Connect to DB
		conn = DriverManager.getConnection(dbURL, user, password);
		context = DSL.using(conn, SQLDialect.POSTGRES);
		// Clear database
		List<Table<?>> tables = context.meta().getTables();
		for (Table<?> table: tables) {
			context.dropTable(table).cascade().execute();
		}
	}
	
	@BeforeClass
	// Initialise 3 test time series data sets
	public static void initialiseData() {
		/* 
		 * Initialise 1st time series with 3 associated data series
		 */
	    tsIRI_1 = "http://tsIRI1";
	    dataIRI_1 = new ArrayList<>();
	    dataIRI_1.add("http://data1"); dataIRI_1.add("http://data2"); dataIRI_1.add("http://data3");
		// Specify type of data for each column (most data will be in doubles, but one can specify different data types)
		dataClass_1 = new ArrayList<>();
		dataClass_1.add(Double.class); dataClass_1.add(String.class); dataClass_1.add(Integer.class);
    	// Create data to add (as a TimeSeries object)
    	timeList_1 = new ArrayList<>();
    	data1_1 = new ArrayList<>();
    	data2_1 = new ArrayList<>();
    	data3_1 = new ArrayList<>();
    	
    	for (int i = 0; i < 10; i++) {
   			timeList_1.add(Instant.now().plusSeconds(i));
    		data1_1.add((double) i);
    		data2_1.add(String.valueOf(i));
    		data3_1.add(i);
    	}
    	dataToAdd_1 = new ArrayList<>();
    	dataToAdd_1.add(data1_1); dataToAdd_1.add(data2_1); dataToAdd_1.add(data3_1);
    	// Constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
    	ts1 = new TimeSeries<>(timeList_1, dataIRI_1, dataToAdd_1);
		/* 
		 * Initialise 2nd time series with same associated data series
		 */
    	// Create data to add (as a TimeSeries object)
    	timeList_2 = new ArrayList<>();
		List<Double> data1_2 = new ArrayList<>();
		List<String> data2_2 = new ArrayList<>();
		List<Integer> data3_2 = new ArrayList<>();
    	
    	for (int i = 0; i < 10; i++) {
    		// Add additional 10 s to ensure no overlap between time lists
   			timeList_2.add(Instant.now().plusSeconds(10+i));
    		data1_2.add((double) (10 + i));
    		data2_2.add(String.valueOf(10+i));
    		data3_2.add(10 + i);
    	}
    	dataToAdd_2  = new ArrayList<>();
    	dataToAdd_2.add(data1_2); dataToAdd_2.add(data2_2); dataToAdd_2.add(data3_2);
    	// Constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
    	ts2 = new TimeSeries<>(timeList_2, dataIRI_1, dataToAdd_2);
		/* 
		 * Initialise 3rd time series with only one associated data series
		 */
	    tsIRI_3 = "http://tsIRI2";
	    dataIRI_3 = new ArrayList<>();
		dataIRI_3.add("http://data4");
		// Specify type of data for each column (most data will be in doubles, but one can specify different data types)
		dataClass_3 = new ArrayList<>();
		dataClass_3.add(Double.class);
    	// Create data to add (as a TimeSeries object)
		List<Instant> timeList_3 = new ArrayList<>();
		List<Double> data1_3 = new ArrayList<>();

    	for (int i = 0; i < 10; i++) {
   			timeList_3.add(Instant.now().plusSeconds(i));
    		data1_3.add((double) i);
    	}
		List<List<?>> dataToAdd_3 = new ArrayList<>();
    	dataToAdd_3.add(data1_3);
    	// Constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
    	ts3 = new TimeSeries<>(timeList_3, dataIRI_3, dataToAdd_3);
	}

	@AfterClass
	// Disconnect from the database after all tests are run
	public static void disconnect() throws SQLException {
		conn.close();
	}

	@Before
	public void initialiseRDBClient() {
    	// Set up TimeSeriesRDBClient to interact with RDB (PostgreSQL)
    	// One must specify the class of the time values, these tests uses the Instant class
    	// One can use classes such as LocalDateTime, Timestamp, Integer, Double, etc.
    	// Once you initialise it with a certain class, you should stick to it
    	// If the class is not supported, the Jooq API should throw an exception
    	client = new TimeSeriesRDBClient<>(Instant.class);
    	client.setRdbURL(dbURL);
		client.setRdbUser(user);
		client.setRdbPassword(password);
	}	

	@After
	// Clear all tables after each test to ensure clean slate
	public void clearDatabase() {
		List<Table<?>> tables = context.meta().getTables();
		for (Table<?> table: tables) {
			context.dropTable(table).cascade().execute();
		}
	}

	@Test
	public void testInitCentralTable() throws NoSuchFieldException, IllegalAccessException {
		// Retrieve the value of the private field 'dbTableName' of the client to check its value
		Field tableNameField = client.getClass().getDeclaredField("dbTableName");
		tableNameField.setAccessible(true);
		String tableName = (String) tableNameField.get(client);
		
		// Check that no central table exists
		Assert.assertEquals(0, context.meta().getTables(tableName).size());
		// Initialise arbitrary time series table (initCentralTable shall be created internally)
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);	
		// Check that central table was created
		Assert.assertEquals(1, context.meta().getTables(tableName).size());
		// Check that central table has four columns (timeseries IRI, timseries table, data IRI, column name)
		Assert.assertEquals(4, context.meta().getTables(tableName).get(0).fields().length);
		// Initialise another arbitrary time series table
		client.initTimeSeriesTable(dataIRI_3, dataClass_3, tsIRI_3);	
		// Check that central table was created
		Assert.assertEquals(1, context.meta().getTables(tableName).size());
	}
	
	@Test
	public void testInitExceptions() {
		// Check exception for wrong dataClass size
		try {
			client.initTimeSeriesTable(dataIRI_1, dataClass_3, tsIRI_1);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: Length of dataClass is different from number of data IRIs",
								e.getMessage());
		}
		// Check exception for already initialised dataIRIs
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);
		try {
			client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_3);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: <" + dataIRI_1.get(0) + "> already has a time series instance (i.e. tsIRI)",
								e.getMessage());
		}
	}
	
	@Test
	public void testInitTimeSeriesTable() throws NoSuchFieldException, IllegalAccessException {
		// Initialise 1st time series
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);
		// Check that timeseries table was created in addition to central table
		Assert.assertEquals(2, context.meta().getTables().size());

		// Retrieve the value of the private field 'dbTableName' of the client to check its value
		Field tableNameField = client.getClass().getDeclaredField("dbTableName");
		tableNameField.setAccessible(true);
		String tableName = (String) tableNameField.get(client);
		Table<?> table = context.meta().getTables(tableName).get(0);
		// Retrieve the value of the private field 'dataIRIcolumn' of the client
		Field dataIRIcolumnField = client.getClass().getDeclaredField("dataIRIcolumn");
		dataIRIcolumnField.setAccessible(true);
		org.jooq.Field<String> dataIRIcolumn = (org.jooq.Field<String>) dataIRIcolumnField.get(client);
		// Retrieve the value of the private field 'tsIRIcolumn' of the client
		Field tsIRIcolumnField = client.getClass().getDeclaredField("tsIRIcolumn");
		tsIRIcolumnField.setAccessible(true);
		org.jooq.Field<String> tsIRIcolumn = (org.jooq.Field<String>) tsIRIcolumnField.get(client);
		// Retrieve the value of the private field 'tsTableNameColumn' of the client to check its value
		Field tsTableNameColumnField = client.getClass().getDeclaredField("tsTableNameColumn");
		tsTableNameColumnField.setAccessible(true);
		org.jooq.Field<String> tsTableNameColumn = (org.jooq.Field<String>) tsTableNameColumnField.get(client);

		// Check that there is a row for each data IRI in the central table
		for (String iri: dataIRI_1) {
			Assert.assertTrue(context.fetchExists(selectFrom(table).where(dataIRIcolumn.eq(iri))));
		}
		// Check that all data IRIs are connected to same timeseries IRI and table name
		List<String> queryResult = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI_1.get(0))).fetch(tsTableNameColumn);
		String tsTableName = queryResult.get(0);
		queryResult = context.select(tsIRIcolumn).from(table).where(dataIRIcolumn.eq(dataIRI_1.get(0))).fetch(tsIRIcolumn);
		String tsIRI = queryResult.get(0);
		// Verify correct time series IRI
		Assert.assertEquals(tsIRI, tsIRI_1);
		for (String iri : dataIRI_1) {
			String curTableName = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(iri)).fetch(tsTableNameColumn).get(0);
			String curTsIRI = context.select(tsIRIcolumn).from(table).where(dataIRIcolumn.eq(iri)).fetch(tsIRIcolumn).get(0);
			Assert.assertEquals(tsTableName, curTableName);
			Assert.assertEquals(tsIRI, curTsIRI);
		}
		
		// Initialise 2nd time series
		client.initTimeSeriesTable(dataIRI_3, dataClass_3, tsIRI_3);
		List<String> dataIRIs = new ArrayList<>();
		dataIRIs.addAll(dataIRI_1);
		dataIRIs.addAll(dataIRI_3);
		// Check that there is (still) a row for each data IRI in the central table
		for (String iri: dataIRIs) {
			Assert.assertTrue(context.fetchExists(selectFrom(table).where(dataIRIcolumn.eq(iri))));
		}
	}
	
	@Test
	public void testAddTimeseriesData() throws NoSuchFieldException, IllegalAccessException {
		// Initialise time series table
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);	
		// Add time series data
		client.addTimeSeriesData(ts1);
		
		// Retrieve the value of the private field 'dbTableName' of the client to check its value
		Field tableNameField = client.getClass().getDeclaredField("dbTableName");
		tableNameField.setAccessible(true);
		String tableName = (String) tableNameField.get(client);
		Table<?> table = context.meta().getTables(tableName).get(0);
		// Retrieve the value of the private field 'dataIRIcolumn' of the client
		Field dataIRIcolumnField = client.getClass().getDeclaredField("dataIRIcolumn");
		dataIRIcolumnField.setAccessible(true);
		org.jooq.Field<String> dataIRIcolumn = (org.jooq.Field<String>) dataIRIcolumnField.get(client);
		// Retrieve the value of the private field 'tsTableNameColumn' of the client to check its value
		Field tsTableNameColumnField = client.getClass().getDeclaredField("tsTableNameColumn");
		tsTableNameColumnField.setAccessible(true);
		org.jooq.Field<String> tsTableNameColumn = (org.jooq.Field<String>) tsTableNameColumnField.get(client);
		// Retrieve the value of the private field 'tsTableNameColumn' of the client to check its value
		Field columnNameColumnField = client.getClass().getDeclaredField("columnNameColumn");
		columnNameColumnField.setAccessible(true);
		org.jooq.Field<String> columnNameColumn = (org.jooq.Field<String>) columnNameColumnField.get(client);
		
		// Check correct data of time series table columns
		Result<? extends Record> res;
		String tstable = null;
		String tscolumn = null;
		for (int i=0; i < dataIRI_1.size(); i++) {
			tstable = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI_1.get(i))).fetch(tsTableNameColumn).get(0);
			tscolumn = context.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI_1.get(i))).fetch(columnNameColumn).get(0);
			// Perform query for data columns
			res = context.select(DSL.field(DSL.name(tscolumn))).from(DSL.table(DSL.name(tstable))).fetch();
			// Check data types
			Assert.assertEquals(dataClass_1.get(i), res.getValues(tscolumn).get(0).getClass());
			// Check array content
			Assert.assertEquals(ts1.getValues(dataIRI_1.get(i)), res.getValues(tscolumn));
		}
		
		// Add additional data and check whether it has been appended correctly
		client.addTimeSeriesData(ts2);
		List<?> combinedList;
		for (int i=0; i < dataIRI_1.size(); i++) {
			tstable = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI_1.get(i))).fetch(tsTableNameColumn).get(0);
			tscolumn = context.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI_1.get(i))).fetch(columnNameColumn).get(0);
			// Perform query for data columns
			res = context.select(DSL.field(DSL.name(tscolumn))).from(DSL.table(DSL.name(tstable))).fetch();
			// Check array content
			combinedList = new ArrayList<>();
			combinedList.addAll((List) dataToAdd_1.get(i));
			combinedList.addAll((List) dataToAdd_2.get(i));
			Assert.assertEquals(combinedList, res.getValues(tscolumn));
		}
		
	}
	
	@Test
	public void testAddTimeseriesDataExceptions() {
		try {
			// Add time series data for non-initialised time series and central table
			client.addTimeSeriesData(ts1);
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: Central RDB lookup table has not been initialised yet",
								e.getMessage());
		}
		try {
			// Add time series data for non-initialised time series
			client.initTimeSeriesTable(dataIRI_3, dataClass_3, tsIRI_3);	
			client.addTimeSeriesData(ts1);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			String s = ts1.getDataIRIs().get(0);
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: <" + s + "> does not have a time series instance (i.e. tsIRI)",
								e.getMessage());
		}
		try {
			// Add time series data which is not in same table
			client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);
			List<String> dataIRIs = new ArrayList<>();
			dataIRIs.addAll(dataIRI_1);
			dataIRIs.addAll(dataIRI_3);
			List<List<?>> dataToAdd = new ArrayList<>();
	    	dataToAdd.add(data1_1); dataToAdd.add(data2_1); dataToAdd.add(data3_1); dataToAdd.add(data3_1);
			TimeSeries<Instant> ts = new TimeSeries<>(timeList_1, dataIRIs, dataToAdd);
			client.addTimeSeriesData(ts);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: Provided data is not within the same RDB table",
								e.getMessage());
		}
	}
	
	@Test
	public void testGetTimeseries() {
		// Initialise time series table
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);	
		// Add time series data
		client.addTimeSeriesData(ts1);
		List<String> iris = new ArrayList<>();
		// Check for time series with only one data IRI
		iris.add(dataIRI_1.get(0));
		TimeSeries<Instant> ts = client.getTimeSeries(iris);
		Assert.assertEquals(timeList_1, ts.getTimes());
		Assert.assertEquals(dataIRI_1.get(0), ts.getDataIRIs().get(0));
		Assert.assertEquals(data1_1, ts.getValues(ts.getDataIRIs().get(0)));	
		// Check for time series with multiple data IRIs
		ts = client.getTimeSeries(dataIRI_1);
		Assert.assertEquals(timeList_1, ts.getTimes());
		for (int i=0; i < dataIRI_1.size(); i++) {
			String iri = dataIRI_1.get(i);
			Assert.assertTrue(ts.getDataIRIs().contains(iri));
			Assert.assertEquals(dataToAdd_1.get(i), ts.getValues(iri));	
		}
	}
	
	@Test
	public void testGetTimeseriesExceptions() {
		try {
			// Add time series data for non-initialised time series and central table
			client.getTimeSeries(dataIRI_1);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: Central RDB lookup table has not been initialised yet",
								e.getMessage());
		}
		try {
			// Add time series data for non-initialised time series
			client.initTimeSeriesTable(dataIRI_3, dataClass_3, tsIRI_3);	
			client.getTimeSeries(dataIRI_1);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertTrue(e.getMessage().contains("> does not have a time series instance (i.e. tsIRI)"));
		}
		try {
			// Add time series data which is not in same table
			client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);
			List<String> dataIRIs = new ArrayList<>();
			dataIRIs.addAll(dataIRI_1);
			dataIRIs.addAll(dataIRI_3);
			client.getTimeSeries(dataIRIs);
			Assert.fail();
		} catch (JPSRuntimeException e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: Provided data is not within the same RDB table",
								e.getMessage());
		}
	}
	
	@Test
	public void testGetTimeseriesWithinBounds() throws NoSuchFieldException, IllegalAccessException {
		// Initialise time series table
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);	
		// Add time series data
		client.addTimeSeriesData(ts1);
		// Check for time series with only one data IRI
		List<String> iris = dataIRI_1.subList(0, 1);
		// Test bounds within range
		Instant lb = ts1.getTimes().get(1);
		Instant ub = ts1.getTimes().get(ts1.getTimes().size()-2);				
		TimeSeries<Instant> ts = client.getTimeSeriesWithinBounds(iris, lb, ub);
		Assert.assertEquals(ts1.getTimes().subList(1, ts1.getTimes().size()-1),
							ts.getTimes());
		Assert.assertEquals(ts1.getValues(iris.get(0)).subList(1, ts1.getTimes().size()-1),
							ts.getValues(iris.get(0)));
		// Test for only lower bound
		ts = client.getTimeSeriesWithinBounds(iris, lb, null);
		Assert.assertEquals(ts1.getTimes().subList(1, ts1.getTimes().size()),
							ts.getTimes());
		Assert.assertEquals(ts1.getValues(iris.get(0)).subList(1, ts1.getTimes().size()),
							ts.getValues(iris.get(0)));
		// Test for only upper bound
		ts = client.getTimeSeriesWithinBounds(iris, null, ub);
		Assert.assertEquals(ts1.getTimes().subList(0, ts1.getTimes().size()-1),
							ts.getTimes());
		Assert.assertEquals(ts1.getValues(iris.get(0)).subList(0, ts1.getTimes().size()-1),
							ts.getValues(iris.get(0)));
		// Test for upper bound out of range (ts2 has time stamps after ts1)
		ub = ts2.getTimes().get(ts2.getTimes().size()-1);	
		ts = client.getTimeSeriesWithinBounds(iris, null, ub);
		Assert.assertEquals(ts1.getTimes().subList(0, ts1.getTimes().size()),
							ts.getTimes());
		Assert.assertEquals(ts1.getValues(iris.get(0)).subList(0, ts1.getTimes().size()),
							ts.getValues(iris.get(0)));
		// Test for lower bound out of range (ts2 has time stamps after ts1)
		client.deleteAll();
		// Retrieve the value of the private field 'dbTableName' of the client to check its value
		Field tableNameField = client.getClass().getDeclaredField("dbTableName");
		tableNameField.setAccessible(true);
		String tableName = (String) tableNameField.get(client);
		// Verify all tables are deleted
		Assert.assertEquals(0, context.meta().getTables(tableName).size());
		// Initialise time series table
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);	
		// Add time series data
		client.addTimeSeriesData(ts2);
		lb = ts1.getTimes().get(0);	
		ts = client.getTimeSeriesWithinBounds(iris, lb, null);
		Assert.assertEquals(ts2.getTimes().subList(0, ts2.getTimes().size()),
							ts.getTimes());
		Assert.assertEquals(ts2.getValues(iris.get(0)).subList(0, ts2.getTimes().size()),
							ts.getValues(iris.get(0)));		
	}
	
	@Test
	public void testGetAggregates() {
		// Initialise time series table
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);	
		// Add time series data
		client.addTimeSeriesData(ts1);
		
		// Check for only one time series (with numerics data content)
		String iri = dataIRI_1.get(0);
		
		// Test average
		OptionalDouble ave = data1_1.stream().mapToDouble(a -> a).average();
		double ave_exp = ave.isPresent() ? ave.getAsDouble() : Double.POSITIVE_INFINITY;
		double ave_act = client.getAverage(iri);
		Assert.assertEquals(ave_exp, ave_act, 1e-6);
		
		// Test minimum
		OptionalDouble min = data1_1.stream().mapToDouble(a -> a).min();
		double min_exp = min.isPresent() ? min.getAsDouble() : Double.POSITIVE_INFINITY;
		double min_act = client.getMinValue(iri);
		Assert.assertEquals(min_exp, min_act, 1e-6);
	
		// Test maximum
		OptionalDouble max = data1_1.stream().mapToDouble(a -> a).max();
		double max_exp = max.isPresent() ? max.getAsDouble() : Double.NEGATIVE_INFINITY;
		double max_act = client.getMaxValue(iri);
		Assert.assertEquals(max_exp, max_act, 1e-6);
		
		// Test Exception for non numerics data
		iri = dataIRI_1.get(1);
		try {
			client.getAverage(iri);
			Assert.fail();
		} catch (Exception e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
		}
	}
	
	@Test
	public void testGetMinMaxTimes() {
		// Initialise time series table
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);	
		// Add time series data
		client.addTimeSeriesData(ts1);
		
		// Check for only one time series (with numerics data content)
		String iri = dataIRI_1.get(0);
		Instant min;
		Instant max;
		
		// Test minimum time
		min = client.getMinTime(iri);
		Assert.assertEquals(timeList_1.get(0), min);
		for (String i : dataIRI_1) {
			Assert.assertEquals(min, client.getMinTime(i));
		}
		
		// Test maximum time
		max = client.getMaxTime(iri);
		Assert.assertEquals(timeList_1.get(timeList_1.size()-1), max);
		for (String i : dataIRI_1) {
			Assert.assertEquals(max, client.getMaxTime(i));
		}
		
		// Test Exception for non numerics data
		iri = dataIRI_3.get(0);
		try {
			client.getMinTime(iri);
		} catch (Exception e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: <" + iri + "> does not have a time series instance",
								e.getMessage());
		}
	}
	
	@Test
	public void testDeleteRows() {
		// Initialise time series table
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);	
		// Add time series data
		client.addTimeSeriesData(ts1);
		
		// Check for time series with only one data IRI
		List<String> iris = dataIRI_1.subList(0, 1);
		String iri = iris.get(0);
		TimeSeries<Instant> ts = client.getTimeSeries(iris);
		Instant lb;
		Instant ub;
		
		// Test for correct manipulation of time series length
		Assert.assertEquals(timeList_1.size(), ts.getTimes().size());
		// Delete latest time entry
		lb = ts.getTimes().get(ts.getTimes().size()-1);
		ub = ts.getTimes().get(ts.getTimes().size()-1);			
		client.deleteRows(iri, lb, ub);
		ts = client.getTimeSeries(iris);
		Assert.assertEquals(timeList_1.size()-1, ts.getTimes().size());
		Assert.assertEquals(timeList_1.subList(0, timeList_1.size()-1), ts.getTimes());
		Assert.assertEquals(ts1.getValues(iri).subList(0, timeList_1.size()-1), ts.getValues(iri));
		
		// Test for upper bound outside current time range
		lb = ts.getTimes().get(0);
		ub = timeList_2.get(timeList_2.size()-1);
		client.deleteRows(iri, lb, ub);
		ts = client.getTimeSeries(iris);
		Assert.assertEquals(0, ts.getTimes().size());
		
		// Test for upper bound outside current time range
		// Add new time series data
		client.addTimeSeriesData(ts2);
		ub = timeList_2.get(timeList_2.size()-2);
		client.deleteRows(iri, lb, ub);
		ts = client.getTimeSeries(iris);
		Assert.assertEquals(1, ts.getTimes().size());
	}
	
	@Test
	public void testDeleteTimeSeries() throws NoSuchFieldException, IllegalAccessException {
		// Initialise time series table
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);	
		// Add time series data
		client.addTimeSeriesData(ts1);
		// Retrieve the value of the private field 'dbTableName' of the client to check its value
		Field tableNameField = client.getClass().getDeclaredField("dbTableName");
		tableNameField.setAccessible(true);
		String tableName = (String) tableNameField.get(client);
		Table<?> table = context.meta().getTables(tableName).get(0);
		// Retrieve the value of the private field 'dataIRIcolumn' of the client
		Field dataIRIcolumnField = client.getClass().getDeclaredField("dataIRIcolumn");
		dataIRIcolumnField.setAccessible(true);
		org.jooq.Field<String> dataIRIcolumn = (org.jooq.Field<String>) dataIRIcolumnField.get(client);
		// Retrieve the value of the private field 'tsTableNameColumn' of the client to check its value
		Field tsTableNameColumnField = client.getClass().getDeclaredField("tsTableNameColumn");
		tsTableNameColumnField.setAccessible(true);
		org.jooq.Field<String> tsTableNameColumn = (org.jooq.Field<String>) tsTableNameColumnField.get(client);
		// Retrieve the value of the private field 'tsTableNameColumn' of the client to check its value
		Field columnNameColumnField = client.getClass().getDeclaredField("columnNameColumn");
		columnNameColumnField.setAccessible(true);
		org.jooq.Field<String> columnNameColumn = (org.jooq.Field<String>) columnNameColumnField.get(client);
		
		// Check that timeseries table was correctly created (additionally to central lookup table)
		Assert.assertEquals(2, context.meta().getTables().size());
		List<String> iris_in_central_table = context.select(dataIRIcolumn).from(table).fetch(dataIRIcolumn);
		Assert.assertEquals(dataIRI_1.size(), iris_in_central_table.size());
		for (String i : dataIRI_1) {
			Assert.assertTrue(iris_in_central_table.contains(i));
			String tstable = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(i)).fetch(tsTableNameColumn).get(0);
			String tscolumn = context.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(i)).fetch(columnNameColumn).get(0);
			// Get all column names in tstable
			org.jooq.Field<?>[] columns = context.meta().getTables(tstable).get(0).fields();
			List<String> iris_in_ts_table = new ArrayList<>();
			for (org.jooq.Field<?> column : columns) {
				iris_in_ts_table.add(column.getName());
			}
			Assert.assertTrue(iris_in_ts_table.contains(tscolumn));
		}		
		
		// Delete first dataIRI column (1st out of 3): Verify correct deletion in central lookup and time series table
		List<String> iris = new ArrayList<>(dataIRI_1);
		String iri = iris.remove(0);
		String deleted_column = context.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(iri)).fetch(columnNameColumn).get(0);
		client.deleteTimeSeries(iri);
		iris_in_central_table = context.select(dataIRIcolumn).from(table).fetch(dataIRIcolumn);
		// Test that still both tables exist
		Assert.assertEquals(2, context.meta().getTables().size());
		// Test correct manipulations of central lookup table and time series table
		Assert.assertEquals(iris.size(), iris_in_central_table.size());
		for (String i : iris) {
			Assert.assertTrue(iris_in_central_table.contains(i));
			String tstable = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(i)).fetch(tsTableNameColumn).get(0);
			String tscolumn = context.select(columnNameColumn).from(table).where(dataIRIcolumn.eq(i)).fetch(columnNameColumn).get(0);
			// Get all column names in tstable
			org.jooq.Field<?>[] columns = context.meta().getTables(tstable).get(0).fields();
			List<String> iris_in_ts_table = new ArrayList<>();
			for (org.jooq.Field<?> column : columns) {
				iris_in_ts_table.add(column.getName());
			}
			Assert.assertTrue(iris_in_ts_table.contains(tscolumn));
			if (i.equals(iris.get(0))) {
				Assert.assertFalse(iris_in_ts_table.contains(deleted_column));
			}
		}
		Assert.assertFalse(iris_in_central_table.contains(iri));
		
		// Delete second dataIRI column (2nd out of 3): Verify deletion of correct amount of columns only
		iri = iris.remove(0);
		String tstable = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(iri)).fetch(tsTableNameColumn).get(0);
		Assert.assertEquals(3, context.meta().getTables(tstable).get(0).fields().length);		
		client.deleteTimeSeries(iri);
		// Test that still both tables exist
		Assert.assertEquals(2, context.meta().getTables().size());
		// Test that only one dataIRI columns (next to time column) remains
		Assert.assertEquals(2, context.meta().getTables(tstable).get(0).fields().length);
		
		// Check Exception for non-instantiated dataIRI
		try {
			iri = "non-existing-iri";
			client.deleteTimeSeries(iri);
			Assert.fail();
		} catch (Exception e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: <" + iri + "> does not have a time series instance",
								e.getMessage());
		}

		// Delete last dataIRI column: Verify that entire table gets deleted
		iri = iris.remove(0);
		client.deleteTimeSeries(iri);
		// Test that only timeseries table remains
		Assert.assertEquals(1, context.meta().getTables().size());
		// Test that time series table is empty
		iris_in_central_table = context.select(dataIRIcolumn).from(table).fetch(dataIRIcolumn);
		Assert.assertEquals(0, iris_in_central_table.size());
		
	}
	
	@Test
	public void testDeleteTimeSeriesTable() throws NoSuchFieldException, IllegalAccessException {
		// Initialise time series tables
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);
		client.initTimeSeriesTable(dataIRI_3, dataClass_3, tsIRI_3);
		// Add time series data
		client.addTimeSeriesData(ts1);
		client.addTimeSeriesData(ts3);
		// Retrieve the value of the private field 'dbTableName' of the client to check its value
		Field tableNameField = client.getClass().getDeclaredField("dbTableName");
		tableNameField.setAccessible(true);
		String tableName = (String) tableNameField.get(client);
		Table<?> table = context.meta().getTables(tableName).get(0);		
		// Retrieve the value of the private field 'dataIRIcolumn' of the client
		Field dataIRIcolumnField = client.getClass().getDeclaredField("dataIRIcolumn");
		dataIRIcolumnField.setAccessible(true);
		org.jooq.Field<String> dataIRIcolumn = (org.jooq.Field<String>) dataIRIcolumnField.get(client);
		
		// Check that timeseries tables were correctly created (additionally to central lookup table)
		Assert.assertEquals(3, context.meta().getTables().size());
		List<String> iris = new ArrayList<>(dataIRI_1);
		iris.addAll(dataIRI_3);
		List<String> iris_in_central_table = context.select(dataIRIcolumn).from(table).fetch(dataIRIcolumn);
		Assert.assertEquals(4, iris_in_central_table.size());
		for (String i : iris) {
			Assert.assertTrue(iris_in_central_table.contains(i));
		}
		
		// Delete first time series table
		client.deleteTimeSeriesTable(dataIRI_3.get(0));
		Assert.assertEquals(2, context.meta().getTables().size());
		iris_in_central_table = context.select(dataIRIcolumn).from(table).fetch(dataIRIcolumn);
		Assert.assertEquals(3, iris_in_central_table.size());
		for (String i : dataIRI_1) {
			Assert.assertTrue(iris_in_central_table.contains(i));
		}
		Assert.assertFalse(iris_in_central_table.contains(dataIRI_3.get(0)));
		
		// Check Exception for non-instantiated dataIRI
		String iri = "non-existing-iri";
		try {
			client.deleteTimeSeries(iri);
			Assert.fail();
		} catch (Exception e) {
			Assert.assertEquals(JPSRuntimeException.class, e.getClass());
			Assert.assertEquals("TimeSeriesRDBClient: <" + iri + "> does not have a time series instance",
								e.getMessage());
		}
		
		// Delete second time series table
		client.deleteTimeSeriesTable(dataIRI_1.get(0));
		Assert.assertEquals(1, context.meta().getTables().size());
		iris_in_central_table = context.select(dataIRIcolumn).from(table).fetch(dataIRIcolumn);
		Assert.assertEquals(0, iris_in_central_table.size());		
	}
	
	@Test
	public void testDeleteAll() {
		// Initialise time series tables
		client.initTimeSeriesTable(dataIRI_1, dataClass_1, tsIRI_1);
		client.initTimeSeriesTable(dataIRI_3, dataClass_3, tsIRI_3);
		// Add time series data
		client.addTimeSeriesData(ts1);
		client.addTimeSeriesData(ts3);
		
		// Delete all tables and verify deleting
		Assert.assertEquals(3, context.meta().getTables().size());
		client.deleteAll();
		Assert.assertEquals(0, context.meta().getTables().size());
		
		// Verify error-free execution if no tables are available
		client.deleteAll();
		Assert.assertEquals(0, context.meta().getTables().size());
	}
}

