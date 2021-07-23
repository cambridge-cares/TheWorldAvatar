package uk.ac.cam.cares.jps.base.timeseries.test;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import org.junit.*;
import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;
import static org.jooq.impl.DSL.selectFrom;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;


@Ignore("Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
public class TimeSeriesRDBClientlIntegrationTest {
	
	// Define RDB database setup (analogous to a triple-store endpoint)
	// Using special testcontainers URL that will spin up a Postgres Docker container when accessed by a driver
	// (see: https://www.testcontainers.org/modules/databases/jdbc/). Note: requires Docker to be installed!
	//private static final String dbURL = "jdbc:tc:postgresql:13.3:///timeseries";
	// Use local development environment URL for easier debugging
	private static final String dbURL = "jdbc:postgresql:timeseries";
	private static final String user = "postgres";
	private static final String password = "postgres";

	private static Connection conn;
	private static DSLContext context;

	// RDB client
	private TimeSeriesRDBClient<Instant> client;

	// Time series attributes
	private List<String> dataIRI;
	private List<Class<?>> dataClass;


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
		for (Table table: tables) {
			context.dropTable(table).cascade().execute();
		}
	}

	// Disconnect from the database after all tests are run
	@AfterClass
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

	// Clear all tables after each test to ensure clean slate
	@After
	public void clearDatabase() {
		List<Table<?>> tables = context.meta().getTables();
		for (Table table: tables) {
			context.dropTable(table).cascade().execute();
		}
	}

	@Before
	public void setup() {
		// Initialise 1 time series with 3 associated data series
		dataIRI = new ArrayList<>();
		dataIRI.add("http://data1"); dataIRI.add("http://data2"); dataIRI.add("http://data3");
		// Specify type of data for each column (most data will be in doubles, but one can specify different data types)
		dataClass = new ArrayList<>();
		dataClass.add(Double.class); dataClass.add(String.class); dataClass.add(Integer.class);
	}

	@Test
	public void testInitTimeSeriesTable() throws NoSuchFieldException, IllegalAccessException {
		client.initTimeSeriesTable(dataIRI, dataClass, null);
		// Check that timeseries table was created in addition to central table
		Assert.assertEquals(2, context.meta().getTables().size());
		
//		// Check that only one table was created
//		Assert.assertEquals(1, context.meta().getTables("dbTable").size());
//		// Check that the table has four columns (timeseries IRI, timseries table, data IRI, column name)
//		Assert.assertEquals(4, context.meta().getTables("dbTable").fields().length);

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
		for (String iri: dataIRI) {
			Assert.assertTrue(context.fetchExists(selectFrom(table).where(dataIRIcolumn.eq(iri))));
		}
		// Check that all data IRIs are connected to same timeseries IRI and table name
		List<String> queryResult = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(dataIRI.get(0))).fetch(tsTableNameColumn);
		String tsTableName = queryResult.get(0);
		queryResult = context.select(tsIRIcolumn).from(table).where(dataIRIcolumn.eq(dataIRI.get(0))).fetch(tsIRIcolumn);
		String tsIRI = queryResult.get(0);
		for (String iri: dataIRI) {
			String curTableName = context.select(tsTableNameColumn).from(table).where(dataIRIcolumn.eq(iri)).fetch(tsTableNameColumn).get(0);
			String curTsIRI = context.select(tsIRIcolumn).from(table).where(dataIRIcolumn.eq(iri)).fetch(tsIRIcolumn).get(0);
			Assert.assertEquals(tsTableName, curTableName);
			Assert.assertEquals(tsIRI, curTsIRI);
		}
	}
	
}

