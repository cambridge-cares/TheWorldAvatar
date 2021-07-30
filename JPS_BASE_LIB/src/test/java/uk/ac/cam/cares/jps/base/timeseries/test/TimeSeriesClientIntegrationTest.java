package uk.ac.cam.cares.jps.base.timeseries.test;


import java.lang.reflect.Field;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.junit.*;
import org.mockito.Mockito;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import org.testcontainers.containers.PostgreSQLContainer;

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import static org.mockito.Mockito.*;

//@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
//		"Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution")
@Testcontainers
public class TimeSeriesClientIntegrationTest {
	
	// TimeSeries client (with RDB and Sparql client)
	private static TimeSeriesClient<Instant> tsClient;
	
	// Time series test data
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
	private static String timeUnit;

	// Will create two Docker containers for Blazegraph and postgreSQL
	// NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
	@Container
	// Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
	// For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker_Registry
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
												 .withExposedPorts(9999);
	@Container
	// Create Docker container with postgres 13.3 image from Docker Hub
	private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");
	
	@BeforeClass
	// Initialise 3 test time series data sets
	public static void initialiseData() {
		// Initialise time unit for all test data series
		timeUnit = "http://s";
		/* 
		 * Initialise 1st time series with 3 associated data series
		 */
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
	
	@Before
	// Create clean slate (new Docker containers) for each test
	public void initialiseTimeSeriesClient() {
		
		// Start Blazegraph container
		blazegraph.start();
		// Start postgreSQL container
		postgres.start();
		
		// Set endpoint to the triple store. The host and port are read from the container
		String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
		// Default namespace in blazegraph is "kb"
		endpoint = endpoint + "/blazegraph/namespace/kb/sparql";
		
		// Set up a kb client that points to the location of the triple store
		RemoteStoreClient kbClient = new RemoteStoreClient();		
		kbClient.setUpdateEndpoint(endpoint);
		kbClient.setQueryEndpoint(endpoint);
		
		// Initialise TimeSeriesClient client with pre-configured kb client
	    try {
	    	tsClient = new TimeSeriesClient<Instant>(kbClient, Instant.class, 
	    						Paths.get(getClass().getResource("/timeseries.properties").toURI()).toString());
	    } catch (Exception e) {
	    	// Simply suppress exceptions for potential issues, as properties will be overwritten anyway
	    }
	    
	    // Configure database access
	    tsClient.setRDBClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());	    
	}
		
	@Test
	public void testInitTimeSeriesWithoutExceptions() {
		
		// Retrieve RDB and RDF/SPARQL clients
		TimeSeriesSparql rdfClient = tsClient.getRdfClient();
		TimeSeriesRDBClient<Instant> rdbClient = tsClient.getRdbClient();	
				
		// Verify kb is initially empty
		Assert.assertEquals(0, rdfClient.countTS());
		
		// Initialise time series (3 dataIRIs, 1 tsIRI) in knowledge base and database		
		tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit);
		
		// Verify correct instantiation in both kb and database
		Assert.assertEquals(1, rdfClient.countTS());
		Assert.assertEquals(3, rdfClient.getAssociatedData(rdfClient.getTimeSeries(dataIRI_1.get(0))).size());
		TimeSeries<Instant> ts = rdbClient.getTimeSeries(dataIRI_1);
		Assert.assertEquals(3, ts.getDataIRIs().size());
		for (String iri : dataIRI_1) {
			Assert.assertTrue(ts.getDataIRIs().contains(iri));
		}		
	}
	
	@Test
	public void testInitTimeSeriesWithUnavailableKG() {
	
		// Interrupt triple store connection
		blazegraph.stop();
		
		try {
			// Initialise time series (3 dataIRIs, 1 tsIRI) in knowledge base and database		
			tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit);
			Assert.fail();
		} catch(Exception e) {
			Assert.assertTrue(e.getMessage().contains("Timeseries was not created!"));
		}		
	}
	
	@Test
	public void testInitTimeSeriesWithUnavailableRDB()  {
	
		// Interrupt triple store connection
		postgres.stop();
		
		try {
			// Initialise time series (3 dataIRIs, 1 tsIRI) in knowledge base and database		
			tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit);
			Assert.fail();
		} catch(Exception e) {
			Assert.assertTrue(e.getMessage().contains("Timeseries was not created!"));
		}		
	}
	
	@Test
	public void testInitTimeSeriesWithUnavailableRDBAndKGRevertIssues() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		// Retrieve the value of the private field 'rdfClient' of the time series client
		Field RDFClient = tsClient.getClass().getDeclaredField("rdfClient");
		RDFClient.setAccessible(true);
		TimeSeriesSparql rdfClient = (TimeSeriesSparql)	RDFClient.get(tsClient);
		
		// Create a spy object of the real rdfClient and substitute the initial rdfClient with it
		// Spy's behave exactly like normal instances, except for particularly stubbed methods
		TimeSeriesSparql rdfClient_spy = spy(rdfClient);
		RDFClient.set(tsClient, rdfClient_spy);
		// Throw error when removal of time series in KG is intended (after RDB interaction failed) to simulate connection error etc.
		doThrow(new JPSRuntimeException("")).when(rdfClient_spy).removeTimeSeries(Mockito.anyString());	
		
		// Interrupt database connection
		postgres.stop();
		
		try {
			// Initialise time series (3 dataIRIs, 1 tsIRI) in knowledge base and database		
			tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit);
			Assert.fail();
		} catch(Exception e) {
			Assert.assertTrue(e.getMessage().contains("Inconsistent state created when initialising time series"));
		}		
	}
	
//	@Test
//	public void testDeleteTimeSeries() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
//		
//		// Retrieve the value of the private field 'rdfClient' of the time series client
//		Field RDFClient = tsClient.getClass().getDeclaredField("rdfClient");
//		RDFClient.setAccessible(true);
//		TimeSeriesSparql rdfClient = (TimeSeriesSparql) RDFClient.get(tsClient);
//		// Retrieve the value of the private field 'rdbClient' of the time series client
//		Field RDBClient = tsClient.getClass().getDeclaredField("rdbClient");
//		RDBClient.setAccessible(true);
//		TimeSeriesRDBClient<Instant> rdbClient = (TimeSeriesRDBClient<Instant>) RDBClient.get(tsClient);
//		
//		// Initialise time series (3 dataIRIs, 1 tsIRI) in knowledge base and database		
//		tsClient.initTimeSeries(dataIRI_1, dataClass_1, timeUnit);
//		tsClient.initTimeSeries(dataIRI_3, dataClass_3, timeUnit);
//		
//		// Verify correct instantiation in both kb and database
//		Assert.assertEquals(2, rdfClient.countTS());
//		TimeSeries<Instant> ts1 = rdbClient.getTimeSeries(dataIRI_1);
//		Assert.assertEquals(3, ts1.getDataIRIs().size());
//		TimeSeries<Instant> ts2 = rdbClient.getTimeSeries(dataIRI_3);
//		Assert.assertEquals(1, ts2.getDataIRIs().size());
//		
//		// Delete first time series
//		//String tsIRI = tsClient;
//		//tsClient.deleteTimeSeries(tsIRI);
//		
//	}
	
}

