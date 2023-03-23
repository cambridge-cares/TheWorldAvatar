package uk.ac.cam.cares.jps.agent.esphome;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.DeleteDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import com.github.stefanbirkner.systemlambda.Statement;
import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * This test class is to test the ESPHome agent with a running KG and postgres database.
 */

@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")

@Testcontainers
public class ESPHomeAgentIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private ESPHomeAgent agent;
    private TimeSeriesClient<OffsetDateTime> tsClient;
    //single key for mocking timeseries data IRI
    private final String key = "IntTemp";
    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";
    //endpoint
    String endpoint;
    // IRIs corresponding to the keys
    private ArrayList<String> IRIs;
    private ArrayList<String> testIRIs;
    //lists of date time
    private List<OffsetDateTime> times;

    // Values created as example readings
    private ArrayList<Double> Values;

    // Readings used by several tests
    JSONObject allReadings;
    
    //list of classes to initialize timeseries
    private List<Class<?>> classes;
    
    //Prefixes
    public static final String ns_ontology = "https://www.theworldavatar.com/kg/ontotimeseries/";
 	private static final Prefix prefix_ontology = SparqlBuilder.prefix("ts", iri(ns_ontology));
 	private static final Iri hasRDB = prefix_ontology.iri("hasRDB");
 	
    @Before
    public void initializeMockTimeSeriesandAgent() throws IOException {
        // Start the containers
        try {
            // Start Blazegraph container
            blazegraph.start();
            // Start postgreSQL container
            postgres.start();
        } catch (Exception e) {
            throw new AssertionError("TimeSeriesClientIntegrationTest: Docker container startup failed. Please try running tests again");
        }
        IRIs = new ArrayList<>();
        IRIs.add(examplePrefix+key);
       
        // Create and set time-series client //
        // Set endpoint to the triple store. The host and port are read from the container
        endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb"
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";

        // Set up a kb client that points to the location of the triple store
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQueryEndpoint(endpoint);
        
        String propertiesFile = Paths.get(folder.getRoot().toString(), "all.properties").toString();
        //single mock property file to represent all 3 properties files
        writePropertyFile(propertiesFile, Arrays.asList("db.user=postgres", "db.password=cares1010", "sparql.query.endpoint="+endpoint, "sparql.update.endpoint="+endpoint, "dataIRI="+IRIs.get(0), "esphome.url=test_url", "esphome.domain=test_domain", "domain.ID=test_ID", "esphome.threshold=25"));
        
        // Initialise TimeSeriesClient client with pre-configured kb client
        tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, null, "postgres", "cares1010");
        // Configure database access
        tsClient.setRDBClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
        //Initialise mock timeseries in triple store and relational database
        classes = new ArrayList<>();
        classes.add(0, Double.class);
        tsClient.initTimeSeries(IRIs, classes, OffsetDateTime.class.getSimpleName());
        
        agent = new ESPHomeAgent();
    }

    @Before
    public void addMockTimeSeriesData() {
        double value = 26.5;
        allReadings = new JSONObject();
        Values = new ArrayList<>();
        Values.add(0, value);
        List<List<?>> values = new ArrayList<>();
        values.add(Values);
        String timestamp = "2022-01-25T11:28:00";
        testIRIs = new ArrayList<>();
        testIRIs.add(examplePrefix+key);
        times = new ArrayList<>();
        times.add(0, convertStringToOffsetDateTime(timestamp));
        TimeSeries<OffsetDateTime> testTimeSeries = new TimeSeries<>(times, testIRIs, values);
        tsClient.addTimeSeriesData(testTimeSeries);
        }

    // Cleaning up containers after each test, otherwise unused containers will first be killed when all tests finished
    @After
    public void stopContainers() {
        if (blazegraph.isRunning()) {
            blazegraph.stop();
        }
        if (postgres.isRunning()) {
            postgres.stop();
        }
    }

    /**
     * Converts a string into a datetime object with zone information using the zone globally define for the agent.
     * @param timestamp The timestamp as string, the format should be equal to 2007-12-03T10:15:30.
     * @return The resulting datetime object.
     */
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp) {
        // Convert first to a local time
        LocalDateTime localTime = LocalDateTime.parse(timestamp);
        // Then add the zone id
        return OffsetDateTime.of(localTime, ZoneOffset.UTC);
    }
    
    private void writePropertyFile(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }
    
    @Test
    public void testProcessRequestParametersAndValidateInput() throws IOException {
    	
    	//test empty requestparams
    	JSONObject testRequestParams = new JSONObject();
    	JSONObject testMessage = agent.processRequestParameters(testRequestParams);
    Assert.assertEquals("Unable to validate request sent to the agent.",testMessage.get("message"));
    
    //test non-empty requestParams but with invalid environment variables
    testRequestParams.put("esphomeAPIProperties", "TEST_APIPROPERTIES");
    testRequestParams.put("esphomeStatusClientProperties", "TEST_CLIENTPROPERTIES");
    testRequestParams.put("timeseriesDataClientProperties", "TEST_SECONDCLIENTPROPERTIES");  
   
    testMessage = agent.processRequestParameters(testRequestParams);
    Assert.assertEquals("Unable to validate request sent to the agent.",testMessage.get("message"));
    
    //test missing apiProperties key in requestparams
    testRequestParams.remove("esphomeAPIProperties");
    testMessage = new JSONObject();
    testMessage = agent.processRequestParameters(testRequestParams);
    Assert.assertEquals("Unable to validate request sent to the agent.",testMessage.get("message"));
    
    //test missing ESPHome client properties key in request params
    testRequestParams.remove("esphomeStatusClientProperties");
    testRequestParams.put("esphomeAPIProperties", "TEST_APIPROPERTIES");
    testMessage = new JSONObject();
    testMessage = agent.processRequestParameters(testRequestParams);
    Assert.assertEquals("Unable to validate request sent to the agent.",testMessage.get("message"));
    
    testRequestParams.put("esphomeStatusClientProperties", "TEST_CLIENTPROPERTIES");
    
    //test missing client properties key in request params
    testRequestParams.remove("timeseriesDataClientProperties");
    testMessage = new JSONObject();
    testMessage = agent.processRequestParameters(testRequestParams);
    Assert.assertEquals("Unable to validate request sent to the agent.",testMessage.get("message"));
    testRequestParams.put("timeseriesDataClientProperties", "TEST_SECONDCLIENTPROPERTIES");
    
	String propertiesFile = Paths.get(folder.getRoot().toString(), "all.properties").toString();
	writePropertyFile(propertiesFile, Arrays.asList("test1=test_1", "test2=test_2"));
    //try and catch is required to use SystemLambda to mock environment variables
    try {
    	SystemLambda.withEnvironmentVariable("TEST_CLIENTPROPERTIES", propertiesFile).and("TEST_APIPROPERTIES", propertiesFile).and("TEST_SECONDCLIENTPROPERTIES", propertiesFile).execute((Statement) () -> {
    		boolean validate = agent.validateInput(testRequestParams);
    		Assert.assertEquals(validate, true);
    		});
		} catch (Exception e) {
		//no Exception should be thrown here
		}
    }

    @Test
    public void testArgumentMismatch() throws IOException {
    	//test empty args
    	String [] args = new String[] {};
    	try {
    		agent.initializeAgent(args);
    		Assert.fail();
            } catch (JPSRuntimeException e) {
            	Assert.assertEquals(e.getMessage(),"Need three properties files in the following order:1) time series client for timeseries data 2) time series client for esphome status 3)esphome API properties");
        }
    	//test only 1 properties file in args
    	String propertiesFile = Paths.get(folder.getRoot().toString(), "all.properties").toString();
    	writePropertyFile(propertiesFile, Arrays.asList("test1=test_1", "test2=test_2"));
    	args = new String[] {propertiesFile};
    	try {
    		agent.initializeAgent(args);
    		Assert.fail();
            } catch (JPSRuntimeException e) {
            	Assert.assertEquals(e.getMessage(),"Need three properties files in the following order:1) time series client for timeseries data 2) time series client for esphome status 3)esphome API properties");
        }
    	
    	//test only 2 properties file in args
    	args = new String[] {propertiesFile, propertiesFile};
    	try {
    		agent.initializeAgent(args);
    		Assert.fail();
            } catch (JPSRuntimeException e) {
            	Assert.assertEquals(e.getMessage(),"Need three properties files in the following order:1) time series client for timeseries data 2) time series client for esphome status 3)esphome API properties");
        }
    	
    }
    
    @Test
    public void testLoadTsClientProperties() throws NoSuchFieldException, IllegalAccessException, IOException {
    	 // Filepath to not yet created file in temporary test folder
        String filePath = Paths.get(folder.getRoot().toString(), "test.properties").toString();
        //test constructor with non existent file
        try {
            agent.loadTsClientProperties(filePath);
            Assert.fail();
        }
        catch (FileNotFoundException e) {
            Assert.assertEquals("No properties file found at specified filepath: " + filePath, e.getMessage());
        }   
        // Test for missing db.user
        writePropertyFile(filePath, Collections.singletonList("db.password=test_password"));
        try {
        	agent.loadTsClientProperties(filePath);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertEquals("Properties file is missing \"db.user=<db_user>\"", e.getMessage());
        }
        
        // Test for missing db.password
        writePropertyFile(filePath, Collections.singletonList("db.user=test_user"));
        try {
        	agent.loadTsClientProperties(filePath);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertEquals("Properties file is missing \"db.password=<db_password>\"", e.getMessage());
        }

        // Test for missing db.url
        writePropertyFile(filePath, Arrays.asList("db.user=test_user", "db.password=test_password"));
        try {
        	agent.loadTsClientProperties(filePath);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertEquals("Properties file is missing \"db.url=<db_url>\"", e.getMessage());
        }
        
        //Test for missing sparql.query.endpoint
        writePropertyFile(filePath, Arrays.asList("db.user=test_user", "db.password=test_password", "db.url=test_url"));
        try {
        	agent.loadTsClientProperties(filePath);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertEquals("Properties file is missing \"sparql.query.endpoint=<sparql_endpoint>\" ", e.getMessage());
        }
        
      //Test for missing sparql.update.endpoint
        writePropertyFile(filePath, Arrays.asList("db.user=test_user", "db.password=test_password", "db.url=test_url", "sparql.query.endpoint=test_query"));
        try {
        	agent.loadTsClientProperties(filePath);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertEquals("Properties file is missing \"sparql.update.endpoint=<sparql_endpoint>\" ", e.getMessage());
        }
        
        //Test for missing data IRI
        writePropertyFile(filePath, Arrays.asList("db.user=test_user", "db.password=test_password", "db.url=test_url", "sparql.query.endpoint=test_query", "sparql.update.endpoint=test_update"));
        try {
        	agent.loadTsClientProperties(filePath);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertEquals("Properties file is missing \"dataIRI=<data_IRI>\"", e.getMessage());
        }
    }
}
    	
    
   
    
    
    

