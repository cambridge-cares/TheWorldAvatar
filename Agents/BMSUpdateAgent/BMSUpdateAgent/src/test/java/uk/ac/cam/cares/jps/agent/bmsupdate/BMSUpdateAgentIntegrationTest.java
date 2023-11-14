package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockserver.integration.ClientAndServer;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.agent.bmsupdate.helper.BlazegraphHelper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.mockserver.model.HttpRequest.request;
import static org.mockserver.model.HttpResponse.response;

@Ignore("Ignore because it causes error when building docker image, but works fine with mvn test." +
        "Requires triple store endpoint set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
@Testcontainers
public class BMSUpdateAgentIntegrationTest {
    @Container
    private static final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);

    @Container
    private final static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");
    
    private static BMSUpdateAgent agent;
   
    private static CloseableHttpClient httpClient;

    //remote store client
    private static RemoteStoreClient kbClient;

    //remote rdb client
    private static RemoteRDBStoreClient rdbClient;
    //tsClient
    private static TimeSeriesClient<OffsetDateTime> tsClient;

    // Values created as example readings
    private static ArrayList<Double> Values;

    //lists of date time
    private static List<OffsetDateTime> times;

    // list of IRIs
    private static ArrayList<String> testIRIs;

    //list of classes to initialize timeseries
    private static List<Class<?>> classes;

    /**
     * Namespaces for ontologies
     */
	public static final String ONTOBMS_NS = "https://www.theworldavatar.com/kg/ontobms/";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/";

    /**
     * Prefixes
     */
	private static final Prefix PREFIX_ONTOBMS = SparqlBuilder.prefix("ontobms", iri(ONTOBMS_NS));
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix("om", iri(OM_NS));

    /**
     * Relationships
     */
    private static final Iri hasBacnetDeviceID = PREFIX_ONTOBMS.iri("hasBacnetDeviceID");
    private static final Iri hasBacnetObjectID = PREFIX_ONTOBMS.iri("hasBacnetObjectID");
    private static final Iri hasNumericalValue = PREFIX_OM.iri("hasNumericalValue");
    

    // Example prefix for IRIs
    private final static String examplePrefix = "example:prefix/api_";

    //test IRI
    private final static String test_IRI = examplePrefix + "test";

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeClass
    public static void setUpBeforeClass() {
        try {
            blazegraph.start();
            postgres.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }
        agent = new BMSUpdateAgent();
        httpClient = HttpClients.createDefault();
        TriplePattern updatePattern = iri(test_IRI).has(hasBacnetDeviceID, "123456").andHas(hasBacnetObjectID, "123").andHas(hasNumericalValue, "0.0");
        InsertDataQuery insert = Queries.INSERT_DATA(updatePattern);
        insert.prefix(PREFIX_ONTOBMS,PREFIX_OM);
        
        // Set endpoint to the triple store. The host and port are read from the container
        String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb"
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";

        // Set up a kb client that points to the location of the triple store
        kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQueryEndpoint(endpoint);
        kbClient.executeUpdate(insert.getQueryString());

        rdbClient = new RemoteRDBStoreClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());

        // Initialise TimeSeriesClient client with pre-configured kb client
        tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());

        Double value = 1.0;

        Values = new ArrayList<>();
        Values.add(0, value);

        List<List<?>> values = new ArrayList<>();
        values.add(Values);

        testIRIs = new ArrayList<>();
        testIRIs.add(test_IRI);

        long timestampLong = System.currentTimeMillis();
        Date date = new java.util.Date(timestampLong);
        SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        Object ts = sdf.format(date);
        String timestamp = ts.toString();
        times = new ArrayList<>();
        times.add(0, convertStringToOffsetDateTime(timestamp));
        TimeSeries<OffsetDateTime> testTimeSeries = new TimeSeries<>(times, testIRIs, values);
        //Initialise mock timeseries in triple store and relational database
        classes = new ArrayList<>();
        classes.add(0, Double.class);
        tsClient.initTimeSeries(testIRIs, classes, OffsetDateTime.class.getSimpleName(), Type.INSTANTANEOUS, null, null);
        tsClient.addTimeSeriesData(testTimeSeries);
    }

    @AfterClass
    public static void tearDownAfterClass() {
        if (blazegraph.isRunning()) {
            blazegraph.close();
        }
    }

    @Test
    public void testGetAndSetTemperature() throws IOException {
        String namespace = "test";
        BlazegraphHelper.createNewNameSpace(namespace, folder, httpClient, getBlazegraphEndPoint());
        BlazegraphHelper.createNewData("initTemperature.xml", getNamespaceUrl(namespace), httpClient);

        RemoteStoreClient rsClient = new RemoteStoreClient(getNamespaceUrl(namespace), getNamespaceUrl(namespace));
        double originalTemperature = agent.getTemperatureInKg("https://www.theworldavatar.com/kg/ontodevice/mock_setpoint", rsClient);
        Assert.assertEquals(26.0, originalTemperature, 0.001);

        agent.setTemperatureInKg("https://www.theworldavatar.com/kg/ontodevice/mock_setpoint", 10.0, rsClient);

        double newTemperature = agent.getTemperatureInKg("https://www.theworldavatar.com/kg/ontodevice/mock_setpoint", rsClient);
        Assert.assertEquals(10.0, newTemperature, 0.001);

        BlazegraphHelper.clearBlazegraphData(getNamespaceUrl(namespace), httpClient);
    }

    @Test
    public void testToggleFan_AlreadyOn() {
        try (ClientAndServer espHome = new ClientAndServer(1011)) {
            espHome.when(request().withPath("/esphome-agent/toggle").withBody(getESPHomeInput().toString()))
                    .respond(response().withBody("{\"message\":[\"The component is already in the ON state.\",\"A request has been successfully sent to the ESPHome web server.\",\"POST request has been sent successfully.\"]}"));

            String response = agent.toggleFan(getESPHomeAgentUrl(espHome.getPort()));
            Assert.assertEquals("The fan is in the ON state.", response);
        }
    }

    @Test
    public void testToggleFan_AlreadyOff() {
        try (ClientAndServer espHome = new ClientAndServer(1011)) {
            espHome.when(request().withPath("/esphome-agent/toggle").withBody(getESPHomeInput().toString()))
                    .respond(response().withBody("{\"message\":[\"The component is already in the OFF state.\",\"A request has been successfully sent to the ESPHome web server.\",\"POST request has been sent successfully.\"]}"));

            String response = agent.toggleFan(getESPHomeAgentUrl(espHome.getPort()));
            Assert.assertEquals("The fan is in the OFF state.", response);
        }
    }

    @Test
    public void testToggleFan_TurnOn() {
        try (ClientAndServer espHome = new ClientAndServer(1011)) {
            espHome.when(request().withPath("/esphome-agent/toggle").withBody(getESPHomeInput().toString()))
                    .respond(response().withBody("{\"message\":[\"A POST request has been sent to turn on the device or component.\",\"A request has been successfully sent to the ESPHome web server.\",\"POST request has been sent successfully.\"]}"));

            String response = agent.toggleFan(getESPHomeAgentUrl(espHome.getPort()));
            Assert.assertEquals("The fan is in the ON state.", response);
        }

    }

    @Test
    public void testToggleFan_TurnOff() {
        try (ClientAndServer espHome = new ClientAndServer(1011)) {
            espHome.when(request().withPath("/esphome-agent/toggle").withBody(getESPHomeInput().toString()))
                    .respond(response().withBody("{\"message\":[\"A POST request has been sent to turn off the device or component.\",\"A request has been successfully sent to the ESPHome web server.\",\"POST request has been sent successfully.\"]}"));

            String response = agent.toggleFan(getESPHomeAgentUrl(espHome.getPort()));
            Assert.assertEquals("The fan is in the OFF state.", response);
        }
    }

    @Test
    public void testGetObjectID() {
        String objectID = agent.getObjectId(test_IRI, kbClient);
        Assert.assertEquals("123", objectID);
    }

    @Test
    public void testGetDeviceID() {
        String deviceID = agent.getDeviceId(test_IRI, kbClient);
        Assert.assertEquals("123456", deviceID);
    }

    @Test
    public void testCheckInsert() {
        //test triggerValue != latestValue
        JSONObject result = agent.checkInsert(test_IRI, kbClient, rdbClient, "0.0", "<example:prefix/api_test> <example:testing> \"example\". ");
        Assert.assertTrue(result.getString("message").contains("The latest timeseries value of example:prefix/api_test is not equivalent to the trigger value 0.0"));

        //test triggerValue = latestValue
        result = agent.checkInsert(test_IRI, kbClient, rdbClient, "1.0", "<example:prefix/api_test> <example:testing> \"example\". ");
        Assert.assertTrue(result.getString("message").contains("Executed the following to the knowledge graph: INSERT DATA"));

        Variable var = SparqlBuilder.var("var");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = var.has(iri("example:testing"), "example");
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals("example:prefix/api_test", queryResult.getJSONObject(0).getString("var"));
    }

    @Test
    public void testCheckDelete() {
        //test triggerValue != latestValue
        JSONObject result = agent.checkDelete(test_IRI, kbClient, rdbClient, "0.0", "<example:prefix/api_test> <https://www.theworldavatar.com/kg/ontobms/hasBacnetDeviceID> \"123456\". ");
        Assert.assertTrue(result.getString("message").contains("The latest timeseries value of example:prefix/api_test is not equivalent to the trigger value 0.0"));

        //test triggerValue = latestValue
        result = agent.checkDelete(test_IRI, kbClient, rdbClient, "1.0", "<example:prefix/api_test> <https://www.theworldavatar.com/kg/ontobms/hasBacnetDeviceID> \"123456\". ");
        Assert.assertTrue(result.getString("message").contains("Executed the following to the knowledge graph: DELETE DATA"));

        Variable var = SparqlBuilder.var("var");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(test_IRI).has(hasBacnetDeviceID,var);
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient.executeQuery();
        } catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("error when executing SPARQL query:"));
        }
    }

    @Test
    public void testCheckInsertAndDelete() {
        //test triggerValue != latestValue
        JSONObject result = agent.checkInsertAndDelete(test_IRI, kbClient, rdbClient, "0.0", "<example:prefix/api_test> <example:testing> \"example\". ", "<example:prefix/api_test> <https://www.theworldavatar.com/kg/ontobms/hasBacnetDeviceID> \"123456\". ");
        Assert.assertTrue(result.getString("message").contains("The latest timeseries value of example:prefix/api_test is not equivalent to the trigger value 0.0"));

        //test triggerValue = latestValue
        result = agent.checkInsertAndDelete(test_IRI, kbClient, rdbClient, "1.0", "<example:prefix/api_test> <example:testing> \"example\". ", "<example:prefix/api_test> <https://www.theworldavatar.com/kg/ontobms/hasBacnetDeviceID> \"123456\". ");
        Assert.assertTrue(result.getJSONArray("message").getString(0).contains("Executed the following to the knowledge graph: INSERT DATA"));
        Assert.assertTrue(result.getJSONArray("message").getString(1).contains("Executed the following to the knowledge graph: DELETE DATA"));

        Variable var = SparqlBuilder.var("var");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = var.has(iri("example:testing"), "example");
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals("example:prefix/api_test", queryResult.getJSONObject(0).getString("var"));

        //create triple pattern
        queryPattern = iri(test_IRI).has(hasBacnetDeviceID,var);
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        try {
        queryResult = kbClient.executeQuery();
        } catch (JPSRuntimeException e) {
            Assert.assertTrue(e.getMessage().contains("error when executing SPARQL query:"));
        }
    }

    @Test
    public void testSetNumericalValue() {
        //test triggerValue != latestValue
        JSONObject result = agent.setNumericalValue(test_IRI, 1.0, kbClient);
        Assert.assertTrue(result.getString("message").contains("Sucessfully executed the following query string:"));

        Variable var = SparqlBuilder.var("var");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(test_IRI).has(hasNumericalValue, var);
        query.select(var).where(queryPattern).prefix(PREFIX_OM);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals("1.0", queryResult.getJSONObject(0).getString("var"));
    }

    private String getESPHomeAgentUrl(Integer port) {
        return "http://localhost:" + port + "/esphome-agent/toggle";
    }

    private String getESPHomeUpdateAgentUrl(Integer port) {
        return "http://localhost:" + port + "/esphome-update-agent/retrieve";
    }

    private String getBlazegraphEndPoint() {
        return "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort() + "/blazegraph";
    }

    private String getNamespaceUrl(String namespace) {
        return getBlazegraphEndPoint() + "/namespace/" + namespace + "/sparql";
    }

    private JSONObject getESPHomeInput() {
        JSONObject expectedBody = new JSONObject();
        expectedBody.put("timeseriesDataClientProperties", "CLIENT_PROPERTIES");
        expectedBody.put("esphomeStatusClientProperties", "ESPHOME_CLIENT_PROPERTIES");
        expectedBody.put("esphomeAPIProperties", "API_PROPERTIES");

        return expectedBody;
    }

    
    /**
     * Converts a string into a datetime object with zone information using the zone globally define for the agent.
     * @param timestamp The timestamp as string, the format should be equal to 2007-12-03T10:15:30.
     * @return The resulting datetime object.
     */
    private static OffsetDateTime convertStringToOffsetDateTime(String timestamp) {
        // Convert first to a local time
        LocalDateTime localTime = LocalDateTime.parse(timestamp);

        // Then add the zone id
        return OffsetDateTime.of(localTime, ZoneOffset.UTC);
    }


}
