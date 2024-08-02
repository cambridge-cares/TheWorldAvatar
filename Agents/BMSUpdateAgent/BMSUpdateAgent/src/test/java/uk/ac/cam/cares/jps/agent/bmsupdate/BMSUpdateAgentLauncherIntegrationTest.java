package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
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
import org.mockserver.client.MockServerClient;
import org.springframework.mock.web.MockHttpServletRequest;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.MockServerContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.agent.bmsupdate.helper.BlazegraphHelper;
import uk.ac.cam.cares.jps.agent.bmsupdate.helper.PropertiesFileHelper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;

import java.io.IOException;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockserver.model.HttpRequest.request;
import static org.mockserver.model.HttpResponse.response;
import static uk.org.webcompere.systemstubs.SystemStubs.withEnvironmentVariable;

@Ignore("Ignore because it causes error when building docker image, but works fine with mvn test." +
        "Requires triple store endpoint set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
@Testcontainers
public class BMSUpdateAgentLauncherIntegrationTest {

    public static final DockerImageName MOCKSERVER_IMAGE = DockerImageName
            .parse("mockserver/mockserver")
            .withTag("mockserver-" + MockServerClient.class.getPackage().getImplementationVersion());
    @Container
    private static final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);

    @Container
    private final static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");
    
    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();
    public static MockServerContainer mockESPHomeServer = new MockServerContainer(MOCKSERVER_IMAGE);
    public static MockServerContainer mockESPHomeUpdateServer = new MockServerContainer(MOCKSERVER_IMAGE);
    public static MockServerContainer mockWacnetServer = new MockServerContainer(MOCKSERVER_IMAGE);
    private static BMSUpdateAgentLauncher launcher;
    private static CloseableHttpClient httpClient;

    String namespace = "test";

    // Example prefix for IRIs
    private final static String examplePrefix = "example:prefix/api_";

    //test IRI
    private final static String test_IRI = examplePrefix + "test";

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
    
    //remote store client
    private static RemoteStoreClient kbClient;

    @BeforeClass
    static public void setUpBeforeClass() {
        launcher = new BMSUpdateAgentLauncher();
        httpClient = HttpClients.createDefault();
    }

    @AfterClass
    public static void tearDownAfterClass() throws IOException {
        httpClient.close();
    }

    private static void preparePropertiesFile() throws IOException {
        List<String> lines = PropertiesFileHelper.readClientPropertiesTemplateFile(mockESPHomeServer.getHost() + ":" + mockESPHomeServer.getServerPort(),
                mockESPHomeUpdateServer.getHost() + ":" + mockESPHomeUpdateServer.getServerPort(),
                blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort(),
                blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort(),
                "test", "test",
                postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword(),
                "http://" + mockWacnetServer.getHost() + ":" + mockWacnetServer.getServerPort() + "/api/v1/bacnet");
        PropertiesFileHelper.writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
    }

    @Before
    public void setUp() throws IOException {
        try {
            mockESPHomeServer.start();
            mockESPHomeUpdateServer.start();
            mockWacnetServer.start();
            blazegraph.start();
            postgres.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }

        preparePropertiesFile();

        BlazegraphHelper.createNewNameSpace(namespace, folder, httpClient, getBlazegraphEndPoint());
        BlazegraphHelper.createNewData("initTemperature.xml", getNamespaceUrl(namespace), httpClient);

        TriplePattern updatePattern = iri(test_IRI).has(hasBacnetDeviceID, "123456").andHas(hasBacnetObjectID, "123").andHas(hasNumericalValue, "0.0");
        InsertDataQuery insert = Queries.INSERT_DATA(updatePattern);
        insert.prefix(PREFIX_ONTOBMS,PREFIX_OM);
        
        // Set endpoint to the triple store. The host and port are read from the container
        String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        endpoint = endpoint + "/blazegraph/namespace/test/sparql";

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

    @After
    public void tearDown() {
        mockESPHomeServer.close();
        mockESPHomeUpdateServer.close();
        mockWacnetServer.close();
        blazegraph.close();
        postgres.close();
    }

    @Test
    public void testExecuteSetSuccess() throws Exception {
        try (MockServerClient espHome = new MockServerClient(mockESPHomeServer.getHost(), mockESPHomeServer.getServerPort());
             MockServerClient espHomeUpdate = new MockServerClient(mockESPHomeUpdateServer.getHost(), mockESPHomeUpdateServer.getServerPort())) {
            setupMockESPHomeClient(espHome);
            setupMockESPUpdateClient(espHomeUpdate);

            double temperature = 24.0;
            withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        JSONObject result = launcher.processRequestParameters(getSetRouteInput(temperature), createHttpServletRequest("set"));
                        assertEquals("The temperature has been set to " + temperature, result.getString("message"));
                        assertEquals("The fan is in the ON state.", result.getString("fanStatus"));
                    });
        }
    }

    @Test
    public void testExecuteSet_FailAtUpdatingKG() throws Exception {
        try (MockServerClient espHome = new MockServerClient(mockESPHomeServer.getHost(), mockESPHomeServer.getServerPort());
             MockServerClient espHomeUpdate = new MockServerClient(mockESPHomeUpdateServer.getHost(), mockESPHomeUpdateServer.getServerPort())) {
            setupMockESPHomeClient(espHome);
            setupMockESPUpdateClient(espHomeUpdate);

            double temperature = 24.0;
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("dataIRI", "https://www.theworldavatar.com/kg/ontodevice/wrong_data_iri");
            jsonObject.put("temperature", temperature);
            jsonObject.put("clientProperties", "CLIENT_PROPERTIES");

            withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        JSONObject result = launcher.processRequestParameters(jsonObject, createHttpServletRequest("set"));
                        assertEquals("Error occurred", result.getString("message"));
                    });
        }
    }

    @Test
    public void testExecuteSet_FailAtESPHomeAgent() throws Exception {
        try (MockServerClient espHomeUpdate = new MockServerClient(mockESPHomeUpdateServer.getHost(), mockESPHomeUpdateServer.getServerPort())) {
            setupMockESPUpdateClient(espHomeUpdate);

            withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        double temperature = 24;
                        double initialTemperature = 26; // hardcoded in initTemperature.xml
                        JSONObject result = launcher.processRequestParameters(getSetRouteInput(temperature), createHttpServletRequest("set"));
                        assertEquals("Error occurred; Set point has been reset to " + initialTemperature, result.getString("message"));

                        checkKGRecover(initialTemperature);
                    });
        }
    }

    @Test
    public void testExecuteSet_FailAtESPHomeUpdateAgent() throws Exception {
        try (MockServerClient espHome = new MockServerClient(mockESPHomeServer.getHost(), mockESPHomeServer.getServerPort())) {
            setupMockESPHomeClient(espHome);

            withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        double temperature = 24;
                        double initialTemperature = 26; // hardcoded in initTemperature.xml
                        JSONObject result = launcher.processRequestParameters(getSetRouteInput(temperature), createHttpServletRequest("set"));
                        assertTrue(result.getString("message").contains("Error occurred; Set point has been reset to " + initialTemperature + ", and the fan status is "));
                        checkKGRecover(initialTemperature);
                    });
        }
    }

    @Test
    public void testExecuteWriteWacnet() throws Exception {
                try (MockServerClient wacnet = new MockServerClient(mockWacnetServer.getHost(), mockWacnetServer.getServerPort())) {
            setupMockWacnetClient(wacnet);
            withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString()).and("WACNET_API_PROPERTIES",Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        JSONObject result = launcher.processRequestParameters(getWriteRouteInput(null, null, 1.0), createHttpServletRequest("wacnet/write"));
                        Assert.assertEquals("Successfully written 1.0 to the object with an ID: 123", result.getString("message"));
                    }
                    );
            withEnvironmentVariable("WACNET_API_PROPERTIES",Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        JSONObject result = launcher.processRequestParameters(getWriteRouteInput("123456", "123", 1.0), createHttpServletRequest("wacnet/write"));
                        Assert.assertEquals("Successfully written 1.0 to the object with an ID: 123", result.getString("message"));
                    }
                    );
       }
    }

    @Test
    public void testExecuteUpdateTriples() throws Exception {
        withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                .execute(() -> {
                        //test INSERT only where latest timeseries value != triggerValue
                        JSONObject result = launcher.processRequestParameters(getUpdateTriplesRouteInput("<example:prefix/api_test> <example:testing> \"example\". ", null, "0.0"), createHttpServletRequest("updateTriples"));
                        Assert.assertTrue(result.getJSONObject("message0").getString("message").contains("The latest timeseries value of example:prefix/api_test is not equivalent to the trigger value 0.0"));
                        //test INSERT only where latest timeseries value == triggerValue
                        result = launcher.processRequestParameters(getUpdateTriplesRouteInput("<example:prefix/api_test> <example:testing> \"example\". ", null, "1.0"), createHttpServletRequest("updateTriples"));
                        Assert.assertTrue(result.getJSONObject("message0").getString("message").contains("Executed the following to the knowledge graph: INSERT DATA"));
                        Variable var = SparqlBuilder.var("var");
                        SelectQuery query = Queries.SELECT();
                        //create triple pattern
                        TriplePattern queryPattern = var.has(iri("example:testing"), "example");
                        query.select(var).where(queryPattern);
                        kbClient.setQuery(query.getQueryString());
                        JSONArray queryResult = kbClient.executeQuery();
                        Assert.assertEquals("example:prefix/api_test", queryResult.getJSONObject(0).getString("var"));

                        //test DELETE only where latest timeseries value != triggerValue
                        result = launcher.processRequestParameters(getUpdateTriplesRouteInput(null, "<example:prefix/api_test> <example:testing> \"example\". ", "0.0"), createHttpServletRequest("updateTriples"));
                        Assert.assertTrue(result.getJSONObject("message0").getString("message").contains("The latest timeseries value of example:prefix/api_test is not equivalent to the trigger value 0.0"));
                        //test DELETE only where latest timeseries value == triggerValue
                        result = launcher.processRequestParameters(getUpdateTriplesRouteInput(null, "<example:prefix/api_test> <example:testing> \"example\". ", "1.0"), createHttpServletRequest("updateTriples"));
                        Assert.assertTrue(result.getJSONObject("message0").getString("message").contains("Executed the following to the knowledge graph: DELETE DATA"));
                        //query should return an error as the triples is deleted
                        try {
                            queryResult = kbClient.executeQuery();
                        } catch (JPSRuntimeException e) {
                            Assert.assertTrue(e.getMessage().contains("error when executing SPARQL query:"));
                        }

                        //test INSERT and DELETE where latest timeseries value != triggerValue
                        result = launcher.processRequestParameters(getUpdateTriplesRouteInput("<example:prefix/api_test> <example:testing> \"example\". ", "<example:prefix/api_test> <https://www.theworldavatar.com/kg/ontobms/hasBacnetDeviceID> \"123456\". ", "0.0"), createHttpServletRequest("updateTriples"));
                        Assert.assertTrue(result.getJSONObject("message0").getString("message").contains("The latest timeseries value of example:prefix/api_test is not equivalent to the trigger value 0.0"));
                        //test INSERT and DELETE where latest timeseries value == triggerValue
                        result = launcher.processRequestParameters(getUpdateTriplesRouteInput("<example:prefix/api_test> <example:testing> \"example\". ", "<example:prefix/api_test> <https://www.theworldavatar.com/kg/ontobms/hasBacnetDeviceID> \"123456\". ", "1.0"), createHttpServletRequest("updateTriples"));
                        Assert.assertTrue(result.getJSONObject("message0").getJSONArray("message").getString(0).contains("Executed the following to the knowledge graph: INSERT DATA"));
                        Assert.assertTrue(result.getJSONObject("message0").getJSONArray("message").getString(1).contains("Executed the following to the knowledge graph: DELETE DATA"));
                        queryResult = kbClient.executeQuery();
                        Assert.assertEquals("example:prefix/api_test", queryResult.getJSONObject(0).getString("var"));

                        queryPattern = iri(test_IRI).has(hasBacnetDeviceID,var);
                        query.select(var).where(queryPattern);
                        kbClient.setQuery(query.getQueryString());
                        try {
                            queryResult = kbClient.executeQuery();
                        } catch (JPSRuntimeException e) {
                            Assert.assertTrue(e.getMessage().contains("error when executing SPARQL query:"));
                        }
                    }
                    );
       }

    @Test
    public void testExecuteUpdatePresentValue() throws Exception {
        try (MockServerClient wacnet = new MockServerClient(mockWacnetServer.getHost(), mockWacnetServer.getServerPort())) {
            setupMockWacnetClient(wacnet);
            withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString()).and("WACNET_API_PROPERTIES",Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        JSONObject result = launcher.processRequestParameters(getUpdatePresentValueInput(), createHttpServletRequest("updatePresentValue"));
                        //Assert.assertEquals("test", result.getJSONObject("message0").toString());
                        Assert.assertTrue(result.getJSONObject("message0").getString("message").contains("Sucessfully executed the following query string:"));

                        Variable var = SparqlBuilder.var("var");
                        SelectQuery query = Queries.SELECT();
                        //create triple pattern
                        TriplePattern queryPattern = iri(test_IRI).has(hasNumericalValue, var);
                        query.select(var).where(queryPattern).prefix(PREFIX_OM);
                        kbClient.setQuery(query.getQueryString());
                        JSONArray queryResult = kbClient.executeQuery();
                        Assert.assertEquals("1.0", queryResult.getJSONObject(0).getString("var"));
                    }
                    );
        }
    }

    private JSONObject getESPHomeInput() {
        JSONObject expectedBody = new JSONObject();
        expectedBody.put("timeseriesDataClientProperties", "CLIENT_PROPERTIES");
        expectedBody.put("esphomeStatusClientProperties", "ESPHOME_CLIENT_PROPERTIES");
        expectedBody.put("esphomeAPIProperties", "API_PROPERTIES");

        return expectedBody;
    }

    private JSONObject getESPHomeUpdateInput() {
        JSONObject expectedBody = new JSONObject();
        expectedBody.put("agentProperties", "ESPHOME_UPDATE_AGENTPROPERTIES");
        expectedBody.put("apiProperties", "ESPHOME_UPDATE_APIPROPERTIES");
        expectedBody.put("clientProperties", "ESPHOME_UPDATE_CLIENTPROPERTIES");

        return expectedBody;
    }

    private JSONObject getSetRouteInput(double temperature) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("dataIRI", "https://www.theworldavatar.com/kg/ontodevice/mock_setpoint");
        jsonObject.put("temperature", temperature);
        jsonObject.put("clientProperties", "CLIENT_PROPERTIES");
        return jsonObject;
    }

    private JSONObject getWriteRouteInput(String bacnetDeviceID, String bacnetObjectID, Double value) {
        JSONObject jsonObject = new JSONObject();
        if (bacnetDeviceID == null && bacnetObjectID == null) {
            jsonObject.put("dataIRI", test_IRI);
            jsonObject.put("value", value);
            jsonObject.put("clientProperties", "CLIENT_PROPERTIES");
        } else {
            jsonObject.put("value", value);
            jsonObject.put("bacnetObjectId", bacnetObjectID);
            jsonObject.put("bacnetDeviceId", bacnetDeviceID);
        }
        return jsonObject;
    }

    private JSONObject getUpdateTriplesRouteInput(String insertString, String deleteString, String value) {
        JSONObject jsonObject = new JSONObject();
        JSONArray checks = new JSONArray();
        JSONObject properties = new JSONObject();
        properties.put("dataIRI", test_IRI);
        properties.put("triggerValue", value);
        properties.put("clientProperties", "CLIENT_PROPERTIES");
        if (insertString != null) {
            properties.put("INSERT", insertString);
        }
        if (deleteString != null) {
            properties.put("DELETE", deleteString);
        }
        checks.put(properties);
        jsonObject.put("checks", checks);
        return jsonObject;
    }

    private JSONObject getUpdatePresentValueInput() {
        JSONObject jsonObject = new JSONObject();
        JSONArray checks = new JSONArray();
        JSONObject properties = new JSONObject();
        properties.put("dataIRI", test_IRI);
        properties.put("clientProperties", "CLIENT_PROPERTIES");
        checks.put(properties);
        jsonObject.put("checks", checks);
        return jsonObject;
    }

    private String getBlazegraphEndPoint() {
        return "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort() + "/blazegraph";
    }

    private String getNamespaceUrl(String namespace) {
        return getBlazegraphEndPoint() + "/namespace/" + namespace + "/sparql";
    }

    private void checkKGRecover(double initialTemperature) throws IOException {
        try (CloseableHttpClient client = HttpClients.createDefault()) {
            HttpPost post = new HttpPost(getNamespaceUrl("test"));

            List<NameValuePair> params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("query", "SELECT * { <https://www.theworldavatar.com/kg/ontodevice/mock_setpoint> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue>  ?temperature }"));
            UrlEncodedFormEntity entity = new UrlEncodedFormEntity(params, "UTF-8");
            post.setEntity(entity);
            post.setHeader("Accept", "application/json");

            CloseableHttpResponse response = client.execute(post);
            JSONObject jo = (JSONObject) new JSONObject(EntityUtils.toString(response.getEntity()))
                    .getJSONObject("results").getJSONArray("bindings").get(0);

            assertEquals(initialTemperature, jo.getJSONObject("temperature").getDouble("value"), 0.0001);
        }
    }

    private void setupMockESPHomeClient(MockServerClient espHome) {
        espHome.when(request().withPath("/esphome-agent/toggle").withBody(getESPHomeInput().toString()))
                .respond(response().withBody("{\"message\":[\"The component is already in the ON state.\",\"A request has been successfully sent to the ESPHome web server.\",\"POST request has been sent successfully.\"]}"));
    }

    private void setupMockWacnetClient(MockServerClient wacnet) {
        JSONObject body = new JSONObject();
        JSONObject presentValue = new JSONObject();
        //"{\"properties\":{\"present-value\":\"700\"}}"
        presentValue.put("present-value", 1.0); 
        body.put("properties", presentValue);
        wacnet.when(request().withMethod("PUT").withPath("/api/v1/bacnet/devices/123456/objects/123").withBody(body.toString()))
        .respond(response().withStatusCode(200).withBody("{\"sucess\":\"true\"}"));

        wacnet.when(request().withMethod("GET").withPath("/api/v1/bacnet/devices/123456/objects/123"))
        .respond(response().withStatusCode(200).withBody("{\"present-value\":" + 1.0 + "}"));
    }

    private void setupMockESPUpdateClient(MockServerClient espHomeUpdate) {
        espHomeUpdate.when(request().withPath("/esphome-update-agent/retrieve").withBody(getESPHomeUpdateInput().toString()))
                .respond(response().withBody("{\"Result\":[\"Input agent object initialized.\",\"Time series client object initialized.\",\"API connector object initialized.\",\"Retrieved status of component.\",\"Data updated with new readings from API.\",\"Timeseries Data has been updated.\"]}"));
    }

    private HttpServletRequest createHttpServletRequest(String route) {
        MockHttpServletRequest request = new MockHttpServletRequest("POST", "/" + route);
        return request;
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
