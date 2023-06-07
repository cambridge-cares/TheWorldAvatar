package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockserver.client.MockServerClient;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.MockServerContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.agent.bmsupdate.helper.BlazegraphHelper;
import uk.ac.cam.cares.jps.agent.bmsupdate.helper.PropertiesFileHelper;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

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
    private static final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();
    @ClassRule
    public static MockServerContainer mockESPHomeServer = new MockServerContainer(MOCKSERVER_IMAGE);
    @ClassRule
    public static MockServerContainer mockESPHomeUpdateServer = new MockServerContainer(MOCKSERVER_IMAGE);
    private static BMSUpdateAgentLauncher launcher;
    private static CloseableHttpClient httpClient;

    String namespace = "test";

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
                blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort());
        PropertiesFileHelper.writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
    }

    @Before
    public void setUp() throws IOException {
        try {
            mockESPHomeServer.start();
            mockESPHomeUpdateServer.start();
            blazegraph.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }

        preparePropertiesFile();

        BlazegraphHelper.createNewNameSpace(namespace, folder, httpClient, getBlazegraphEndPoint());
        BlazegraphHelper.createNewData("initTemperature.xml", getNamespaceUrl(namespace), httpClient);
    }

    @After
    public void tearDown() {
        mockESPHomeServer.close();
        mockESPHomeUpdateServer.close();
        blazegraph.close();
    }

    @Test
    public void testLauncherProcessRequestParam() throws Exception {
        try (MockServerClient espHome = new MockServerClient(mockESPHomeServer.getHost(), mockESPHomeServer.getServerPort());
             MockServerClient espHomeUpdate = new MockServerClient(mockESPHomeUpdateServer.getHost(), mockESPHomeUpdateServer.getServerPort())) {
            setupMockESPHomeClient(espHome);
            setupMockESPUpdateClient(espHomeUpdate);

            double temperature = 24.0;
            withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        JSONObject result = launcher.processRequestParameters(getLauncherInput(temperature));
                        assertEquals("The temperature has been set to " + temperature, result.getString("message"));
                        assertEquals("The fan is in the ON state.", result.getString("fanStatus"));
                    });
        }

    }

    @Test
    public void testLauncherProcessRequestParam_FailAtUpdatingKG() throws Exception {
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
                        JSONObject result = launcher.processRequestParameters(jsonObject);
                        assertEquals("Error occurred", result.getString("message"));
                    });
        }
    }

    @Test
    public void testLauncherProcessRequestParam_FailAtESPHomeAgent() throws Exception {
        try (MockServerClient espHomeUpdate = new MockServerClient(mockESPHomeUpdateServer.getHost(), mockESPHomeUpdateServer.getServerPort())) {
            setupMockESPUpdateClient(espHomeUpdate);

            withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        double temperature = 24;
                        double initialTemperature = 26; // hardcoded in initTemperature.xml
                        JSONObject result = launcher.processRequestParameters(getLauncherInput(temperature));
                        assertEquals("Error occurred; Set point has been reset to " + initialTemperature, result.getString("message"));

                        checkKGRecover(initialTemperature);
                    });
        }
    }

    @Test
    public void testLauncherProcessRequestParam_FailAtESPHomeUpdateAgent() throws Exception {
        try (MockServerClient espHome = new MockServerClient(mockESPHomeServer.getHost(), mockESPHomeServer.getServerPort())) {
            setupMockESPHomeClient(espHome);

            withEnvironmentVariable("CLIENT_PROPERTIES", Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        double temperature = 24;
                        double initialTemperature = 26; // hardcoded in initTemperature.xml
                        JSONObject result = launcher.processRequestParameters(getLauncherInput(temperature));
                        assertTrue(result.getString("message").contains("Error occurred; Set point has been reset to " + initialTemperature + ", and the fan status is "));
                        checkKGRecover(initialTemperature);
                    });
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

    private JSONObject getLauncherInput(double temperature) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("dataIRI", "https://www.theworldavatar.com/kg/ontodevice/mock_setpoint");
        jsonObject.put("temperature", temperature);
        jsonObject.put("clientProperties", "CLIENT_PROPERTIES");
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

    private void setupMockESPUpdateClient(MockServerClient espHomeUpdate) {
        espHomeUpdate.when(request().withPath("/esphome-update-agent/retrieve").withBody(getESPHomeUpdateInput().toString()))
                .respond(response().withBody("{\"Result\":[\"Input agent object initialized.\",\"Time series client object initialized.\",\"API connector object initialized.\",\"Retrieved status of component.\",\"Data updated with new readings from API.\",\"Timeseries Data has been updated.\"]}"));
    }
}
