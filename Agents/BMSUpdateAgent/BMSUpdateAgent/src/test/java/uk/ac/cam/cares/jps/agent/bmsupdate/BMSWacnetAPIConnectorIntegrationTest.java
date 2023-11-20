package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockserver.client.MockServerClient;
import org.testcontainers.containers.MockServerContainer;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.agent.bmsupdate.helper.PropertiesFileHelper;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import static org.mockserver.model.HttpRequest.request;
import static org.mockserver.model.HttpResponse.response;
import static uk.org.webcompere.systemstubs.SystemStubs.withEnvironmentVariable;

@Ignore("Ignore because it causes error when building docker image, but works fine with mvn test." +
        "Requires triple store endpoint set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
@Testcontainers
public class BMSWacnetAPIConnectorIntegrationTest {

    public static final DockerImageName MOCKSERVER_IMAGE = DockerImageName
            .parse("mockserver/mockserver")
            .withTag("mockserver-" + MockServerClient.class.getPackage().getImplementationVersion());

    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();
    public static MockServerContainer mockWacnetServer = new MockServerContainer(MOCKSERVER_IMAGE);
    private static CloseableHttpClient httpClient;
    private static BMSWacnetAPIConnector bmsWacnetAPIConnector;

    @BeforeClass
    static public void setUpBeforeClass() {
        httpClient = HttpClients.createDefault();
    }

    @AfterClass
    public static void tearDownAfterClass() throws IOException {
        httpClient.close();
    }

    private static void preparePropertiesFile() throws IOException {
        List<String> lines = PropertiesFileHelper.readClientPropertiesTemplateFile("place_holder",
                "place_holder",
                "place_holder",
                "place_holder",
                "place_holder", "place_holder",
                "place_holder", "place_holder", "place_holder",
                "http://" + mockWacnetServer.getHost() + ":" + mockWacnetServer.getServerPort() + "/api/v1/bacnet");
        PropertiesFileHelper.writeToTempFolder(Paths.get(folder.getRoot().toString(), "client.properties").toString(), lines);
    }

    @Before
    public void setUp() throws IOException {
        try {
            mockWacnetServer.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }

        preparePropertiesFile();
    }

    @After
    public void tearDown() {
        mockWacnetServer.close();
    }

    @Test
    public void testWritePresentValue() throws Exception {
        try (MockServerClient wacnet = new MockServerClient(mockWacnetServer.getHost(), mockWacnetServer.getServerPort())) {
            setupMockWacnetClient(wacnet);
            withEnvironmentVariable("WACNET_API_PROPERTIES",Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        JSONObject result = new JSONObject();
                        bmsWacnetAPIConnector = new BMSWacnetAPIConnector(System.getenv("WACNET_API_PROPERTIES"));
                        result = bmsWacnetAPIConnector.writePresentValue("123456", "123", 1.0);
                        Assert.assertEquals("Successfully written 1.0 to the object with an ID: 123", result.getString("message"));
                    }
                    );
        }
    }

    @Test
    public void testReadPresentValue() throws Exception {
        try (MockServerClient wacnet = new MockServerClient(mockWacnetServer.getHost(), mockWacnetServer.getServerPort())) {
            setupMockWacnetClient(wacnet);
            withEnvironmentVariable("WACNET_API_PROPERTIES",Paths.get(folder.getRoot().toString(), "client.properties").toString())
                    .execute(() -> {
                        bmsWacnetAPIConnector = new BMSWacnetAPIConnector(System.getenv("WACNET_API_PROPERTIES"));
                        Double value = bmsWacnetAPIConnector.readPresentValue("123456", "123");
                        Assert.assertEquals("1.0", value.toString());
                    }
                    );
        }
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
}
