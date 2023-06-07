package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockserver.integration.ClientAndServer;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.agent.bmsupdate.helper.BlazegraphHelper;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.IOException;

import static org.mockserver.model.HttpRequest.request;
import static org.mockserver.model.HttpResponse.response;

@Ignore("Ignore because it causes error when building docker image, but works fine with mvn test." +
        "Requires triple store endpoint set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
@Testcontainers
public class BMSUpdateAgentIntegrationTest {
    @Container
    private static final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    private static BMSUpdateAgent agent;
    private static CloseableHttpClient httpClient;
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeClass
    public static void setUpBeforeClass() {
        try {
            blazegraph.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }
        agent = new BMSUpdateAgent();
        httpClient = HttpClients.createDefault();

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


}
