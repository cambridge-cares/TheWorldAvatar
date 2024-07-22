package uk.ac.cam.cares.jps.agent.pips;

import org.apache.http.HttpHeaders;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockserver.client.MockServerClient;
import org.mockserver.model.Parameters;
import org.testcontainers.containers.MockServerContainer;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockserver.model.HttpRequest.request;
import static org.mockserver.model.HttpResponse.response;
import static uk.org.webcompere.systemstubs.SystemStubs.withEnvironmentVariable;

@Testcontainers
public class PIPSTSAgentAPIConnectorTest {

    public static final DockerImageName MOCKSERVER_IMAGE = DockerImageName
            .parse("mockserver/mockserver")
            .withTag("mockserver-" + MockServerClient.class.getPackage().getImplementationVersion());

    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();
    public static MockServerContainer mockWacnetServer = new MockServerContainer(MOCKSERVER_IMAGE);
    private static PIPSTSAgentAPIConnector pipsTsAgentAPIConnector;

    @Before
    public void setUp() throws IOException {
        try {
            mockWacnetServer.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }
    }

    @After
    public void tearDown() throws InterruptedException {
        mockWacnetServer.close();
    }

    private void setupMockPIPSTSAgent(MockServerClient agent) {
        Map<String, List<String>> entries = new HashMap<>();
        ArrayList<String> source = new ArrayList<String>();
        source.add("test");
        ArrayList<String> num = new ArrayList<String>();
        num.add("1");
        entries.put("source", source);
        entries.put("num", num);
        agent.when(request().withMethod("GET").withPath("/pips-timeseries-agent/timeseries").withQueryStringParameters(new Parameters().withEntries(entries)).withHeader(HttpHeaders.AUTHORIZATION, "Bearer test_token"))
        .respond(response().withStatusCode(200).withBody("{\"test_module\":[{\"temperature_A\":10.0, \"timestamp\": \"2024-07-09T16:41:53+08:00\"}]}"));

        Map<String, List<String>> incorrectEntries = new HashMap<>();
        ArrayList<String> incorrectSource = new ArrayList<String>();
        source.add("wrong_source");
        incorrectEntries.put("source", incorrectSource);
        incorrectEntries.put("num", num);

        agent.when(request().withMethod("GET").withPath("/pips-timeseries-agent/timeseries").withQueryStringParameters(new Parameters().withEntries(incorrectEntries)).withHeader(HttpHeaders.AUTHORIZATION, "Bearer test_token"))
        .respond(response().withStatusCode(200).withBody("{\"Result\":\"No data retrieved...\"}"));
    }

    @Test
    public void testGetTimeSeriesSuccess() throws Exception {
        try (MockServerClient pipsTsAgent = new MockServerClient(mockWacnetServer.getHost(), mockWacnetServer.getServerPort())) {
            setupMockPIPSTSAgent(pipsTsAgent);
            withEnvironmentVariable("PIPS_AGENT_TIMESERIES_PATH", "http://" + mockWacnetServer.getHost() + ":" + mockWacnetServer.getServerPort() + "/pips-timeseries-agent/timeseries")
            .execute(() -> {
                JSONObject result = new JSONObject();
                pipsTsAgentAPIConnector = new PIPSTSAgentAPIConnector();
                result = pipsTsAgentAPIConnector.getTimeSeries("test_token", "test", 1, false);
                Assert.assertEquals("2024-07-09T16:41:53+08:00", result.getJSONArray("test_module").getJSONObject(0).get("timestamp"));
            }
            );
        }
    }

    @Test
    public void testGetTimeSeriesFail() throws Exception {
        try (MockServerClient pipsTsAgent = new MockServerClient(mockWacnetServer.getHost(), mockWacnetServer.getServerPort())) {
            setupMockPIPSTSAgent(pipsTsAgent);
            withEnvironmentVariable("PIPS_AGENT_TIMESERIES_PATH", "http://" + mockWacnetServer.getHost() + ":" + mockWacnetServer.getServerPort() + "/pips-timeseries-agent/timeseries")
            .execute(() -> {
                JSONObject result = new JSONObject();
                pipsTsAgentAPIConnector = new PIPSTSAgentAPIConnector();
                result = pipsTsAgentAPIConnector.getTimeSeries("test_token", "wrong_source", 1, false);
                Assert.assertEquals("No data retrieved...", result.getString("Result"));
            }
            );
        }
    }
}
