package uk.ac.cam.cares.jps.agent.pips;

import org.apache.http.HttpHeaders;
import org.json.JSONObject;
import org.junit.*;
import org.mockserver.client.MockServerClient;
import org.mockserver.model.Parameters;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.mockserver.model.HttpRequest.request;
import static org.mockserver.model.HttpResponse.response;
import static uk.org.webcompere.systemstubs.SystemStubs.withEnvironmentVariable;

public class PIPSTSAgentAPIConnectorTest {

    private static final String MOCKSERVER_HOST = "test_pips_request_agent-mockserver-1";
    private static final int MOCKSERVER_PORT = 1080;

    /**
     * sleep to allow for MockServer container to finish setting up before beginning tests
     * @throws InterruptedException
     */
    @Before
    public void sleep() throws InterruptedException {
        TimeUnit.SECONDS.sleep(10);
    }

    /**
     * Set up mock PIPSTSAgent with expectations matching that of the actual PIPSTSAgent
     * @param agent
     */
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
    }

    /**
     * Set up mock PIPSTSAgent with expectations where a request containing incorrect parameters will return back a "No data retrieved..." error message
     * @param agent
     */
    private void setupMockPIPSTSAgentForFailed(MockServerClient agent) throws InterruptedException {
        Map<String, List<String>> incorrectEntries = new HashMap<>();
        ArrayList<String> incorrectSource = new ArrayList<String>();
        ArrayList<String> source = new ArrayList<String>();
        source.add("wrong_source");
        ArrayList<String> num = new ArrayList<String>();
        num.add("1");
        incorrectEntries.put("source", incorrectSource);
        incorrectEntries.put("num", num);

        agent.when(request().withMethod("GET").withPath("/pips-timeseries-agent/timeseries").withQueryStringParameters(new Parameters().withEntries(incorrectEntries)).withHeader(HttpHeaders.AUTHORIZATION, "Bearer test_token"))
        .respond(response().withStatusCode(200).withBody("{\"Result\":\"No data retrieved...\"}"));
    }

    /**
     * Successful test for PIPSTSAgentAPIConnector.getTimeSeries
     * @throws Exception
     */
    @Test
    public void testGetTimeSeriesSuccess() throws Exception {
        try (MockServerClient pipsTsAgent = new MockServerClient(MOCKSERVER_HOST, MOCKSERVER_PORT)) {
            setupMockPIPSTSAgent(pipsTsAgent);
            withEnvironmentVariable("PIPS_AGENT_TIMESERIES_PATH", "http://" + MOCKSERVER_HOST + ":" + String.valueOf(MOCKSERVER_PORT) + "/pips-timeseries-agent/timeseries")
            .execute(() -> {
                JSONObject result = new JSONObject();
                PIPSTSAgentAPIConnector pipsTsAgentAPIConnector = new PIPSTSAgentAPIConnector();
                result = pipsTsAgentAPIConnector.getTimeSeries("test_token", "test", 1, false);
                Assert.assertEquals("2024-07-09T16:41:53+08:00", result.getJSONArray("test_module").getJSONObject(0).get("timestamp"));
            }
            );
        }
    }

    /**
     * Failed test for PIPSTSAgentAPIConnector.getTimeSeries
     * @throws Exception
     */
    @Test
    public void testGetTimeSeriesFail() throws Exception {
        try (MockServerClient pipsTsAgent = new MockServerClient(MOCKSERVER_HOST, MOCKSERVER_PORT)) {
            setupMockPIPSTSAgentForFailed(pipsTsAgent);
            withEnvironmentVariable("PIPS_AGENT_TIMESERIES_PATH", "http://" + MOCKSERVER_HOST + ":" + String.valueOf(MOCKSERVER_PORT) + "/pips-timeseries-agent/timeseries")
            .execute(() -> {
                JSONObject result = new JSONObject();
                PIPSTSAgentAPIConnector pipsTsAgentAPIConnector = new PIPSTSAgentAPIConnector();
                result = pipsTsAgentAPIConnector.getTimeSeries("test_token", "wrong_source", 1, false);
                Assert.assertEquals("No data retrieved...", result.getString("Result"));
            }
            );
        }
    }
}
