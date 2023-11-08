package uk.ac.cam.cares.jps.agent.dashboard;

import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.json.JSONObject;
import org.junit.jupiter.api.*;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import uk.ac.cam.cares.jps.agent.dashboard.json.DashboardClient;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

class DashboardAgentTest {
    private static DashboardAgent agent;
    private static final String KEY_METHOD = "method";
    private static final JsonPrimitive GET_METHOD = new JsonPrimitive("GET");
    private static final JsonPrimitive POST_METHOD = new JsonPrimitive("POST");
    private static final String KEY_ROUTE = "requestUrl";
    private static final String BASE_ROUTE = "http://localhost:3067/dashboard-agent/";
    private static final JsonPrimitive JSON_BASE_ROUTE = new JsonPrimitive(BASE_ROUTE);

    private static final JsonPrimitive STATUS_ROUTE = new JsonPrimitive(BASE_ROUTE + "status");
    private static final JsonPrimitive SETUP_ROUTE = new JsonPrimitive(BASE_ROUTE + "setup");

    @BeforeEach
    void setup() {
        agent = new DashboardAgent();
    }

    @Test
    void testProcessRequestParameters_UndefinedRoute() {
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, GET_METHOD);
        requestParams.add(KEY_ROUTE, JSON_BASE_ROUTE);
        JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
        assertEquals("Invalid route! Requested route does not exist for : ", response.getString("Result"));
    }

    @Test
    void testProcessRequestParameters_StatusRouteViaGET() {
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, GET_METHOD);
        requestParams.add(KEY_ROUTE, STATUS_ROUTE);
        JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
        assertEquals("Agent is ready to receive requests.", response.getString("Result"));
    }

    @Test
    void testProcessRequestParameters_StatusRouteViaInvalidPOST() {
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, POST_METHOD);
        requestParams.add(KEY_ROUTE, STATUS_ROUTE);
        JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
        assertEquals("Invalid request type! Route status can only accept GET request.", response.getString("Result"));
    }

    @Test
    void testProcessRequestParameters_SetupRoute() throws IOException {
        // Set up params
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, GET_METHOD);
        requestParams.add(KEY_ROUTE, SETUP_ROUTE);
        File config = TestUtils.genSampleCredFile(true, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);

        // Mock the bridge object, as it cannot be unit tested and requires integration test
        try (MockedConstruction<DashboardClient> mockDashboardClient = Mockito.mockConstruction(DashboardClient.class,
                (mock, context) -> {
                    Mockito.when(mock.initDashboard()).thenAnswer((Answer<Void>) invocation -> null);
                })) {
            // Execute method
            JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
            assertEquals("Dashboard has been successfully set up!", response.getString("Result"));
        } finally {
            config.delete();
        }
    }

    @Test
    void testProcessRequestParameters_SetupRouteViaInvalidPOST() {
        // Set up request parameters
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, POST_METHOD);
        requestParams.add(KEY_ROUTE, SETUP_ROUTE);
        JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
        assertEquals("Invalid request type! Route setup can only accept GET request.", response.getString("Result"));
    }
}