package uk.ac.cam.cares.jps.agent.dashboard;

import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.json.JSONObject;
import org.junit.jupiter.api.*;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;
import uk.ac.cam.cares.jps.agent.dashboard.json.DashboardClient;
import uk.ac.cam.cares.jps.agent.dashboard.stack.StackClient;

import javax.servlet.ServletException;
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
    private static final JsonPrimitive RESET_ROUTE = new JsonPrimitive(BASE_ROUTE + "reset");
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
        assertEquals("Invalid route! Requested route does not exist for : ", response.getString("Error"));
    }

    @Test
    void testProcessRequestParameters_ResetRouteViaGET() throws ServletException {
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, GET_METHOD);
        requestParams.add(KEY_ROUTE, RESET_ROUTE);
        // Mock the client
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class)) {
            agent.init();
            // Execute method
            JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
            assertEquals("Agent has been successfully reset!", response.getString("Result"));
        }
    }

    @Test
    void testProcessRequestParameters_ResetRouteViaInvalidPOST() {
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, POST_METHOD);
        requestParams.add(KEY_ROUTE, RESET_ROUTE);
        JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
        assertEquals("Invalid request type! Route reset can only accept GET request.", response.getString("Error"));
    }

    @Test
    void testProcessRequestParameters_StatusRouteViaGET() throws ServletException {
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, GET_METHOD);
        requestParams.add(KEY_ROUTE, STATUS_ROUTE);
        // Mock the client
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class)) {
            agent.init();
            // Execute method
            JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
            assertEquals("Agent is ready to receive requests.", response.getString("Result"));
        }
    }

    @Test
    void testProcessRequestParameters_StatusRouteViaInvalidPOST() {
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, POST_METHOD);
        requestParams.add(KEY_ROUTE, STATUS_ROUTE);
        JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
        assertEquals("Invalid request type! Route status can only accept GET request.", response.getString("Error"));
    }

    @Test
    void testProcessRequestParameters_SetupRoute() throws IOException, ServletException {
        // Set up params
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, GET_METHOD);
        requestParams.add(KEY_ROUTE, SETUP_ROUTE);
        File config = TestUtils.genSampleCredFile(true, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class);
             MockedConstruction<DashboardClient> mockDashboardClient = Mockito.mockConstruction(DashboardClient.class,
                (mock, context) -> {
                    Mockito.when(mock.initDashboard()).thenAnswer((Answer<Void>) invocation -> null);
                })) {
            agent.init();
            // Execute method
            JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
            assertEquals("Dashboard has been successfully set up!", response.getString("Result"));
        } finally {
            config.delete();
        }
    }

    @Test
    void testProcessRequestParameters_SetupRouteInvalidGrafanaJson() throws IOException {
        // Set up params
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, GET_METHOD);
        requestParams.add(KEY_ROUTE, SETUP_ROUTE);
        File config = TestUtils.genSampleCredFile(true, IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        String expectedError = "Bad request data! The json model is not compliant with Grafana standards!";
        try (MockedConstruction<DashboardClient> mockDashboardClient = Mockito.mockConstruction(DashboardClient.class,
                // Throw an exception
                (mock, context) -> {
                    Mockito.when(mock.initDashboard()).thenThrow(new IllegalArgumentException(expectedError));
                })) {
            // Execute method
            JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
            // Verify that the error was received and put into the response
            assertEquals("Dashboard could not be set up! " + expectedError, response.getString("Error"));
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
        assertEquals("Invalid request type! Route setup can only accept GET request.", response.getString("Error"));
    }
}