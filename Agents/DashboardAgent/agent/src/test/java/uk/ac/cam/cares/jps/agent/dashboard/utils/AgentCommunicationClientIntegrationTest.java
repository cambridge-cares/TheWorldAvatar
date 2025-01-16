package uk.ac.cam.cares.jps.agent.dashboard.utils;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;
import uk.ac.cam.cares.jps.agent.dashboard.IntegrationTestUtils;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.net.http.HttpResponse;

import static org.junit.jupiter.api.Assertions.*;

class AgentCommunicationClientIntegrationTest {
    private static final String RESPONSE_KEY = "body";
    private static final String RESPONSE_VALUE = "json";
    private static final String RESPONSE_KEY_TWO = "url";
    private static final String RESPONSE_VALUE_TWO = "test";

    @AfterEach
    void resetDashboard() {
        IntegrationTestUtils.deleteServiceAccounts();
    }

    @Test
    void testVerifySuccessfulRequest_Success() {
        // Set up a mock response
        HttpResponse successfulResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(successfulResponse.statusCode()).thenReturn(200);
        // Execute methods and ensure no exception is thrown when the response is successful (status code 200)
        AgentCommunicationClient.verifySuccessfulRequest(successfulResponse, "Error message should not be logged");
    }

    @Test
    void testVerifySuccessfulRequest_OtherHttpFailure() {
        String errorMessage = "Request failed";
        // Set up a mock response
        HttpResponse errorResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(errorResponse.statusCode()).thenReturn(404);
        // Verify that a JPSRuntimeException is thrown when the response status code is not 200
        assertThrows(JPSRuntimeException.class, () ->
                AgentCommunicationClient.verifySuccessfulRequest(errorResponse, errorMessage)
        );
    }

    @Test
    void testVerifySuccessfulRequest_400ErrorFail() {
        String errorMessage = "Request failed";
        // Set up a mock response
        HttpResponse errorResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(errorResponse.statusCode()).thenReturn(400);
        // Verify that the following exception is thrown when the response status code is 400
        assertThrows(IllegalArgumentException.class, () ->
                AgentCommunicationClient.verifySuccessfulRequest(errorResponse, errorMessage)
        );
    }

    @Test
    void testRetrieveResponseBody_JsonObject() {
        // Create a mock HttpResponse with a JSON object response body
        HttpResponse jsonResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(jsonResponse.body()).thenReturn("{\"" + RESPONSE_KEY + "\": \"" + RESPONSE_VALUE + "\"}");
        // Execute method
        JsonElement result = AgentCommunicationClient.retrieveResponseBody(jsonResponse);
        // Verify that the result is a JsonObject with the expected content
        assertEquals(JsonObject.class, result.getClass());
        JsonObject jsonObjectResult = result.getAsJsonObject();
        assertTrue(jsonObjectResult.has(RESPONSE_KEY));
        assertEquals(RESPONSE_VALUE, jsonObjectResult.get(RESPONSE_KEY).getAsString());
    }

    @Test
    void testRetrieveResponseBody_JsonArray() {
        // Create a mock HttpResponse with a JSON array response body
        HttpResponse jsonResponse = Mockito.mock(HttpResponse.class);
        Mockito.when(jsonResponse.body()).thenReturn("[{\"" + RESPONSE_KEY + "\": " + RESPONSE_VALUE + "}, {\"" + RESPONSE_KEY_TWO + "\": " + RESPONSE_VALUE_TWO + "}]");
        // Execute method
        JsonElement result = AgentCommunicationClient.retrieveResponseBody(jsonResponse);
        // Verify that the result is a JsonArray with the expected content
        assertEquals(JsonArray.class, result.getClass());
        JsonArray jsonArrayResult = result.getAsJsonArray();
        assertEquals(2, jsonArrayResult.size());
        // Verify first item
        JsonObject firstItem = jsonArrayResult.get(0).getAsJsonObject();
        assertTrue(firstItem.has(RESPONSE_KEY));
        assertEquals(RESPONSE_VALUE, firstItem.get(RESPONSE_KEY).getAsString());
        // Verify second item
        JsonObject secItem = jsonArrayResult.get(1).getAsJsonObject();
        assertTrue(secItem.has(RESPONSE_KEY_TWO));
        assertEquals(RESPONSE_VALUE_TWO, secItem.get(RESPONSE_KEY_TWO).getAsString());
    }

    @Test
    void testSendGetRequest_Integration() {
        String url = IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceAccountServiceUrl() + IntegrationTestUtils.SERVICE_ACCOUNT_SEARCH_SUB_ROUTE;
        // Test sending a GET request with no authentication
        HttpResponse responseWithoutCredentials = AgentCommunicationClient.sendGetRequest(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceUrl());
        // Should return a redirect status code
        assertEquals("(GET " + IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceUrl() + ") 302", responseWithoutCredentials.toString());
        // Test sending a GET request with random bearer token
        HttpResponse responseWithBearerToken = AgentCommunicationClient.sendGetRequest(url, "Invalid");
        // Should return unauthorised status code
        assertEquals("(GET " + url + ") 401", responseWithBearerToken.toString());
        // Test sending a GET request with basic authentication
        HttpResponse responseWithBasicAuth = AgentCommunicationClient.sendGetRequest(url, IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
        // Shoud return a successful response
        assertEquals("(GET " + url + ") 200", responseWithBasicAuth.toString());
    }

    @Test
    void testSendPostRequest_Integration() {
        String url = IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceAccountServiceUrl();
        String sampleAccountName = "PostTest";
        String sampleAccountRole = "Viewer";
        String sampleAccountSettings = "false";
        String params = "{ \"name\": \"" + sampleAccountName + "\", \"role\": \"" + sampleAccountRole + "\", \"isDisabled\" : " + sampleAccountSettings + "}";
        // Test sending a POST request with random bearer token
        HttpResponse responseWithBearerToken = AgentCommunicationClient.sendPostRequest(url, params, "Invalid");
        // Should return unauthorised status code
        assertEquals("(POST " + url + ") 401", responseWithBearerToken.toString());
        // Test sending a POST request with basic authentication
        HttpResponse responseWithBasicAuth = AgentCommunicationClient.sendPostRequest(url, params, IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
        // Shoud return a successful response
        assertEquals("(POST " + url + ") 201", responseWithBasicAuth.toString());
        JsonObject jsonResponse = JsonParser.parseString(responseWithBasicAuth.body().toString()).getAsJsonObject();
        assertEquals(sampleAccountName, jsonResponse.get("name").getAsString());
        assertEquals(sampleAccountRole, jsonResponse.get("role").getAsString());
        assertEquals(sampleAccountSettings, jsonResponse.get("isDisabled").getAsString());
    }
}