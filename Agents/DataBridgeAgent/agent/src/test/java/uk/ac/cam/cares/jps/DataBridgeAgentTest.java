package uk.ac.cam.cares.jps;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class DataBridgeAgentTest {
    private static DataBridgeAgent agent;
    private static final String KEY_METHOD = "method";
    private static final String GET_METHOD = "GET";
    private static final String POST_METHOD = "POST";
    private static final String KEY_ROUTE = "requestUrl";
    private static final String BASE_ROUTE = "http://localhost:3055/data-bridge-agent/";
    private static final String STATUS_ROUTE = BASE_ROUTE + "status";

    @BeforeEach
    void setup() {
        agent = new DataBridgeAgent();
    }

    @Test
    void testProcessRequestParametersForUndefinedRoute() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, BASE_ROUTE);
        JSONObject response = agent.processRequestParameters(requestParams);
        assertTrue(response.isEmpty());
    }
    @Test
    void testProcessRequestParametersForStatusRouteViaGET() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, STATUS_ROUTE);
        JSONObject response = agent.processRequestParameters(requestParams);
        assertEquals("Agent is ready to receive requests.", response.getString("Result"));
    }

    @Test
    void testProcessRequestParametersForStatusRouteViaInvalidPOST() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, POST_METHOD);
        requestParams.put(KEY_ROUTE, STATUS_ROUTE);
        JSONObject response = agent.processRequestParameters(requestParams);
        assertEquals("Invalid request type! Route status can only accept GET request.", response.getString("Result"));
    }
}