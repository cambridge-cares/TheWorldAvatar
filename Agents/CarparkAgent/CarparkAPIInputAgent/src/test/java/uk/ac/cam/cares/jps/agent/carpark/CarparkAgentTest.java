package uk.ac.cam.cares.jps.agent.carpark;

import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.servlet.ServletException;

import static org.junit.jupiter.api.Assertions.*;

class CarparkAgentTest {
    private static CarparkAgent agent;
    private static final String KEY_METHOD = "method";
    private static final JsonPrimitive GET_METHOD = new JsonPrimitive("GET");
    private static final String KEY_ROUTE = "requestUrl";
    private static final String BASE_ROUTE = "http://localhost:1080/carpark-agent/";
    private static final JsonPrimitive JSON_BASE_ROUTE = new JsonPrimitive(BASE_ROUTE);
    private static final JsonPrimitive STATUS_ROUTE = new JsonPrimitive(BASE_ROUTE + "status");

    @BeforeEach
    void setup() {
        agent = new CarparkAgent();
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
    void testProcessRequestParameters_StatusRoute() throws ServletException {
        JsonObject requestParams = new JsonObject();
        requestParams.add(KEY_METHOD, GET_METHOD);
        requestParams.add(KEY_ROUTE, STATUS_ROUTE);
        agent.init();
        // Execute method
        JSONObject response = agent.processRequestParameters(new JSONObject(requestParams.toString()));
        assertEquals("Agent is ready to receive requests.", response.getString("Result"));
    }

    // test for retrieve and create routes is not necessary as the functions implemented in these two routes have been tested in other unit tests
}