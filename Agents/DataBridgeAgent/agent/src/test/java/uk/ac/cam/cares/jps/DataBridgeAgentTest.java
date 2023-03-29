package uk.ac.cam.cares.jps;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

class DataBridgeAgentTest {
    private static DataBridgeAgent agent;
    private static final String KEY_METHOD = "method";
    private static final String GET_METHOD = "GET";
    private static final String POST_METHOD = "POST";
    private static final String KEY_ROUTE = "requestUrl";
    private static final String BASE_ROUTE = "http://localhost:3055/data-bridge-agent/";
    private static final String STATUS_ROUTE = BASE_ROUTE + "status";
    private static final String SPARQL_ROUTE = BASE_ROUTE + "sparql";
    private static final String SQL_ROUTE = BASE_ROUTE + "sql";
    private static final String originSparql = "http://www.example.org/blazegraph/namespace/test/sparql";
    private static final String destinationSparql = "http://www.target.org:9999/blazegraph/namespace/target/sparql";
    private static final String srcDb = "jdbc:postgresql://localhost:5432/db";
    private static final String srcUser = "postgres";
    private static final String srcPass = "pass1";
    private static final String tgtDb = "jdbc:postgresql://host.docker.internal:5432/db";
    private static final String tgtUser = "user";
    private static final String tgtPass = "pass2";

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

    @Test
    void testProcessRequestParametersForSparqlRouteViaGETIncompleteProperties() throws IOException {
        // Generate sample config file
        File config = TestConfigUtils.genSampleSPARQLConfigFile(false, originSparql, destinationSparql);
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, SPARQL_ROUTE);
        try {
            // Execute method should throw right error and response
            JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, ()-> agent.processRequestParameters(requestParams));
            assertEquals("Missing Properties:\n" +
                    "sparql.destination.endpoint is missing! Please add the input to endpoint.properties.\n", thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testProcessRequestParametersForSparqlRouteViaGET() throws IOException {
        // Generate sample config file
        File config = TestConfigUtils.genSampleSPARQLConfigFile(true, originSparql, destinationSparql);
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, SPARQL_ROUTE);
        // Mock the bridge object, as it cannot be unit tested and requires integration test
        try (MockedConstruction<SparqlBridge> mockConnector = Mockito.mockConstruction(SparqlBridge.class)) {
            // Execute method
            JSONObject response = agent.processRequestParameters(requestParams);
            // Verify response
            assertEquals("Triples have been successfully transferred from " + originSparql + " to " + destinationSparql, response.getString("Result"));
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testProcessRequestParametersForSparqlRouteViaInvalidPOST() {
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, POST_METHOD); // Wrong method
        requestParams.put(KEY_ROUTE, SPARQL_ROUTE);
        // Execute method
        JSONObject response = agent.processRequestParameters(requestParams);
        // Verify response
        assertEquals("Invalid request type! Route sparql can only accept GET request.", response.getString("Result"));
    }

    @Test
    void testProcessRequestParametersForSqlRouteViaGETIncompleteProperties() throws IOException {
        // Generate sample config file
        File config = TestConfigUtils.genSampleSQLConfigFile(false, srcDb, srcUser, srcPass, tgtDb, tgtUser, tgtPass);
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        try {
            // Execute method should throw right error and response
            JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, ()-> agent.processRequestParameters(requestParams));
            assertEquals("Missing Properties:\n" +
                    "src.db.user is missing! Please add the input to endpoint.properties.\n"+
                    "src.db.password is missing! Please add the input to endpoint.properties.\n", thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testProcessRequestParametersForSqlRouteViaGET() throws IOException {
        // Generate sample config file
        File config = TestConfigUtils.genSampleSQLConfigFile(true, srcDb, srcUser, srcPass, tgtDb, tgtUser, tgtPass);
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        // Mock the bridge object, as it cannot be unit tested and requires integration test
        try (MockedConstruction<SqlBridge> mockConnector = Mockito.mockConstruction(SqlBridge.class)) {
            // Execute method
            JSONObject response = agent.processRequestParameters(requestParams);
            // Verify response
            assertEquals("Data have been successfully transferred from " + srcDb + " to " + tgtDb, response.getString("Result"));
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testProcessRequestParametersForSqlRouteViaInvalidPOST() {
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, POST_METHOD); // Wrong method
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        // Execute method
        JSONObject response = agent.processRequestParameters(requestParams);
        // Verify response
        assertEquals("Invalid request type! Route sql can only accept GET request.", response.getString("Result"));
    }
}