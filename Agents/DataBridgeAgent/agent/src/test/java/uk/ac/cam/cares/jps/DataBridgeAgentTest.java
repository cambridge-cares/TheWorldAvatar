package uk.ac.cam.cares.jps;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.bridge.SparqlBridge;
import uk.ac.cam.cares.jps.bridge.SqlBridge;
import uk.ac.cam.cares.jps.bridge.TimeSeriesBridge;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

class DataBridgeAgentTest {
    private static DataBridgeAgent agent;
    private static final JSONObject EXPECTED_RESPONSE = new JSONObject();
    private static final String KEY_SOURCE_NAMESPACE = "source";
    private static final String KEY_TARGET_NAMESPACE = "target";
    private static final String VAL_SOURCE_NAMESPACE = "http://admin:srcpass@example.org/blazegraph/namespace/test/sparql";
    private static final String VAL_TARGET_NAMESPACE = "http://admin:newpassword@stack-blazegraph:8080/blazegraph/namespace/target/sparql";
    private static final String KEY_SOURCE_DATABASE = "srcDbName";
    private static final String KEY_TARGET_DATABASE = "tgtDbName";
    private static final String VAL_SOURCE_DATABASE = "source";
    private static final String VAL_TARGET_DATABASE = "target";
    private static final String KEY_NAMESPACE = "namespace";
    private static final String KEY_DATABASE = "database";
    private static final String VAL_NAMESPACE = "default";
    private static final String KEY_TIME_CLASS = "timeClass";
    private static final String KEY_TIMESTAMP = "timestamp";
    private static final String KEY_VALUES = "values";
    private static final String VAL_TIME_CLASS = "INSTANTANEOUS";
    private static final String[] VAL_TIMESTAMP = {"2022-11-09T03:05:18", "2022-11-19T03:05:18", "2022-11-29T03:05:18"};
    private static final JSONObject VAL_VALUES = new JSONObject();
    private static final String KEY_VALUE_IRI = "iri1";
    private static final Integer[] VAL_VALUE_IRI = {1, 2, 3};
    private static final String KEY_METHOD = "method";
    private static final String GET_METHOD = "GET";
    private static final String POST_METHOD = "POST";
    private static final String KEY_ROUTE = "requestUrl";
    private static final String BASE_ROUTE = "http://localhost:3055/data-bridge-agent/";
    private static final String STATUS_ROUTE = BASE_ROUTE + "status";
    private static final String SPARQL_ROUTE = BASE_ROUTE + "sparql";
    private static final String SQL_ROUTE = BASE_ROUTE + "sql";
    private static final String TIME_SERIES_ROUTE = BASE_ROUTE + "timeseries";
    private static final String srcDb = "jdbc:postgresql://localhost:5432/db";
    private static final String srcUser = "postgres";
    private static final String srcPass = "pass1";
    private static final String tgtDb = "jdbc:postgresql://host.docker.internal:5432/db";
    private static final String tgtUser = "user";
    private static final String tgtPass = "pass2";

    @BeforeAll
    static void generateExpected() {
        EXPECTED_RESPONSE.put("Result", "Successful transfer");
        VAL_VALUES.put(KEY_VALUE_IRI, VAL_VALUE_IRI);
    }

    @BeforeEach
    void setup() {
        agent = new DataBridgeAgent();
    }

    @Test
    void testValidateInput() {
        // For status route
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_ROUTE, STATUS_ROUTE);
        assertTrue(agent.validateInput(requestParams));

        // For time series route
        requestParams = new JSONObject();
        // Optional parameter should not invalidate the outcome
        requestParams.put(KEY_NAMESPACE, VAL_NAMESPACE);
        requestParams.put(KEY_DATABASE, VAL_SOURCE_DATABASE);
        requestParams.put(KEY_TIME_CLASS, VAL_TIME_CLASS);
        requestParams.put(KEY_TIMESTAMP, VAL_TIMESTAMP);
        requestParams.put(KEY_VALUES, VAL_VALUES);
        requestParams.put(KEY_ROUTE, TIME_SERIES_ROUTE);
        assertTrue(agent.validateInput(requestParams));

        // For sparql route
        requestParams = new JSONObject();
        requestParams.put(KEY_SOURCE_NAMESPACE, VAL_SOURCE_NAMESPACE);
        requestParams.put(KEY_TARGET_NAMESPACE, VAL_TARGET_NAMESPACE);
        requestParams.put(KEY_ROUTE, SPARQL_ROUTE);
        assertTrue(agent.validateInput(requestParams));

        // For sql route without parameters
        requestParams = new JSONObject();
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        assertTrue(agent.validateInput(requestParams));
        // For sql route with source parameters
        requestParams = new JSONObject();
        requestParams.put(KEY_SOURCE_DATABASE, VAL_SOURCE_DATABASE);
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        assertTrue(agent.validateInput(requestParams));
        // For sql route with target parameters
        requestParams = new JSONObject();
        requestParams.put(KEY_TARGET_DATABASE, VAL_TARGET_DATABASE);
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        assertTrue(agent.validateInput(requestParams));

    }

    @Test
    void testValidateInputInvalidWithNoParameters() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_ROUTE, "");
        assertFalse(agent.validateInput(requestParams));
    }

    @Test
    void testValidateInputInvalidForTimeSeriesRoute() {
        // Invalid when missing one of three parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_TIME_CLASS, VAL_TIME_CLASS);
        requestParams.put(KEY_TIMESTAMP, VAL_TIMESTAMP);
        requestParams.put(KEY_ROUTE, TIME_SERIES_ROUTE);
        assertFalse(agent.validateInput(requestParams));
        // Invalid for invalid time class
        requestParams = new JSONObject();
        requestParams.put(KEY_TIME_CLASS, "timezone");
        requestParams.put(KEY_TIMESTAMP, VAL_TIMESTAMP);
        requestParams.put(KEY_VALUES, VAL_VALUES);
        requestParams.put(KEY_ROUTE, TIME_SERIES_ROUTE);
        assertFalse(agent.validateInput(requestParams));
    }

    @Test
    void testValidateInputInvalid() {
        // Invalid for only source namespace key provided to sparql route
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_SOURCE_NAMESPACE, VAL_SOURCE_NAMESPACE);
        requestParams.put(KEY_ROUTE, SPARQL_ROUTE);
        assertFalse(agent.validateInput(requestParams));
        // Invalid if both parameters are passed to sql route
        requestParams = new JSONObject();
        requestParams.put(KEY_SOURCE_DATABASE, VAL_SOURCE_DATABASE);
        requestParams.put(KEY_TARGET_DATABASE, VAL_TARGET_DATABASE);
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        assertFalse(agent.validateInput(requestParams));
        // Invalid when parameter is not a string
        requestParams = new JSONObject();
        requestParams.put(KEY_SOURCE_DATABASE, 135);
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        assertFalse(agent.validateInput(requestParams));
    }

    @Test
    void testProcessRequestParametersForUndefinedRoute() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, BASE_ROUTE);
        JSONObject response = agent.processRequestParameters(requestParams);
        assertEquals("Parameters are invalid, please check logs for more details.", response.getString("Result"));
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
    void testProcessRequestParametersForSparqlRouteIncompleteParameters() {
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, POST_METHOD);
        requestParams.put(KEY_ROUTE, SPARQL_ROUTE);
        requestParams.put(KEY_SOURCE_NAMESPACE, VAL_SOURCE_NAMESPACE);
        // Execute method should throw right error and response
        assertEquals("Parameters are invalid, please check logs for more details.", agent.processRequestParameters(requestParams).getString("Result"));
    }

    @Test
    void testProcessRequestParametersForSparqlRoute() {
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, POST_METHOD);
        requestParams.put(KEY_ROUTE, SPARQL_ROUTE);
        requestParams.put(KEY_SOURCE_NAMESPACE, VAL_SOURCE_NAMESPACE);
        requestParams.put(KEY_TARGET_NAMESPACE, VAL_TARGET_NAMESPACE);
        // Mock the bridge object, as it cannot be unit tested and requires integration test
        try (MockedConstruction<SparqlBridge> mockConnector = Mockito.mockConstruction(SparqlBridge.class)) {
            // Execute method
            JSONObject response = agent.processRequestParameters(requestParams);
            // Verify response
            assertEquals("Triples have been successfully transferred from " + VAL_SOURCE_NAMESPACE + " to " + VAL_TARGET_NAMESPACE, response.getString("Result"));
        }
    }

    @Test
    void testProcessRequestParametersForSparqlRouteViaInvalidGET() {
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD); // Wrong method
        requestParams.put(KEY_ROUTE, SPARQL_ROUTE);
        requestParams.put(KEY_SOURCE_NAMESPACE, VAL_SOURCE_NAMESPACE);
        requestParams.put(KEY_TARGET_NAMESPACE, VAL_TARGET_NAMESPACE);
        // Execute method
        JSONObject response = agent.processRequestParameters(requestParams);
        // Verify response
        assertEquals("Invalid request type! Route sparql can only accept POST request.", response.getString("Result"));
    }

    @Test
    void testProcessRequestParametersForSqlRouteIncompleteProperties() throws IOException {
        // Generate sample config file
        File config = TestConfigUtils.genSampleSQLConfigFile(false, srcDb, srcUser, srcPass, tgtDb, tgtUser, tgtPass);
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        try {
            // Execute method should throw right error and response
            JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> agent.processRequestParameters(requestParams));
            assertEquals("Missing Properties:\n" +
                    "src.db.user is missing! Please add the input to endpoint.properties.\n" +
                    "src.db.password is missing! Please add the input to endpoint.properties.\n", thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testProcessRequestParametersForSqlRoute() throws IOException {
        // Generate sample config file
        File config = TestConfigUtils.genSampleSQLConfigFile(true, srcDb, srcUser, srcPass, tgtDb, tgtUser, tgtPass);
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        // Mock the bridge object, as it cannot be unit tested and requires integration test
        try (MockedConstruction<SqlBridge> mockConnector = Mockito.mockConstruction(SqlBridge.class,
                (mock, context) -> {
                    Mockito.when(mock.transfer(false)).thenReturn(EXPECTED_RESPONSE);
                })
        ) {
            // Execute method
            JSONObject response = agent.processRequestParameters(requestParams);
            // Verify response
            assertEquals(EXPECTED_RESPONSE, response);
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testProcessRequestParametersForSqlRouteForStack() throws IOException {
        // Generate sample config file
        File config = TestConfigUtils.genSampleSQLConfigFile(true, srcDb, srcUser, srcPass, tgtDb, tgtUser, tgtPass);
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_SOURCE_DATABASE, VAL_SOURCE_DATABASE);
        requestParams.put(KEY_ROUTE, SQL_ROUTE);
        // Mock the postgis config object to control what values are retrieved
        PostGISEndpointConfig mockConfig = Mockito.mock(PostGISEndpointConfig.class);
        // Mock the container client object to retrieve the stack configuration as stack cannot be started in a test environment
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class,
                (mock, context) -> {
                    Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(mockConfig);
                });
             // Mock the bridge object, as it cannot be unit tested and requires integration test
             MockedConstruction<SqlBridge> mockConnector = Mockito.mockConstruction(SqlBridge.class,
                     (mock, context) -> {
                         Mockito.when(mock.transfer(true)).thenReturn(EXPECTED_RESPONSE);
                     })
        ) {
            // Mock the values to be retrieved for the stack RDB
            Mockito.when(mockConfig.getJdbcURL(Mockito.anyString())).thenReturn(srcDb);
            Mockito.when(mockConfig.getUsername()).thenReturn(srcUser);
            Mockito.when(mockConfig.getPassword()).thenReturn(srcPass);
            // Execute method
            JSONObject response = agent.processRequestParameters(requestParams);
            // Verify response
            assertEquals(EXPECTED_RESPONSE, response);
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

    @Test
    void testProcessRequestParametersForTimeSeriesRoute() {
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, POST_METHOD);
        requestParams.put(KEY_NAMESPACE, VAL_SOURCE_NAMESPACE);
        requestParams.put(KEY_DATABASE, VAL_SOURCE_DATABASE);
        requestParams.put(KEY_TIME_CLASS, VAL_TIME_CLASS);
        requestParams.put(KEY_TIMESTAMP, VAL_TIMESTAMP);
        requestParams.put(KEY_VALUES, VAL_VALUES);
        requestParams.put(KEY_ROUTE, TIME_SERIES_ROUTE);
        // Mock the postgis config object to control what values are retrieved
        PostGISEndpointConfig mockConfig = Mockito.mock(PostGISEndpointConfig.class);
        // Mock the container client object to retrieve the stack configuration as stack cannot be started in a test environment
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class,
                (mock, context) -> {
                    Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(mockConfig);
                });
             // Mock the bridge object, as it cannot be unit tested and requires integration test
             MockedConstruction<TimeSeriesBridge> mockConnector = Mockito.mockConstruction(TimeSeriesBridge.class,
                     (mock, context) -> {
                         Mockito.when(mock.instantiateTimeSeries(Mockito.any())).thenReturn(EXPECTED_RESPONSE);
                     })

        ) {
            // Mock the values to be retrieved for the stack RDB
            Mockito.when(mockConfig.getJdbcURL(Mockito.anyString())).thenReturn(srcDb);
            Mockito.when(mockConfig.getUsername()).thenReturn(srcUser);
            Mockito.when(mockConfig.getPassword()).thenReturn(srcPass);
            // Execute method
            JSONObject response = agent.processRequestParameters(requestParams);
            // Verify response
            assertEquals(EXPECTED_RESPONSE, response);
        }
    }

    @Test
    void testProcessRequestParametersForTimeSeriesRouteViaInvalidGet() {
        // Set up request parameters
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD); // Wrong method
        requestParams.put(KEY_NAMESPACE, VAL_NAMESPACE);
        requestParams.put(KEY_DATABASE, VAL_SOURCE_DATABASE);
        requestParams.put(KEY_TIME_CLASS, VAL_TIME_CLASS);
        requestParams.put(KEY_TIMESTAMP, VAL_TIMESTAMP);
        requestParams.put(KEY_VALUES, VAL_VALUES);
        requestParams.put(KEY_ROUTE, TIME_SERIES_ROUTE);
        // Execute method
        JSONObject response = agent.processRequestParameters(requestParams);
        // Verify response
        assertEquals("Invalid request type! Route timeseries can only accept POST request.", response.getString("Result"));
    }
}