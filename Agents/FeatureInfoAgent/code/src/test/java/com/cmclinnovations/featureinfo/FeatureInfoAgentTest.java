package com.cmclinnovations.featureinfo;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import org.apache.http.client.methods.HttpGet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hamcrest.Matchers;
import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.hamcrest.MockitoHamcrest;
import org.springframework.mock.web.MockHttpServletResponse;

import com.cmclinnovations.featureinfo.config.ConfigEndpoint;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.EndpointType;
import com.cmclinnovations.featureinfo.config.NamespaceGetterTest;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Unit tests for the FeatureInfoAgent class.
 */
public class FeatureInfoAgentTest {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(FeatureInfoAgentTest.class);

    /**
     * Test config store.
     */
    private static final ConfigStore CONFIG = new ConfigStore();

    /**
     * Read in mock config file before running tests.
     */
    @BeforeAll
    public static void setup() {
        try (InputStream is = NamespaceGetterTest.class.getResourceAsStream("/mock-config-file.json")) {
            BufferedReader bufferReader = new BufferedReader(new InputStreamReader(is));
            StringBuilder stringBuilder = new StringBuilder();

            String eachStringLine;
            while ((eachStringLine = bufferReader.readLine()) != null) {
                stringBuilder.append(eachStringLine).append("\n");
            }
            CONFIG.loadFile(stringBuilder.toString());

        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not read mock config file!");
        }

        // Add mock endpoints to the config
        CONFIG.addEndpoint(new ConfigEndpoint("ONTOP", "http://my-fake-ontop.com/", null, null, EndpointType.ONTOP));
        CONFIG.addEndpoint(new ConfigEndpoint("POSTGRES", "http://my-fake-postgres.com/", null, null, EndpointType.POSTGRES));
        CONFIG.addEndpoint(new ConfigEndpoint("blazegraph-test", "http://fake-website.com/blazegraph/namespace/test/sparql", null, null, EndpointType.BLAZEGRAPH));

        // Write a temporary query file
        try {
            String tmpdir = System.getProperty("java.io.tmpdir");

            Path tempMeta = Paths.get(tmpdir, "FeatureInfoAgentTest-Meta.sparql");
            Files.writeString(tempMeta, "SAMPLE-META-QUERY");
            LOGGER.info("Written temporary query file for testing (" + tempMeta.toString() + ").");

            Path tempTime = Paths.get(tmpdir, "FeatureInfoAgentTest-Time.sparql");
            Files.writeString(tempTime, "SAMPLE-TIME-QUERY");
            LOGGER.info("Written temporary query file for testing (" + tempTime.toString() + ").");

            Path tempForced = Paths.get(tmpdir, "FeatureInfoAgentTest-Forces.sparql");
            Files.writeString(tempForced, "FORCED-ENDPOINT-QUERY");
            LOGGER.info("Written temporary query file for testing (" + tempForced.toString() + ").");

            // Add to the config
            CONFIG.addMetaQueryForClass("SAMPLE-CLASS", tempMeta.toString());
            CONFIG.addTimeQueryForClass("SAMPLE-CLASS", tempTime.toString());
            CONFIG.addMetaQueryForClass("FORCED-ENDPOINT", tempForced.toString());
            CONFIG.addTimeQueryForClass("TIME-ONLY-CLASS", tempTime.toString());

            FeatureInfoAgent.CONFIG = CONFIG;

        } catch(IOException exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not write temporary query file!");
        }
    }

    /**
     * Clean up after all tests have executed.
     */
    @AfterAll
    public static void cleanup() {
          // Delete the temporary query files
          try {
            String tmpdir = System.getProperty("java.io.tmpdir");
            
            Path tempMeta = Paths.get(tmpdir, "FeatureInfoAgentTest-Meta.sparql");
            Path tempTime = Paths.get(tmpdir, "FeatureInfoAgentTest-Time.sparql");
            Path tempForced = Paths.get(tmpdir, "FeatureInfoAgentTest-Forces.sparql");
            Files.deleteIfExists(tempMeta);
            Files.deleteIfExists(tempTime);
            Files.deleteIfExists(tempForced);

            LOGGER.info("Removed temporary query files.");
        } catch(IOException exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not delete temporary query file!");
        }
    }

    /**
     * Tests the response of an unknown route.
     */
    @Test
    public void testBadRoute() throws Exception {
        FeatureInfoAgent agent = new FeatureInfoAgent();

        // Mock AgentCaller.readJsonParameter() method
        try(MockedStatic<AgentCaller> caller = Mockito.mockStatic(AgentCaller.class)) {
            caller.when(() -> {
                AgentCaller.readJsonParameter(ArgumentMatchers.any());
            }).thenReturn(
                new JSONObject("{\"property-one\":\"value-one\",\"property-two\":\"value-two\"}")
            );

            // Mock request
            HttpServletRequest request = spy(HttpServletRequest.class);
            when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
            when(request.getRequestURI()).thenReturn("http://fake-website.com/feature-info-agent/foobar");

            // Mock response with mocked writer
            HttpServletResponse response = spy(MockHttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(response.getWriter()).thenReturn(printWriter);

            // Run the agent
            agent.doGet(request, response);

            // Check response code
            Assertions.assertEquals(Response.Status.NOT_IMPLEMENTED.getStatusCode(), response.getStatus(), "Status code did not match the expected value!");
            Assertions.assertTrue(strWriter.toString().contains("Unknown route"), "Response body did not contain expected string!");
        
        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            Assertions.fail("Exception occured when attempting to test a bad request!");
        }
     }

    /**
     * Tests the response of a badly formed incoming request.
     */
    @Test
    public void testBadRequest() throws Exception {
        FeatureInfoAgent agent = new FeatureInfoAgent();

        // Mock AgentCaller.readJsonParameter() method
        try(MockedStatic<AgentCaller> caller = Mockito.mockStatic(AgentCaller.class)) {
            caller.when(() -> {
                AgentCaller.readJsonParameter(ArgumentMatchers.any());
            }).thenReturn(
                new JSONObject("{\"property-one\":\"value-one\",\"property-two\":\"value-two\"}")
            );

            // Mock request
            HttpServletRequest request = spy(HttpServletRequest.class);
            when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
            when(request.getRequestURI()).thenReturn("http://fake-website.com/feature-info-agent/get");

            // Mock response with mocked writer
            HttpServletResponse response = spy(MockHttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(response.getWriter()).thenReturn(printWriter);

            // Run the agent
            agent.doGet(request, response);

            // Check response code
            Assertions.assertEquals(Response.Status.BAD_REQUEST.getStatusCode(), response.getStatus(), "Status code did not match the expected value!");
            Assertions.assertTrue(strWriter.toString().contains("Bad request"), "Response body did not contain expected string!");
        
        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            Assertions.fail("Exception occured when attempting to test a bad request!");
        }
     }

    /**
     * Tests the response of status request (when the FeatureInfoAgent is in an invalid state).
     */
    @Test
    public void testBadStatus() throws Exception {
        FeatureInfoAgent agent = new FeatureInfoAgent();

        // Destroy the config to force a bad state
        FeatureInfoAgent.CONFIG = null;

        // Mock request
        HttpServletRequest request = spy(HttpServletRequest.class);
        when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
        when(request.getRequestURI()).thenReturn("http://fake-website.com/feature-info-agent/status");

        // Mock response with mocked writer
        HttpServletResponse response = spy(MockHttpServletResponse.class);
        StringWriter strWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(strWriter);
        when(response.getWriter()).thenReturn(printWriter);

        // Run the agent
        agent.doGet(request, response);

        // Check response code
        Assertions.assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus(), "Status code did not match the expected value!");
        Assertions.assertFalse(strWriter.toString().isBlank(), "Response body is empty!");

        // Restore the config
        FeatureInfoAgent.CONFIG = CONFIG;
    }

    /**
     * Tests the response of status request (when the FeatureInfoAgent is in a valid state).
     */
    @Test
    public void testGoodStatus() throws Exception {
        FeatureInfoAgent agent = new FeatureInfoAgent();

        // Mock AgentCaller.readJsonParameter() method
        try(MockedStatic<AgentCaller> caller = Mockito.mockStatic(AgentCaller.class)) {
            caller.when(() -> {
                AgentCaller.readJsonParameter(ArgumentMatchers.any());
            }).thenReturn(
                new JSONObject("{\"property-one\":\"value-one\",\"property-two\":\"value-two\"}")
            );

            // Mock request
            HttpServletRequest request = spy(HttpServletRequest.class);
            when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
            when(request.getRequestURI()).thenReturn("http://fake-website.com/feature-info-agent/status");

            // Mock response with mocked writer
            HttpServletResponse response = spy(MockHttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(response.getWriter()).thenReturn(printWriter);

            // Run the agent
            agent.doGet(request, response);

            // Check the response
            Assertions.assertEquals(Response.Status.OK.getStatusCode(), response.getStatus(), "Status code did not match the expected value!");
            Assertions.assertTrue(strWriter.toString().contains("Ready"), "Response body did not contain expected string!");
        
        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            Assertions.fail("Exception occured when attempting to test a bad request!");
        }
    }

    /**
     * Test the agent with a mock query.
     */
    @Test
    public void testQuery() throws Exception {
        FeatureInfoAgent agent = new FeatureInfoAgent();
        FeatureInfoAgent.RDB_CONN = mock(Connection.class);

        // Mock clients
        RemoteStoreClient rsClient = this.mockRemoteStoreClient(true, true, null);
        FeatureInfoAgent.RS_CLIENT_OVER = rsClient;
        FeatureInfoAgent.RDB_CLIENT_OVER = this.mockRDBClient();
        TimeSeriesClient<Instant> tsClient = this.mockTimeSeriesClient();
        FeatureInfoAgent.TS_CLIENT_OVER = tsClient;

        // Mock request
        HttpServletRequest request = spy(HttpServletRequest.class);
        when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
        when(request.getRequestURI()).thenReturn("http://fake-website.com/feature-info-agent/get");

        // Mock response with mocked writer
        HttpServletResponse response = spy(MockHttpServletResponse.class);
        StringWriter strWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(strWriter);
        when(response.getWriter()).thenReturn(printWriter);

        // Mock AgentCaller.readJsonParameter() method
        try(MockedStatic<AgentCaller> caller = Mockito.mockStatic(AgentCaller.class)) {
            caller.when(() -> {
                AgentCaller.readJsonParameter(ArgumentMatchers.any());
            }).thenReturn(
                new JSONObject("{\"iri\":\"http://fake-iri.com\"}")
            );

            // Run the agent
            agent.doGet(request, response);

            // Check the response code
            Assertions.assertEquals(Response.Status.OK.getStatusCode(), response.getStatus(), "Status code did not match the expected value!");
            
            // Check against expected result
            JSONObject jsonResult = new JSONObject(strWriter.toString());
            JSONObject expected = new JSONObject("""
                {\"meta\":[{\"propertyOne\":\"1.0 [s]\"}],\"time\":[{\"data\":[\"Measurement One\"],\"values\":[[\"1.0\",\"2.0\",\"3.0\"]],
                \"timeClass\":\"Instant\",\"valuesClass\":[\"String\"],\"id\":1,\"units\":[\"m/s\"],\"time\":[\"-1000000000-01-01T00:00:00Z\",
                \"1970-01-01T00:00:00Z\",\"+1000000000-12-31T23:59:59.999999999Z\"],\"properties\":[{}]}]}
            """);

            System.out.println(jsonResult.toString());

            Assertions.assertTrue(jsonResult.similar(expected), "Response body did not contain expected JSON object!");
        } 
    }

    /**
     * Tests that the agent can run using a class that has an associated 
     * timeseries query, but no meta query.
     */
    @Test
    public void testQueryNoMeta() throws Exception {
        FeatureInfoAgent agent = new FeatureInfoAgent();
        FeatureInfoAgent.RDB_CONN = mock(Connection.class);

        // Mock the RemoteStoreClient 
        RemoteStoreClient rsClient = this.mockRemoteStoreClient(false, true, null);

        // Mock result when querying for class
        when(rsClient.executeFederatedQuery(
            ArgumentMatchers.anyList(),
            ArgumentMatchers.contains("?class")))
            .thenReturn(
                new org.json.JSONArray("[{\"class\": \"TIME-ONLY-CLASS\"}]")
            );

        FeatureInfoAgent.RS_CLIENT_OVER = rsClient;
        FeatureInfoAgent.RDB_CLIENT_OVER = this.mockRDBClient();

        // Mock timeseries client
        TimeSeriesClient<Instant> tsClient = this.mockTimeSeriesClient();
        FeatureInfoAgent.TS_CLIENT_OVER = tsClient;

        // Mock request
        HttpServletRequest request = spy(HttpServletRequest.class);
        when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
        when(request.getRequestURI()).thenReturn("http://fake-website.com/feature-info-agent/get");

        // Mock response with mocked writer
        HttpServletResponse response = spy(MockHttpServletResponse.class);
        StringWriter strWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(strWriter);
        when(response.getWriter()).thenReturn(printWriter);

        // Mock AgentCaller.readJsonParameter() method
        try(MockedStatic<AgentCaller> caller = Mockito.mockStatic(AgentCaller.class)) {
            caller.when(() -> {
                AgentCaller.readJsonParameter(ArgumentMatchers.any());
            }).thenReturn(
                new JSONObject("{\"iri\":\"http://fake-iri.com\"}")
            );

            // Run the agent
            agent.doGet(request, response);

            // Check the response code
            Assertions.assertEquals(Response.Status.OK.getStatusCode(), response.getStatus(), "Status code did not match the expected value!");
            
            // Check against expected result
            JSONObject jsonResult = new JSONObject(strWriter.toString());
            JSONObject expected = new JSONObject("""
                {\"time\":[{\"data\":[\"Measurement One\"],\"values\":[[\"1.0\",\"2.0\",\"3.0\"]],\"timeClass\":\"Instant\",\"valuesClass\":[\"String\"],\"id\":1,\"units\":[\"m/s\"],\"time\":[\"-1000000000-01-01T00:00:00Z\",\"1970-01-01T00:00:00Z\",\"+1000000000-12-31T23:59:59.999999999Z\"],\"properties\":[{}]}]}
            """);

            Assertions.assertTrue(jsonResult.similar(expected), "Response body did not contain expected JSON object!");
        } 
    }

    /**
     * Test the agent with a mock query that does not return any timeseries data.
     */
    @Test
    public void testQueryNoTime() throws Exception {
        FeatureInfoAgent agent = new FeatureInfoAgent();
        FeatureInfoAgent.RDB_CONN = mock(Connection.class);

        // Mock the RemoteStoreClient 
        RemoteStoreClient rsClient = this.mockRemoteStoreClient(true, false, null);
        FeatureInfoAgent.RS_CLIENT_OVER = rsClient;
        FeatureInfoAgent.RDB_CLIENT_OVER = this.mockRDBClient();

        // Mock request
        HttpServletRequest request = spy(HttpServletRequest.class);
        when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
        when(request.getRequestURI()).thenReturn("http://fake-website.com/feature-info-agent/get");

        // Mock response with mocked writer
        HttpServletResponse response = spy(MockHttpServletResponse.class);
        StringWriter strWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(strWriter);
        when(response.getWriter()).thenReturn(printWriter);

        // Mock AgentCaller.readJsonParameter() method
        try(MockedStatic<AgentCaller> caller = Mockito.mockStatic(AgentCaller.class)) {
            caller.when(() -> {
                AgentCaller.readJsonParameter(ArgumentMatchers.any());
            }).thenReturn(
                new JSONObject("{\"iri\":\"http://fake-iri.com\"}")
            );

            // Run the agent
            agent.doGet(request, response);

            // Check the response code
            Assertions.assertEquals(Response.Status.OK.getStatusCode(), response.getStatus(), "Status code did not match the expected value!");
            
            // Check against expected result
            JSONObject jsonResult = new JSONObject(strWriter.toString());
            JSONObject expected = new JSONObject("""
                {\"meta\":[{\"propertyOne\":\"1.0 [s]\"}]}
            """);

            Assertions.assertTrue(jsonResult.similar(expected), "Response body did not contain expected JSON object!");
        } 
    }

     /**
     * Tests running the FeatureInfoAgent with bad configuration settings (unreachable endpoints).
     * Note that this should not throw an exception here but instead update the HTTP code sent back.
     * @throws Exception
     */
    @Test
    public void testBadConfig() throws Exception {
        FeatureInfoAgent agent = new FeatureInfoAgent();
        FeatureInfoAgent.RS_CLIENT_OVER = null;

        // Mock request
        HttpServletRequest request = spy(HttpServletRequest.class);
        when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
        when(request.getRequestURI()).thenReturn("http://fake-website.com/feature-info-agent/get");

        // Mock response with mocked writer
        HttpServletResponse response = spy(MockHttpServletResponse.class);
        StringWriter strWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(strWriter);
        when(response.getWriter()).thenReturn(printWriter);

        // Mock AgentCaller.readJsonParameter() method
        try(MockedStatic<AgentCaller> caller = Mockito.mockStatic(AgentCaller.class)) {
            caller.when(() -> {
                AgentCaller.readJsonParameter(ArgumentMatchers.any());
            }).thenReturn(
                new JSONObject("{\"iri\":\"http://fake-iri.com\"}")
            );

            // Run the agent
            agent.doGet(request, response);

            // Check the response code
            Assertions.assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus(), "Status code did not match the expected value!");
            
            // Check against expected result
            JSONObject jsonResult = new JSONObject(strWriter.toString());
            Assertions.assertTrue(jsonResult.toString().contains("Could not contact endpoints"), "Response body did not contain expected content!");
        } 
    }

    /**
     * Test the agent with an enforced Blazegraph endpoint.
     * 
     * NOTE: This tests a feature that is currently disabled (commented out). If it is
     * re-enabled within the FeatureInfoAgent, then remove the @Ignore annotation.
     */
    @Test
    @Ignore
    public void testEnforcedEndpoint() throws Exception {
        FeatureInfoAgent agent = new FeatureInfoAgent();

        // Mock clients
        RemoteStoreClient rsClient = this.mockRemoteStoreClient(true, true, "http://fake-website.com/blazegraph/namespace/test/sparql");
        FeatureInfoAgent.RS_CLIENT_OVER = rsClient;
        FeatureInfoAgent.RDB_CLIENT_OVER = this.mockRDBClient();
        TimeSeriesClient<Instant> tsClient = this.mockTimeSeriesClient();
        FeatureInfoAgent.TS_CLIENT_OVER = tsClient;

        // Mock request
        HttpServletRequest request = spy(HttpServletRequest.class);
        when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
        when(request.getRequestURI()).thenReturn("http://fake-website.com/feature-info-agent/get");

        // Mock response with mocked writer
        HttpServletResponse response = spy(MockHttpServletResponse.class);
        StringWriter strWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(strWriter);
        when(response.getWriter()).thenReturn(printWriter);

        // Mock AgentCaller.readJsonParameter() method
        try(MockedStatic<AgentCaller> caller = Mockito.mockStatic(AgentCaller.class)) {
            caller.when(() -> {
                AgentCaller.readJsonParameter(ArgumentMatchers.any());
            }).thenReturn(
                new JSONObject("{\"iri\":\"http://fake-iri.com\", \"endpoint\":\"http://fake-website.com/blazegraph/namespace/test/sparql\"}")
            );

            // Run the agent
            agent.doGet(request, response);

            // Check the response code
            Assertions.assertEquals(Response.Status.OK.getStatusCode(), response.getStatus(), "Status code did not match the expected value!");
            
            // Check against the expected result
            JSONObject jsonResult = new JSONObject(strWriter.toString());
            JSONObject expected = new JSONObject("{\"meta\": [{\"Forced\": \"Yes\"}]}");
            Assertions.assertTrue(jsonResult.similar(expected), "JSON response did not match expected result!");
        } 
    }

    /**
     * 
     * @return
     */
    private RemoteRDBStoreClient mockRDBClient() throws Exception {
        RemoteRDBStoreClient rdbClient = mock(RemoteRDBStoreClient.class);
        when(rdbClient.getConnection())
        .thenReturn(null);

        return rdbClient;
    }

    /**
     * Produces a mock RemoteStoreClient instance that returns the expected results.
     * 
     * @param mockTime should the query for measurements be mocked?
     * @param endpoint enforced endpoint URL
     * @return
     */
    private RemoteStoreClient mockRemoteStoreClient(boolean mockMeta, boolean mockTime, String endpoint) throws Exception {
        // Mock the RemoteStoreClient
        RemoteStoreClient rsClient = mock(RemoteStoreClient.class);
        
        if(endpoint != null) {
            // Mock result when querying for class
            when(rsClient.executeFederatedQuery(
                (List<String>) MockitoHamcrest.argThat(Matchers.contains(endpoint)),
                ArgumentMatchers.contains("?class")))
                .thenReturn(
                    new org.json.JSONArray("[{\"class\": \"FORCED-ENDPOINT\"}]")
                );
        } else {
            // Mock result when querying for class
            when(rsClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.contains("?class")))
                .thenReturn(
                    new org.json.JSONArray("[{\"class\": \"SAMPLE-CLASS\"}]")
                );
        }
      

        // Mock result when querying for metadata
        if(mockMeta) {
            when(rsClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                eq("SAMPLE-META-QUERY")))
                .thenReturn(
                    new org.json.JSONArray("[{\"Property\":\"propertyOne\",\"Value\":\"1.0\",\"Unit\":\"s\"}]")
                );
        }

        // Mock result when querying for measurements
        if(mockTime) {
            when(rsClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                eq("SAMPLE-TIME-QUERY")))
                .thenReturn(
                    new org.json.JSONArray("[{\"Measurement\":\"http://measurement-iri.com\",\"Name\":\"Measurement One\",\"Unit\":\"m/s\"}]")
                );
        }

        // Mock result when querying with a forced endpoint
        if(endpoint != null) {
            when(rsClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                eq("FORCED-ENDPOINT-QUERY")))
                .thenReturn(
                    new org.json.JSONArray("[{\"Property\":\"Forced\",\"Value\":\"Yes\"}]")
                );
        }

        return rsClient;
    }

    /**
     * Produces a mock TimeSeriesClient instance that returns the expected results.
     * 
     * @return
     */
    @SuppressWarnings("unchecked")
    private TimeSeriesClient<Instant> mockTimeSeriesClient() throws Exception {
        // Create mock Timeseries client
        TimeSeriesClient<Instant> tsClient = mock(TimeSeriesClient.class);

        when(tsClient.convertToJSON(
            ArgumentMatchers.anyList(),
            ArgumentMatchers.anyList(), 
            ArgumentMatchers.anyList(),
            ArgumentMatchers.anyList())
            ).thenCallRealMethod();

        when(tsClient.getTimeSeries(
            ArgumentMatchers.anyList(),
            ArgumentMatchers.any(Connection.class)))
            .thenReturn(
                new TimeSeries<Instant>(
                    Arrays.asList(Instant.MIN, Instant.EPOCH, Instant.MAX),
                    Arrays.asList("http://measurement-iri.com"),
                    Arrays.asList(Arrays.asList("1.0", "2.0", "3.0"))
                )
            );

        when(tsClient.getTimeSeriesWithinBounds(
            ArgumentMatchers.any(),
            ArgumentMatchers.any(),
            ArgumentMatchers.any(),
            ArgumentMatchers.any()))
            .thenReturn(
                new TimeSeries<Instant>(
                    Arrays.asList(Instant.MIN, Instant.EPOCH, Instant.MAX),
                    Arrays.asList("http://measurement-iri.com"),
                    Arrays.asList(Arrays.asList("1.0", "2.0", "3.0"))
                )
            );

        return tsClient;
    }

}
// End of class.