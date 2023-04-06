package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class Ifc2OntoBIMAgentTest {
    @TempDir
    private static Path tempDir;
    private static Path sampleTtl;
    private static Ifc2OntoBIMAgent agent;
    private static final Map<String, String> sampleConfig = new HashMap<>();
    private static final String sampleEndpoint = "https://www.example.org/sparql";
    private static final String sampleIfcOwlAPI = "https://ifcowlconverter:8080/ifcowlconverter/";
    private static final String KEY_BASEURI = "uri";
    private static final String KEY_METHOD = "method";
    private static final String GET_METHOD = "GET";
    private static final String POST_METHOD = "POST";
    private static final String KEY_ROUTE = "requestUrl";
    private static final String BASE_ROUTE = "https://localhost:3025/ifc2ontobim-agent/";
    private static final String GET_ROUTE = BASE_ROUTE + "status";
    private static final String POST_CONVERT_ROUTE = BASE_ROUTE + "convert";
    private static final String POST_CONVERT_NO_GEOM_ROUTE = BASE_ROUTE + "convert-no-geom";

    @BeforeAll
    static void init() throws IOException {
        sampleTtl = tempDir.resolve("test.ttl");
        List<String> lines = new ArrayList<>();
        lines.add("@prefix rdf: <" + JunitTestUtils.rdfUri + "> .");
        lines.add("@prefix bim: <" + JunitTestUtils.bimUri + "> .");
        lines.add("");
        lines.add("bim:Zone_19 rdf:type bim:Zone .");
        Files.write(sampleTtl, lines);
        sampleConfig.put("sparql.query.endpoint", sampleEndpoint);
        sampleConfig.put("sparql.update.endpoint", sampleEndpoint);
        sampleConfig.put("ifc.owl.agent", sampleIfcOwlAPI);
    }

    @BeforeEach
    void setup() {
        agent = new Ifc2OntoBIMAgent();
    }

    @Test
    void testProcessRequestParametersForUndefinedRoute() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, BASE_ROUTE);
        JSONObject response = agent.processRequestParameters(requestParams);
        assertTrue(response.keySet().contains("Runtime"));
        assertFalse(response.keySet().contains("Result"));
    }

    @Test
    void testProcessRequestParametersForStatusRouteViaGET() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_METHOD);
        requestParams.put(KEY_ROUTE, GET_ROUTE);
        JSONObject response = agent.processRequestParameters(requestParams);
        assertEquals("Agent is ready to receive requests.", response.getString("Result"));
    }

    @Test
    void testProcessRequestParametersForStatusRouteViaInvalidPOST() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, POST_METHOD);
        requestParams.put(KEY_ROUTE, GET_ROUTE);
        JSONObject response = agent.processRequestParameters(requestParams);
        assertEquals("Invalid request type! Route status can only accept GET request.", response.getString("Result"));
    }

    @Test
    void testProcessRequestParametersForConvertRouteViaPOST() {
        JSONObject requestParams = new JSONObject();
        String uri = "https://www.theworldavatar.com/ifc/building/";
        requestParams.put(KEY_BASEURI, uri);
        requestParams.put(KEY_METHOD, POST_METHOD);
        requestParams.put(KEY_ROUTE, POST_CONVERT_ROUTE);
        Set<String> ttlFileSet = genFileSet();
        try (MockedStatic<AccessClient> mockAccessClient = Mockito.mockStatic(AccessClient.class)) {
            // Stub the method to do nothing when called
            mockAccessClient.when(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString())).thenAnswer((Answer<Void>) invocation -> null);
            // Stub the method to return the sample config when ran
            mockAccessClient.when(AccessClient::retrieveClientProperties).thenAnswer((Answer<Map<String, String>>) invocation -> sampleConfig);
            // Stub the method to return the file list when it is running
            mockAccessClient.when(() -> AccessClient.listTTLFiles(Mockito.anyString())).thenAnswer((Answer<Set<String>>) invocation -> ttlFileSet);
            try (MockedConstruction<OntoBimConverter> mockBimConverter = Mockito.mockConstruction(OntoBimConverter.class)) {
                JSONObject response = agent.runAgent(new String[]{"test"}, true);
                // Verify methods was called
                mockAccessClient.verify(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString()));
                mockAccessClient.verify(() -> AccessClient.listTTLFiles(Mockito.anyString()));
                Mockito.verify(mockBimConverter.constructed().get(0)).convertOntoBIM(Mockito.anyString(), Mockito.anyBoolean());
                assertEquals("test.ifc has been successfully instantiated and uploaded to " + sampleEndpoint, response.getString("Result"));
            }
        }
    }

    @Test
    void testProcessRequestParametersEmptyParamsForConvertRouteViaPOST() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, POST_METHOD);
        requestParams.put(KEY_ROUTE, POST_CONVERT_ROUTE);
        JSONObject testMessage = agent.processRequestParameters(requestParams);
        String expected = "Request parameters are not defined correctly.";
        assertTrue(testMessage.toString().contains(expected));
    }

    @Test
    void testProcessRequestParametersForConvertRouteViaInvalidGET() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_ROUTE);
        requestParams.put(KEY_ROUTE, POST_CONVERT_ROUTE);
        JSONObject response = agent.processRequestParameters(requestParams);
        assertEquals("Invalid request type! Route convert can only accept POST request.", response.getString("Result"));
    }


    @Test
    void testProcessRequestParametersForConvertNoGeomRouteViaPOST() {
        JSONObject requestParams = new JSONObject();
        String uri = "https://www.theworldavatar.com/ifc/building/";
        requestParams.put(KEY_BASEURI, uri);
        requestParams.put(KEY_METHOD, POST_METHOD);
        requestParams.put(KEY_ROUTE, POST_CONVERT_NO_GEOM_ROUTE);
        Set<String> ttlFileSet = genFileSet();
        try (MockedStatic<AccessClient> mockAccessClient = Mockito.mockStatic(AccessClient.class)) {
            // Stub the method to do nothing when called
            mockAccessClient.when(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString())).thenAnswer((Answer<Void>) invocation -> null);
            // Stub the method to return the sample config when ran
            mockAccessClient.when(AccessClient::retrieveClientProperties).thenAnswer((Answer<Map<String, String>>) invocation -> sampleConfig);
            // Stub the method to return the file list when it is running
            mockAccessClient.when(() -> AccessClient.listTTLFiles(Mockito.anyString())).thenAnswer((Answer<Set<String>>) invocation -> ttlFileSet);
            try (MockedConstruction<OntoBimConverter> mockBimConverter = Mockito.mockConstruction(OntoBimConverter.class)) {
                JSONObject response = agent.runAgent(new String[]{"test"}, true);
                // Verify methods was called
                mockAccessClient.verify(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString()));
                mockAccessClient.verify(() -> AccessClient.listTTLFiles(Mockito.anyString()));
                Mockito.verify(mockBimConverter.constructed().get(0)).convertOntoBIM(Mockito.anyString(), Mockito.anyBoolean());
                assertEquals("test.ifc has been successfully instantiated and uploaded to " + sampleEndpoint, response.getString("Result"));
            }
        }
    }

    @Test
    void testProcessRequestParametersEmptyParamsForConvertNoGeomRouteViaPOST() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, POST_METHOD);
        requestParams.put(KEY_ROUTE, POST_CONVERT_NO_GEOM_ROUTE);
        JSONObject testMessage = agent.processRequestParameters(requestParams);
        String expected = "Request parameters are not defined correctly.";
        assertTrue(testMessage.toString().contains(expected));
    }

    @Test
    void testProcessRequestParametersForConvertNoGeomRouteViaInvalidGET() {
        JSONObject requestParams = new JSONObject();
        requestParams.put(KEY_METHOD, GET_ROUTE);
        requestParams.put(KEY_ROUTE, POST_CONVERT_NO_GEOM_ROUTE);
        JSONObject response = agent.processRequestParameters(requestParams);
        assertEquals("Invalid request type! Route convert-no-geom can only accept POST request.", response.getString("Result"));
    }


    @Test
    void testValidateInputForUri() {
        // Test both variations of accepted uris
        JSONObject requestParams = new JSONObject();
        String uri = "https://www.theworldavatar.com/";
        requestParams.put(KEY_BASEURI, uri);
        assertTrue(agent.validateInput(requestParams));

        requestParams = new JSONObject();
        uri = "https://www.theworldavatar.com/ifc/bim#";
        requestParams.put(KEY_BASEURI, uri);
        assertTrue(agent.validateInput(requestParams));

        requestParams = new JSONObject();
        uri = "default";
        requestParams.put(KEY_BASEURI, uri);
        assertTrue(agent.validateInput(requestParams));
    }

    @Test
    void testValidateInputForEndpoint() {
        // Test both variations of accepted uris
        JSONObject requestParams = new JSONObject();
        String uri = "https://www.theworldavatar.com/";
        requestParams.put(KEY_BASEURI, uri);
        assertTrue(agent.validateInput(requestParams));
    }

    @Test
    void testValidateInputFail() {
        // Test if the string does not start with https://www. , it returns false
        JSONObject requestParams = new JSONObject();
        String uri = "www.theworldavatar.com/";
        requestParams.put(KEY_BASEURI, uri);
        assertFalse(agent.validateInput(requestParams));

        // Test if the string does not end with / or #, it returns false
        requestParams = new JSONObject();
        uri = "https://www.theworldavatar.com/ifc";
        requestParams.put(KEY_BASEURI, uri);
        assertFalse(agent.validateInput(requestParams));
    }

    @Test
    void testValidateInputForEmptyParams() {
        JSONObject requestParams = new JSONObject();
        assertFalse(agent.validateInput(requestParams));
    }

    @Test
    void testRunAgent() {
        // Set up
        Set<String> ttlFileSet = genFileSet();
        try (MockedStatic<AccessClient> mockAccessClient = Mockito.mockStatic(AccessClient.class)) {
            // Stub the method to do nothing when called
            mockAccessClient.when(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString())).thenAnswer((Answer<Void>) invocation -> null);
            // Stub the method to return the sample config when ran
            mockAccessClient.when(AccessClient::retrieveClientProperties).thenAnswer((Answer<Map<String, String>>) invocation -> sampleConfig);
            // Stub the method to return the file list when it is running
            mockAccessClient.when(() -> AccessClient.listTTLFiles(Mockito.anyString())).thenAnswer((Answer<Set<String>>) invocation -> ttlFileSet);
            try (MockedConstruction<OntoBimConverter> mockBimConverter = Mockito.mockConstruction(OntoBimConverter.class)) {
                JSONObject response = agent.runAgent(new String[]{"test"}, true);
                // Verify methods was called
                mockAccessClient.verify(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString()));
                mockAccessClient.verify(() -> AccessClient.listTTLFiles(Mockito.anyString()));
                Mockito.verify(mockBimConverter.constructed().get(0)).convertOntoBIM(Mockito.anyString(), Mockito.anyBoolean());
                assertEquals("test.ifc has been successfully instantiated and uploaded to " + sampleEndpoint, response.getString("Result"));
            }
        }
    }

    @Test
    void testRunAgentNoTTLFile() {
        try (MockedStatic<AccessClient> mockAccessClient = Mockito.mockStatic(AccessClient.class)) {
            // Stub the method to do nothing when called
            mockAccessClient.when(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString())).thenAnswer((Answer<Void>) invocation -> null);
            // Stub the method to return the sample config when ran
            mockAccessClient.when(AccessClient::retrieveClientProperties).thenAnswer((Answer<Map<String, String>>) invocation -> sampleConfig);
            JSONObject response = agent.runAgent(new String[]{"test"}, true);
            // Verify method was called
            mockAccessClient.verify(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString()));
            mockAccessClient.verify(() -> AccessClient.listTTLFiles(Mockito.anyString()));
            // Verify that the OntoBimConverter class was not constructed as there is no TTL file listed
            assertEquals("No TTL file detected! Please place at least 1 IFC file input.", response.getString("Result"));
            // Verify that ontoBIM converter has not been created or any method executed as code fails before
            OntoBimConverter mockBimConverter = Mockito.mock(OntoBimConverter.class);
            Mockito.verifyNoInteractions(mockBimConverter);
        }
    }

    @Test
    void testRunAgentMultipleTTLFile() {
        // Set up
        Set<String> ttlFileSet = genMultipleFileSet();
        try (MockedStatic<AccessClient> mockAccessClient = Mockito.mockStatic(AccessClient.class)) {
            // Stub the method to do nothing when called
            mockAccessClient.when(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString())).thenAnswer((Answer<Void>) invocation -> null);
            // Stub the method to return the sample config when ran
            mockAccessClient.when(AccessClient::retrieveClientProperties).thenAnswer((Answer<Map<String, String>>) invocation -> sampleConfig);
            // Stub the method to return the file list when it is running
            mockAccessClient.when(() -> AccessClient.listTTLFiles(Mockito.anyString())).thenAnswer((Answer<Set<String>>) invocation -> ttlFileSet);
            JSONObject response = agent.runAgent(new String[]{"test"}, true);
            // Verify method was called
            mockAccessClient.verify(() -> AccessClient.sendPostRequest(Mockito.anyString(), Mockito.anyString()));
            mockAccessClient.verify(() -> AccessClient.listTTLFiles(Mockito.anyString()));
            // Verify that the OntoBimConverter class was not constructed as there is no TTL file listed
            assertEquals("More than one TTL file detected! Files cannot be converted or uploaded.", response.getString("Result"));
            // Verify that ontoBIM converter has not been created or any method executed as code fails before
            OntoBimConverter mockBimConverter = Mockito.mock(OntoBimConverter.class);
            Mockito.verifyNoInteractions(mockBimConverter);
        }
    }

    private Set<String> genFileSet() {
        Set<String> ttlFileSet = new HashSet<>();
        ttlFileSet.add(sampleTtl.toString());
        return ttlFileSet;
    }

    private Set<String> genMultipleFileSet() {
        Set<String> ttlFileSet = genFileSet();
        ttlFileSet.add("data/sample.ttl");
        return ttlFileSet;
    }
}