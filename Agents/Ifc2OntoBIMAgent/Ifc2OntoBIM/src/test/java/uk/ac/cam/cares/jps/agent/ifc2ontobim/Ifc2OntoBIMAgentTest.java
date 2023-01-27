package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class Ifc2OntoBIMAgentTest {
    @TempDir
    private static Path tempDir;
    private static Path sampleTtl;
    private static Ifc2OntoBIMAgent agent;
    private static final String KEY_BASEURI = "uri";
    private static final String KEY_ENDPOINT = "endpoint";

    @BeforeAll
    static void init() throws IOException {
        sampleTtl = tempDir.resolve("test.ttl");
        List<String> lines = new ArrayList<>();
        lines.add("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .");
        lines.add("@prefix bim: <http://www.theworldavatar.com/ontology/ontobim/ontoBIM#> .");
        lines.add("");
        lines.add("bim:Zone_19 rdf:type bim:Zone .");
        Files.write(sampleTtl, lines);
    }

    @BeforeEach
    void setup() {
        agent = new Ifc2OntoBIMAgent();
    }

    @Test
    void testProcessRequestParameters() {
        JSONObject requestParams = new JSONObject();
        String uri = "http://www.theworldavatar.com/ifc/building/";
        requestParams.put(KEY_BASEURI, uri);
        Set<String> ttlFileSet = genFileSet();

        JSONObject testMessage;
        try (MockedConstruction<OntoBimConverter> mockBimConverter = Mockito.mockConstruction(OntoBimConverter.class,
                // Stub the method to return the file list when it is running
                (mock, context) -> Mockito.when(mock.listTTLFiles(Mockito.any())).thenReturn(ttlFileSet))) {
                testMessage = agent.processRequestParameters(requestParams);
                // Verify methods was called
                Mockito.verify(mockBimConverter.constructed().get(0)).listTTLFiles(Mockito.any());
                Mockito.verify(mockBimConverter.constructed().get(0)).convertOntoBIM(Mockito.anyString());
        }
        String expected = ".ttl has been successfully converted!\",\"All ttl files have been generated in OntoBIM. Please check the directory.";
        assertTrue(testMessage.toString().contains(expected));
    }

    @Test
    void testProcessRequestParametersEmptyParams() {
        JSONObject requestParams = new JSONObject();
        JSONObject testMessage = agent.processRequestParameters(requestParams);
        String expected = "Request parameters are not defined correctly.";
        assertTrue(testMessage.toString().contains(expected));
    }

    @Test
    void testValidateInputForUri() {
        // Test both variations of accepted uris
        JSONObject requestParams = new JSONObject();
        String uri = "http://www.theworldavatar.com/";
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
        String uri = "http://www.theworldavatar.com/";
        String endpoint = "http://localhost:9999/blazegraph/namespace/test/sparql";
        requestParams.put(KEY_BASEURI, uri);
        requestParams.put(KEY_ENDPOINT, endpoint);
        assertTrue(agent.validateInput(requestParams));
    }

    @Test
    void testValidateInputFail() {
        // Test if the string does not start with http://www. , it returns false
        JSONObject requestParams = new JSONObject();
        String uri = "www.theworldavatar.com/";
        requestParams.put(KEY_BASEURI, uri);
        assertFalse(agent.validateInput(requestParams));

        // Test if the string does not end with / or #, it returns false
        requestParams = new JSONObject();
        uri = "https://www.theworldavatar.com/ifc";
        requestParams.put(KEY_BASEURI, uri);
        assertFalse(agent.validateInput(requestParams));

        // Test bad endpoint url
        requestParams = new JSONObject();
        uri = "default";
        String endpoint = "localhost:9999";
        requestParams.put(KEY_BASEURI, uri);
        requestParams.put(KEY_ENDPOINT, endpoint);
        assertFalse(agent.validateInput(requestParams));

        // Test bad endpoint url
        requestParams = new JSONObject();
        endpoint = "http://localhost:9999/blazegraph/namespace/bah";
        requestParams.put(KEY_BASEURI, uri);
        requestParams.put(KEY_ENDPOINT, endpoint);
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
        try (MockedConstruction<OntoBimConverter> mockOwlConverter = Mockito.mockConstruction(OntoBimConverter.class,
                // Stub the method to return the file list when it is running
                (mock, context) -> Mockito.when(mock.listTTLFiles(Mockito.any())).thenReturn(ttlFileSet))) {
            try (MockedConstruction<OntoBimConverter> mockBimConverter = Mockito.mockConstruction(OntoBimConverter.class)) {
                agent.runAgent(new String[]{});
                // Verify methods was called
                Mockito.verify(mockOwlConverter.constructed().get(0)).listTTLFiles(Mockito.any());
                Mockito.verify(mockBimConverter.constructed().get(0)).convertOntoBIM(Mockito.anyString());
            }
        }
    }

    @Test
    void testRunAgentNoTTLFile() {
        try (MockedConstruction<OntoBimConverter> mockOwlConverter = Mockito.mockConstruction(OntoBimConverter.class)) {
            try (MockedConstruction<OntoBimConverter> mockBimConverter = Mockito.mockConstruction(OntoBimConverter.class)) {
                JSONObject message = agent.runAgent(new String[]{});
                // Verify method was called
                Mockito.verify(mockOwlConverter.constructed().get(0)).listTTLFiles(Mockito.any());
                // Verify that the OntoBimConverter class was not constructed as there is no TTL file listed
                assertThrows(IndexOutOfBoundsException.class, () -> mockBimConverter.constructed().get(0));
                assertEquals("No TTL file detected! Please place at least 1 IFC file input.", message.getString("Result"));
            }
        }
    }

    private Set<String> genFileSet() {
        Set<String> ttlFileSet = new HashSet<>();
        ttlFileSet.add(sampleTtl.toString());
        return ttlFileSet;
    }
}