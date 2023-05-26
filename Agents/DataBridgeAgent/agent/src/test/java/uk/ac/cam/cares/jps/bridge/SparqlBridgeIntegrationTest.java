package uk.ac.cam.cares.jps.bridge;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.IntegrationTestUtils;

import java.util.Queue;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SparqlBridgeIntegrationTest {
    @BeforeEach
    void setupData() {
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.SRC_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_INSERT);
    }

    @AfterEach
    void clearNamespace() {
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.TGT_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
    }

    @AfterAll
    static void cleanUp() {
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.SRC_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
    }

    @Test
    void testTransfer() {
        // Verify that the target endpoint has no values before transferring
        Queue<String> response = IntegrationTestUtils.query(IntegrationTestUtils.TGT_SPARQL_ENDPOINT);
        assertTrue(response.isEmpty());
        // Execute the class and its method
        SparqlBridge testBridge = new SparqlBridge(IntegrationTestUtils.SRC_SPARQL_ENDPOINT, IntegrationTestUtils.TGT_SPARQL_ENDPOINT);
        testBridge.transfer();
        // Verify the response is correct
        response = IntegrationTestUtils.query(IntegrationTestUtils.TGT_SPARQL_ENDPOINT);
        assertEquals(IntegrationTestUtils.SAMPLE_RDF_SUBJECT, response.poll());
        assertEquals(IntegrationTestUtils.SAMPLE_RDF_PREDICATE, response.poll());
        assertEquals(IntegrationTestUtils.SAMPLE_RDF_OBJECT, response.poll());
    }
}