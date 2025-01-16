package com.cmclinnovations.stack.clients.blazegraph;

import java.nio.file.Path;
import java.util.Properties;

import org.json.JSONArray;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import com.cmclinnovations.stack.clients.utils.BlazegraphContainer;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@Testcontainers
class BlazegraphClientTest {

    private static final String COUNT_QUERY = "SELECT (COUNT(*) as ?count) WHERE {?s ?p ?o}";
    private static final String INSERT_QUERY = "prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n" +
            "INSERT DATA {\n" + //
            "  <http://example.com/a> a <http://example.com/A>.\n" +
            "  <http://example.com/A> rdfs:subClassOf <http://example.com/B>.\n" +
            "}";

    @Container
    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();

    private static final BlazegraphClient blazegraphClient = BlazegraphClient.getInstance();;

    static {
        blazegraph.writeBlazegraphConfig();
    }

    @Test
    void testNonExistantNamespace() {
        RemoteStoreClient remoteStoreClient = blazegraphClient.getRemoteStoreClient("nonExistantNamespace");
        Assertions.assertThrows(JPSRuntimeException.class,
                () -> remoteStoreClient.execute(COUNT_QUERY));
    }

    @Test
    void testCreateNamespace() {
        String namespace = "basicNamespace";
        blazegraphClient.createNamespace(namespace);
        RemoteStoreClient remoteStoreClient = blazegraphClient.getRemoteStoreClient(namespace);
        int expectedBaseCount = 187;
        assertTripleCount(remoteStoreClient, expectedBaseCount);
        remoteStoreClient.executeUpdate(INSERT_QUERY);
        assertTripleCount(remoteStoreClient, expectedBaseCount + 9);
    }

    @Test
    void testCreateNamespaceWithProperties() {
        String namespace = "namespaceWithOutInference";
        Properties properties = new Properties();
        properties.setProperty("com.bigdata.rdf.store.AbstractTripleStore.axiomsClass",
                "com.bigdata.rdf.axioms.NoAxioms");
        blazegraphClient.createNamespace(namespace, properties);
        RemoteStoreClient remoteStoreClient = blazegraphClient.getRemoteStoreClient(namespace);
        assertTripleCount(remoteStoreClient, 0);
        remoteStoreClient.executeUpdate(INSERT_QUERY);
        assertTripleCount(remoteStoreClient, 2);
    }

    @Test
    void testFilterQuery() {
        String query = "SELECT * WHERE { SERVICE <ontop> {?s ?p ?o} SERVICE <ontop> {?a ?b ?c}}";
        String endpointUrl = "http://ontop/sparql";
        String expected = "SELECT * WHERE { SERVICE <http://ontop/sparql> {?s ?p ?o} SERVICE <http://ontop/sparql> {?a ?b ?c}}";
        Assertions.assertEquals(expected, blazegraphClient.filterQuery(query, endpointUrl));
    }

    @Test
    void testGetEndpoint() {
        BlazegraphEndpointConfig endpoint = blazegraphClient.readEndpointConfig();
        Assertions.assertEquals(blazegraph.getURL(), endpoint.getUrl("kb"));
    }

    @Test
    void testGetInstance() {
        Assertions.assertSame(BlazegraphClient.getInstance(), BlazegraphClient.getInstance());
    }

    @Test
    void testRemoveNamespace() {
        String namespace = "tempNamespace";
        blazegraphClient.createNamespace(namespace);
        RemoteStoreClient remoteStoreClient = blazegraphClient.getRemoteStoreClient(namespace);
        int expectedBaseCount = 187;
        assertTripleCount(remoteStoreClient, expectedBaseCount);
        blazegraphClient.removeNamespace(namespace);
        Assertions.assertThrows(JPSRuntimeException.class,
                () -> remoteStoreClient.execute(COUNT_QUERY));
    }

    @Test
    void testRemoveNonExistantNamespace() {
        String namespace = "nonExistantNamespace";
        RemoteStoreClient remoteStoreClient = blazegraphClient.getRemoteStoreClient(namespace);
        blazegraphClient.removeNamespace(namespace);
        Assertions.assertThrows(JPSRuntimeException.class,
                () -> remoteStoreClient.execute(COUNT_QUERY));
    }

    @Test
    void testUploadRDFFiles() {
        String namespace = "fileNamespace";
        blazegraphClient.createNamespace(namespace);
        RemoteStoreClient remoteStoreClient = blazegraphClient.getRemoteStoreClient(namespace);

        int expectedBaseCount = 187;
        assertTripleCount(remoteStoreClient, expectedBaseCount);

        Path path = Assertions.assertDoesNotThrow(
                () -> Path.of(BlazegraphClientTest.class.getResource(".").toURI()));

        blazegraphClient.uploadRDFFiles(path, namespace);
        assertTripleCount(remoteStoreClient, expectedBaseCount + 9);
    }

    private void assertTripleCount(RemoteStoreClient remoteStoreClient, int expectedCount) {
        JSONArray results = Assertions.assertDoesNotThrow(
                () -> remoteStoreClient.executeQuery(COUNT_QUERY));
        Assertions.assertEquals(expectedCount, results.getJSONObject(0).getInt("count"));
    }
}
