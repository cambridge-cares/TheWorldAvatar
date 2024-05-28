package com.cmclinnovations.stack.clients.core.datasets;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.json.JSONArray;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.junit.jupiter.Container;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.DockerClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.utils.BlazegraphContainer;
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

class DCATUpdateQueryTest {

    @Container
    private final BlazegraphContainer blazegraph = new BlazegraphContainer();

    private final RemoteStoreClient remoteStoreClient;

    DCATUpdateQueryTest() {
        blazegraph.start();
        remoteStoreClient = blazegraph.getRemoteStoreClient();
    }

    @BeforeEach
    void cleanNamespace() {
        remoteStoreClient.executeUpdate(BlazegraphContainer.DELETE_ALL_QUERY);
    }

    @Test
    void testAddDataset() {
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunQuery(dataset, Service.NONE, 5);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").build();
            buildAndRunQuery(dataset, Service.NONE, 6);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset, Service.NONE, 11);
        }
    }

    @Test
    void testAddExternalDataset() {
        (new TreeMap<>(Map.of("A", 5, "B", 10, "C", 15))).forEach((name, count) -> {
            Dataset dataset = new DatasetBuilder(name).build();
            buildAndRunQuery(dataset, Service.NONE, count);
        });
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withExternalDatasetNames(List.of("A", "B")).build();
            buildAndRunQuery(dataset, Service.NONE, 22);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withExternalDatasetNames(List.of("B", "C")).build();
            buildAndRunQuery(dataset, Service.NONE, 23);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withExternalDatasetNames(List.of("A", "B")).build();
            buildAndRunQuery(dataset, Service.NONE, 30);
        }
    }

    @Test
    void testAddBlazegraph() {
        writeBlazegraphConfig();
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunQuery(dataset, Service.BLAZEGRAPH, 11);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withNamespace(new Namespace("namespace1")).build();
            buildAndRunQuery(dataset, Service.BLAZEGRAPH, 16);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset, Service.BLAZEGRAPH, 27);
        }
    }

    @Test
    void testAddPostGIS() {
        writePostGISConfig();
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunQuery(dataset, Service.POSTGIS, 11);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withDatabase("database1").build();
            buildAndRunQuery(dataset, Service.POSTGIS, 16);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset, Service.POSTGIS, 27);
        }
    }

    @Test
    void testAddGeoServer() {
        writePostGISConfig();
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunQuery(dataset, Service.GEOSERVER, 17);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withWorkspaceName("workspace1").build();
            buildAndRunQuery(dataset, Service.GEOSERVER, 22);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset, Service.GEOSERVER, 39);
        }
    }

    @Test
    void testAddOntop() {
        writePostGISConfig();
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withOntopMappings(List.of("ontop.obda")).build();
            writeOntopConfig(dataset.getOntopName());
            buildAndRunQuery(dataset, Service.ONTOP, 18);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withOntopMappings(List.of("ontop.obda")).build();
            buildAndRunQuery(dataset, Service.ONTOP, 19);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withOntopMappings(List.of("ontop.obda")).build();
            writeOntopConfig(dataset.getOntopName());
            buildAndRunQuery(dataset, Service.ONTOP, 37);
        }
    }

    @SuppressWarnings("null")
    @Test
    void testAddDataSubset() throws JsonMappingException, JsonProcessingException {
        ObjectMapper mapper = JsonHelper.getMapper();
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy nice TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset, Service.NONE, 11);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy bad TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset, Service.NONE, 13);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy awsome TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset, Service.NONE, 24);
        }
    }

    @Test
    void testCatalogingTrivial() {
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunCatalogingQuery(dataset, 5);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").build();
            buildAndRunCatalogingQuery(dataset, 6);
        }
        {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunCatalogingQuery(dataset, 11);
        }
    }

    private void writeBlazegraphConfig() {
        DockerClient dockerClient = DockerClient.getInstance();
        dockerClient.writeEndpointConfig(
                new BlazegraphEndpointConfig("blazegraph", blazegraph.getHost(),
                        blazegraph.getFirstMappedPort().toString(),
                        BlazegraphContainer.USERNAME, BlazegraphContainer.PASSWORD));
    }

    private void writePostGISConfig() {
        DockerClient dockerClient = DockerClient.getInstance();
        dockerClient.writeEndpointConfig(
                new PostGISEndpointConfig("postgis", "test-postgis", "1234",
                        "user", "passwordFile"));
    }

    private void writeOntopConfig(String name) {
        DockerClient dockerClient = DockerClient.getInstance();
        dockerClient.writeEndpointConfig(
                new OntopEndpointConfig(name, StackClient.prependStackName(name).replace('_', '-'), "5678"));
    }

    private static enum Service {
        NONE,
        BLAZEGRAPH,
        POSTGIS,
        GEOSERVER,
        ONTOP
    }

    private void buildAndRunQuery(Dataset dataset, Service service, int expected) {
        DCATUpdateQuery dcatUpdateQuery = new DCATUpdateQuery();
        dcatUpdateQuery.addDataset(dataset);

        dataset.getExternalDatasetNames().forEach(dcatUpdateQuery::addExternalDataset);

        dataset.getDataSubsets().forEach(dcatUpdateQuery::addDataSubset);

        switch (service) {
            case NONE:
                break;
            case BLAZEGRAPH:
                dcatUpdateQuery.addBlazegraphServer(dataset);
                break;
            case POSTGIS:
                dcatUpdateQuery.addPostGISServer(dataset);
                break;
            case GEOSERVER:
                dcatUpdateQuery.addPostGISServer(dataset);
                dcatUpdateQuery.addGeoServerServer(dataset);
                break;
            case ONTOP:
                dcatUpdateQuery.addPostGISServer(dataset);
                dcatUpdateQuery.addOntopServer(dataset);
                break;
        }

        String query = dcatUpdateQuery.getQuery();
        remoteStoreClient.executeUpdate(query);

        JSONArray results = remoteStoreClient.executeQuery(BlazegraphContainer.SELECT_ALL_QUERY);

        Assertions.assertEquals(expected, results.length(), results.toString());
    }

    private void buildAndRunCatalogingQuery(Dataset dataset, int expectedNumberOfTriples) {
        DCATUpdateQuery dcatUpdateQuery = new DCATUpdateQuery();
        String query = dcatUpdateQuery.getQueryStringForCataloging(dataset);

        remoteStoreClient.executeUpdate(query);
        List<Object> result = remoteStoreClient.executeQuery(BlazegraphContainer.SELECT_ALL_QUERY).toList();

        Assertions.assertEquals(expectedNumberOfTriples, result.size());
    }
}
