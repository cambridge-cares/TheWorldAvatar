package com.cmclinnovations.stack.clients.core.datasets;

import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.datatypes.xsd.XSDDateTime;
import org.apache.jena.graph.GraphMemFactory;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.impl.ModelCom;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
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

    private String testName;
    private Map<String, Integer> fileIndecies = new HashMap<>();

    @BeforeEach
    void setUp(TestInfo testInfo) {
        testName = testInfo.getDisplayName();
        fileIndecies.put(testName, 0);
    }

    @BeforeEach
    void cleanNamespace() {
        remoteStoreClient.executeUpdate(BlazegraphContainer.DELETE_ALL_QUERY);
    }

    @Test
    void testAddDataset() {
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunQuery(dataset, Service.NONE);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").build();
            buildAndRunQuery(dataset, Service.NONE);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset, Service.NONE);
        });
    }

    @Test
    void testAddExternalDataset() {
        Assertions.assertAll(() -> {
            Assertions.assertAll(Stream.of("A", "B", "C").map(name -> () -> {
                Dataset dataset = new DatasetBuilder(name).build();
                buildAndRunQuery(dataset, Service.NONE);
            }));
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withExternalDatasetNames(List.of("A", "B")).build();
            buildAndRunQuery(dataset, Service.NONE);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withExternalDatasetNames(List.of("B", "C")).build();
            buildAndRunQuery(dataset, Service.NONE);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withExternalDatasetNames(List.of("A", "B")).build();
            buildAndRunQuery(dataset, Service.NONE);
        });
    }

    @Test
    void testAddBlazegraph() {
        writeBlazegraphConfig();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunQuery(dataset, Service.BLAZEGRAPH);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withNamespace(new Namespace("namespace1")).build();
            buildAndRunQuery(dataset, Service.BLAZEGRAPH);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset, Service.BLAZEGRAPH);
        });
    }

    @Test
    void testAddPostGIS() {
        writePostGISConfig();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunQuery(dataset, Service.POSTGIS);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withDatabase("database1").build();
            buildAndRunQuery(dataset, Service.POSTGIS);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset, Service.POSTGIS);
        });
    }

    @Test
    void testAddGeoServer() {
        writePostGISConfig();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunQuery(dataset, Service.GEOSERVER);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withWorkspaceName("workspace1").build();
            buildAndRunQuery(dataset, Service.GEOSERVER);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset, Service.GEOSERVER);
        });
    }

    @Test
    void testAddOntop() {
        writePostGISConfig();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withOntopMappings(List.of("ontop.obda")).build();
            writeOntopConfig(dataset.getOntopName());
            buildAndRunQuery(dataset, Service.ONTOP);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withOntopMappings(List.of("ontop.obda")).build();
            buildAndRunQuery(dataset, Service.ONTOP);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withOntopMappings(List.of("ontop.obda")).build();
            writeOntopConfig(dataset.getOntopName());
            buildAndRunQuery(dataset, Service.ONTOP);
        });
    }

    @SuppressWarnings("null")
    @Test
    void testAddDataSubset() throws JsonMappingException, JsonProcessingException {
        ObjectMapper mapper = JsonHelper.getMapper();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy nice TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset, Service.NONE);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy bad TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset, Service.NONE);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy awsome TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset, Service.NONE);
        });
    }

    @Test
    void testCatalogingTrivial() {
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunCatalogingQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").build();
            buildAndRunCatalogingQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunCatalogingQuery(dataset);
        });
    }

    private void writeBlazegraphConfig() {
        DockerClient dockerClient = DockerClient.getInstance();
        dockerClient.writeEndpointConfig(
                new BlazegraphEndpointConfig("blazegraph", blazegraph.getHost(),
                        "8080",
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

    private void buildAndRunQuery(Dataset dataset, Service service) {
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
        runAndCompareQuery(query);
    }

    private void buildAndRunCatalogingQuery(Dataset dataset) {
        DCATUpdateQuery dcatUpdateQuery = new DCATUpdateQuery();
        String query = dcatUpdateQuery.getQueryStringForCataloging(dataset);

        runAndCompareQuery(query);
    }

    private void runAndCompareQuery(String query) {
        remoteStoreClient.executeUpdate(query);

        Model results = remoteStoreClient.executeConstruct(BlazegraphContainer.CONSTRUCT_ALL_QUERY);

        checkExpectedFile(results);

        Model expectedResults = genericiseModel(readTurtleFromFile());
        Model actualResults = genericiseModel(results);

        Assertions.assertTrue(expectedResults.isIsomorphicWith(actualResults), () -> {
            writeTurtleToFile(results);
            return getMessage("has different statements.", expectedResults, actualResults);
        });

    }

    private String getMessage(String message, Model expected, Model actual) {
        StringBuilder stringBuilder = new StringBuilder(testName);
        stringBuilder.append(" ").append(fileIndecies.get(testName)).append(" ").append(message);

        stringBuilder.append("\nExpected:\n").append(toTurtle(expected));

        stringBuilder.append("\nActual:\n").append(toTurtle(actual));

        return stringBuilder.toString();
    }

    private void checkExpectedFile(Model results) {
        fileIndecies.computeIfPresent(testName, (n, i) -> i + 1);
        Path dirPath = getCheckedDir();
        Path path = dirPath.resolve(getExpectedFilename());

        Assumptions.assumeTrue(Files.exists(path), () -> {
            writeTurtleToFile(results);
            return "File '" + getExpectedFilename()
                    + "' needs to be checked and moved from the 'forReview' directory to the correct test resources directory.";
        });
    }

    private Model readTurtleFromFile() {
        Path dirPath = getCheckedDir();
        Model model = new ModelCom(GraphMemFactory.createDefaultGraph());
        Path path = dirPath.resolve(getExpectedFilename());

        model.read(path.toUri().toString());

        return model;
    }

    private String getExpectedFilename() {
        return testName + fileIndecies.get(testName) + ".ttl";
    }

    private Path getCheckedDir() {
        return Assertions.assertDoesNotThrow(() -> Path.of(this.getClass().getResource(".").toURI()));
    }

    private Path getForReviewDir() {
        return Path.of("testing_temp", "forReview", "DCATUpdateQueryTest");
    }

    private void writeTurtleToFile(Model model) {
        Path dirPath = getForReviewDir();
        try {
            Files.createDirectories(dirPath);
            try (FileWriter writer = new FileWriter(dirPath.resolve(getExpectedFilename()).toFile())) {
                model.write(writer, "TURTLE");
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    private Model genericiseModel(Model modelIn) {
        Map<RDFNode, RDFNode> specificToGeneric = modelIn.listSubjects().toSet().stream()
                .collect(Collectors.toMap(s -> s, s -> ResourceFactory.createResource()));
        specificToGeneric.entrySet().stream()
                .collect(Collectors.toMap(e -> ResourceFactory.createPlainLiteral(e.getKey().toString()),
                        e -> ResourceFactory.createPlainLiteral(e.getValue().toString())));
        List<Literal> dateTimes = modelIn.listObjects().toSet().stream().filter(o -> o.isLiteral())
                .map(o -> o.asLiteral()).filter(o -> o.getDatatype().equals(XSDDatatype.XSDdateTime))
                .sorted((o1, o2) -> ((XSDDateTime) o1.getValue()).compare((XSDDateTime) o2.getValue()))
                .collect(Collectors.toList());
        specificToGeneric.putAll(specificToGeneric.entrySet().stream()
                .collect(Collectors.toMap(e -> ResourceFactory.createPlainLiteral(e.getKey().toString()),
                        e -> ResourceFactory.createPlainLiteral("An URI as a literal!"))));
        specificToGeneric.putAll(dateTimes.stream().collect(
                Collectors.toMap(dt -> dt,
                        dt -> ResourceFactory.createTypedLiteral(
                                XSDDatatype.XSDdateTime.parseValidated("1970-01-0"
                                        + (1 + Integer.toString(dateTimes.indexOf(dt))) + "T00:00:00.000Z")))));

        Model modelOut = new ModelCom(GraphMemFactory.createDefaultGraph());
        modelIn.listStatements().forEach(
                state -> modelOut.add((Resource) specificToGeneric.get(state.getSubject()), state.getPredicate(),
                        specificToGeneric.getOrDefault(state.getObject(), state.getObject())));
        return modelOut;
    }

    private String toTurtle(Model results) {
        StringWriter writer = new StringWriter();
        results.write(writer, "TURTLE");
        String turtle = writer.toString();
        return turtle;
    }
}
