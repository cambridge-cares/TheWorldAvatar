package com.cmclinnovations.stack.clients.core.datasets;

import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.core.datasets.DatasetBuilder.Service;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.utils.BlazegraphContainer;
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

class DCATUpdateQueryTest {

    @Container
    private static final BlazegraphContainer blazegraph = new BlazegraphContainer();

    private static final RemoteStoreClient remoteStoreClient = blazegraph.getRemoteStoreClient();

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
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset);
        });
    }

    @Test
    void testAddExternalDataset() {
        Assertions.assertAll(() -> {
            Assertions.assertAll(Stream.of("A", "B", "C").map(name -> () -> {
                Dataset dataset = new DatasetBuilder(name).build();
                buildAndRunQuery(dataset);
            }));
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withExternalDatasetNames(List.of("A", "B")).build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withExternalDatasetNames(List.of("B", "C")).build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withExternalDatasetNames(List.of("A", "B")).build();
            buildAndRunQuery(dataset);
        });
    }

    @Test
    void testAddOntologyDataset() {
        Assertions.assertAll(() -> {
            Assertions.assertAll(Stream.of("A", "B", "C").map(name -> () -> {
                Dataset dataset = new DatasetBuilder(name).build();
                buildAndRunQuery(dataset);
            }));
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withOntologyDataset(List.of("A", "B")).build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withOntologyDataset(List.of("B", "C")).build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withOntologyDataset(List.of("A", "B")).build();
            buildAndRunQuery(dataset);
        });
    }

    @Test
    void testAddBlazegraph() {
        writeBlazegraphConfig();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withServices(Service.BLAZEGRAPH).build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withServices(Service.BLAZEGRAPH)
                    .withNamespace(new Namespace("namespace1")).build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withServices(Service.BLAZEGRAPH).build();
            buildAndRunQuery(dataset);
        });
    }

    @Test
    void testAddPostGIS() {
        writePostGISConfig();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withServices(Service.POSTGIS).build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withServices(Service.POSTGIS).withDatabase("database1").build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withServices(Service.POSTGIS).build();
            buildAndRunQuery(dataset);
        });
    }

    @Test
    void testAddGeoServer() {
        writePostGISConfig();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withServices(Service.GEOSERVER).build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withServices(Service.GEOSERVER).withWorkspaceName("workspace1")
                    .build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withServices(Service.GEOSERVER).build();
            buildAndRunQuery(dataset);
        });
    }

    @Test
    void testAddOntop() {
        writePostGISConfig();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").withOntopMappings(List.of("ontop.obda"))
                    .withServices(Service.ONTOP).build();
            writeOntopConfig(dataset.getOntopName());
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").withOntopMappings(List.of("ontop.obda"))
                    .withServices(Service.ONTOP).build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").withOntopMappings(List.of("ontop.obda"))
                    .withServices(Service.ONTOP).build();
            writeOntopConfig(dataset.getOntopName());
            buildAndRunQuery(dataset);
        });
    }

    @SuppressWarnings("null")
    @Test
    void testAddDataSubset() {
        writeBlazegraphConfig();
        ObjectMapper mapper = JsonHelper.getMapper();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy nice TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy bad TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy awsome TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset);
        });
    }

    @SuppressWarnings("null")
    @Test
    void testAddDataSubsetSkipping() {
        writeBlazegraphConfig();
        ObjectMapper mapper = JsonHelper.getMapper();
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy nice TBox.\",\"subdirectory\":\"tbox\",\"skip\": true}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy nice TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy bad TBox.\",\"subdirectory\":\"tbox2\",\"skip\": true}",
                            TBoxCSV.class)))
                    .build();
            buildAndRunQuery(dataset);
        });
    }

    @Test
    void testAddMetadataSimple() {
        writeBlazegraphConfig();

        Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                .withDatasetDirectory("test1")
                .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                .withMetadataRDF(new Metadata(Map.of(), Optional.of("<http://p/a> <http://q/b> <http://r/c>")))
                .build();
        buildAndRunQuery(dataset);
    }

    @Test
    void testAddMetadataPrefixOnly() {
        writeBlazegraphConfig();

        Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                .withDatasetDirectory("test1")
                .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                .withMetadataRDF(new Metadata(Map.of("p", "http://p/"), Optional.empty()))
                .build();
        buildAndRunQuery(dataset);
    }

    @Test
    void testAddMetadataPrefix() {
        writeBlazegraphConfig();

        Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                .withDatasetDirectory("test1")
                .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                .withMetadataRDF(new Metadata(Map.of("p", "http://p/"), Optional.of("p:a <http://q/b> <http://r/c>")))
                .build();
        buildAndRunQuery(dataset);
    }

    @Test
    void testAddMetadataMissingPrefixes() {
        writeBlazegraphConfig();

        Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                .withDatasetDirectory("test1")
                .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                .withMetadataRDF(new Metadata(Map.of(),
                        Optional.of("<http://p/a> rdf:type <http://p/A>; rdfs:label \"Hello\"")))
                .build();
        Assertions.assertThrows(JPSRuntimeException.class, () -> buildAndRunQuery(dataset));
    }

    @Test
    void testAddMetadataUseBuiltinPrefix() {
        writeBlazegraphConfig();

        Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                .withDatasetDirectory("test1")
                .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                .withMetadataRDF(new Metadata(Map.of("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
                        Optional.of("<http://p/a> rdfs:label \"Hello\"")))
                .build();
        buildAndRunQuery(dataset);
    }

    @Test
    void testAddMetadataOverrideBuiltinPrefixes() {
        writeBlazegraphConfig();

        Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                .withDatasetDirectory("test1")
                .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                .withMetadataRDF(new Metadata(
                        Map.of("rdf", "http://cmcl.io/rdf#", "rdfs", "http://cmcl.io/rdfs#", "dcat",
                                "http://purl.org/dc/terms/", "xsd", "http://cmcl.io/xsd#"),
                        Optional.of("<http://p/a> rdf:type <http://p/A>; rdfs:label \"Hello\"^^xsd:string")))
                .build();
        buildAndRunQuery(dataset);
    }

    @Test
    void testAddMetadataFromFile() {
        writeBlazegraphConfig();

        Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                .withDatasetDirectory("test1")
                .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                .withMetadataRDF(new Metadata(Map.of(),
                        Optional.of(
                                "@src/test/resources/com/cmclinnovations/stack/clients/core/datasets/datasetMetadata.ttl")))
                .build();
        buildAndRunQuery(dataset);
    }

    @Test
    void testCatalogingTrivial() {
        Assertions.assertAll(() -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1").build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2").build();
            buildAndRunQuery(dataset);
        }, () -> {
            Dataset dataset = new DatasetBuilder("testDataset3").withDescription("Dataset for testing3")
                    .withDatasetDirectory("test3").build();
            buildAndRunQuery(dataset);
        });
    }

    @SuppressWarnings("null")
    @Test
    void testRemovingDataset() {
        writeBlazegraphConfig();
        ObjectMapper mapper = JsonHelper.getMapper();

        Dataset dataset1;
        Dataset dataset2;
        try {
            dataset1 = new DatasetBuilder("testDataset").withDescription("Dataset for testing")
                    .withDatasetDirectory("test1")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy nice TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();

            dataset2 = new DatasetBuilder("testDataset2").withDescription("Dataset for testing2")
                    .withDatasetDirectory("test2")
                    .withRdfType("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#TBox")
                    .withDataSubsets(List.of(mapper.readValue(
                            "{\"type\": \"tboxcsv\",\"name\":\"Tbox1\",\"description\":\"A realy bad TBox.\",\"subdirectory\":\"tbox\"}",
                            TBoxCSV.class)))
                    .build();

            Assertions.assertAll(() -> {
                buildAndRunDelete(dataset1);
            }, () -> {
                buildAndRunQuery(dataset1);
            }, () -> {
                buildAndRunDelete(dataset1);
            }, () -> {
                buildAndRunQuery(dataset1);
            }, () -> {
                buildAndRunQuery(dataset2);
            }, () -> {
                buildAndRunDelete(dataset1);
            });

        } catch (JsonProcessingException e) {
            Assertions.fail(e);
        }
    }

    private void writeBlazegraphConfig() {
        BlazegraphClient.writeEndpointConfig(
                new BlazegraphEndpointConfig("blazegraph", "blazegraph",
                        "8080",
                        BlazegraphContainer.USERNAME, BlazegraphContainer.PASSWORD));
    }

    private void writePostGISConfig() {
        PostGISClient.writeEndpointConfig(
                new PostGISEndpointConfig("postgis", "test-postgis", "1234",
                        "user", "passwordFile"));
    }

    private void writeOntopConfig(String name) {
        OntopClient.writeEndpointConfig(
                new OntopEndpointConfig(name, StackClient.prependStackName(name).replace('_', '-'), "5678"));
    }

    private void buildAndRunQuery(Dataset dataset) {

        runUpdate(dataset);

        checkResults();
    }

    private void buildAndRunDelete(Dataset dataset) {

        runDelete(dataset);

        checkResults();
    }

    private void checkResults() {
        Model results = remoteStoreClient.executeConstruct(BlazegraphContainer.CONSTRUCT_ALL_QUERY);

        checkExpectedFile(results);

        Model expectedResults = genericiseModel(readTurtleFromFile());
        Model actualResults = genericiseModel(results);

        Assertions.assertTrue(expectedResults.isIsomorphicWith(actualResults), () -> {
            writeTurtleToFile(results);
            return getMessage("has different statements.", expectedResults, actualResults);
        });
    }

    private void runUpdate(Dataset dataset) {
        DCATUpdateQuery dcatUpdateQuery = new DCATUpdateQuery();
        String query = dcatUpdateQuery.getUpdateQuery(dataset);

        remoteStoreClient.executeUpdate(query);
    }

    private void runDelete(Dataset dataset) {
        DCATUpdateQuery dcatUpdateQuery = new DCATUpdateQuery();
        String query = dcatUpdateQuery.getDeleteQuery(dataset);

        remoteStoreClient.executeUpdate(query);
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
