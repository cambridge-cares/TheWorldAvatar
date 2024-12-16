package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class DatasetReaderTest {
    private static final Path configPath = Assertions
            .assertDoesNotThrow(() -> Path.of(DatasetReaderTest.class.getResource("datasetReader").toURI()));

    @Test
    void testGetAllDatasets() {
        List<Dataset> allDatasets = DatasetReader.getAllDatasets(configPath);

        List<String> expectedDatasetNames = List.of("explicitlyNamed", "implicitlyNamed", "nonTrivialExternal",
                "trivialExternal", "ontology", "trivialWithOntology", "nonTrivialWithOntology");
        List<String> actualDatasetNames = allDatasets.stream().map(Dataset::getName).collect(Collectors.toList());

        Assertions.assertTrue(CollectionUtils.isEqualCollection(expectedDatasetNames, actualDatasetNames));
    }

    private static Stream<Arguments> getArgsForGetStackSpecificDatasetsTests() {
        return Stream.of(
                Arguments.of("explicitlyNamed", List.of("explicitlyNamed")),
                Arguments.of("implicitlyNamed", List.of("implicitlyNamed")),
                Arguments.of("trivialExternal", List.of("implicitlyNamed", "trivialExternal")),
                Arguments.of("nonTrivialExternal",
                        List.of("implicitlyNamed", "trivialExternal", "explicitlyNamed", "nonTrivialExternal")),
                Arguments.of("trivialWithOntology",
                        List.of("ontology", "trivialWithOntology")),
                Arguments.of("nonTrivialWithOntology",
                        List.of("implicitlyNamed", "trivialExternal", "explicitlyNamed", "nonTrivialExternal",
                                "ontology", "nonTrivialWithOntology")));
    }

    @ParameterizedTest
    @MethodSource("getArgsForGetStackSpecificDatasetsTests")
    void testGetStackSpecificDatasets(String selectedDatasetName, List<String> expectedDatasetNames) {
        List<Dataset> allDatasets = DatasetReader.getAllDatasets(configPath);

        Stream<Dataset> selectedDatasets = DatasetReader.getStackSpecificDatasets(allDatasets, selectedDatasetName);
        List<String> actualDatasetNames = selectedDatasets.map(Dataset::getName).collect(Collectors.toList());

        Assertions.assertEquals(expectedDatasetNames, actualDatasetNames);
    }

    private static Stream<Arguments> getArgsForReadInputDatasetTests() {
        return Stream.of(
                Arguments.of("explicitlyNamed", List.of(), List.of(), List.of()),
                Arguments.of("implicitlyNamed", List.of(), List.of(), List.of()),
                Arguments.of("trivialExternal", List.of("implicitlyNamed"), List.of(), List.of("implicitlyNamed")),
                Arguments.of("nonTrivialExternal", List.of("trivialExternal", "explicitlyNamed"), List.of(),
                        List.of("trivialExternal", "explicitlyNamed")),
                Arguments.of("trivialWithOntology", List.of(), List.of("ontology"), List.of("ontology")),
                Arguments.of("nonTrivialWithOntology", List.of("nonTrivialExternal"), List.of("ontology"),
                        List.of("nonTrivialExternal", "ontology")));
    }

    @ParameterizedTest
    @MethodSource("getArgsForReadInputDatasetTests")
    void testReadInputDataset(String selectedDatasetName, List<String> expectedExternalDatasetNames,
            List<String> expectedOntologyDatasetNames, List<String> referencedDatasetNames) {

        Dataset dataset = DatasetReader.readDataset(configPath.resolve(selectedDatasetName + ".json"));

        Assertions.assertEquals(selectedDatasetName, dataset.getName());

        Assertions.assertEquals(expectedExternalDatasetNames, dataset.getExternalDatasetNames());

        Assertions.assertEquals(expectedOntologyDatasetNames, dataset.getOntologyDatasetNames());

        Assertions.assertEquals(referencedDatasetNames, dataset.getReferencedDatasetNames());
    }
}
