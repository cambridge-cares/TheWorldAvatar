package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
                "trivialExternal");
        List<String> actualDatasetNames = allDatasets.stream().map(Dataset::getName).collect(Collectors.toList());

        Assertions.assertIterableEquals(expectedDatasetNames, actualDatasetNames);
    }

    private static Stream<Arguments> getArgsForGetStackSpecificDatasetsTests() {
        return Stream.of(
                Arguments.of("explicitlyNamed", List.of("explicitlyNamed")),
                Arguments.of("implicitlyNamed", List.of("implicitlyNamed")),
                Arguments.of("trivialExternal", List.of("implicitlyNamed", "trivialExternal")),
                Arguments.of("nonTrivialExternal",
                        List.of("implicitlyNamed", "trivialExternal", "explicitlyNamed", "nonTrivialExternal")));
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
                Arguments.of("explicitlyNamed", List.of()),
                Arguments.of("implicitlyNamed", List.of()),
                Arguments.of("trivialExternal", List.of("implicitlyNamed")),
                Arguments.of("nonTrivialExternal", List.of("trivialExternal", "explicitlyNamed")));
    }

    @ParameterizedTest
    @MethodSource("getArgsForReadInputDatasetTests")
    void testReadInputDataset(String selectedDatasetName, List<String> expectedExternalDatasetNames) {

        Dataset dataset = DatasetReader.readDataset(configPath.resolve(selectedDatasetName + ".json"));

        Assertions.assertEquals(selectedDatasetName, dataset.getName());

        Assertions.assertEquals(expectedExternalDatasetNames, dataset.getExternalDatasetNames());
    }
}
