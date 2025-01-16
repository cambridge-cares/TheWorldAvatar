package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;

import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.databind.InjectableValues;
import com.fasterxml.jackson.databind.ObjectMapper;

public class DatasetReader {

    private DatasetReader() {
    }

    private static final ObjectMapper objectMapper = JsonHelper.getMapper();

    public static Stream<Dataset> getStackSpecificDatasets(List<Dataset> allDatasets, String selectedDatasetName) {
        Collection<Dataset> selectedDatasets;

        // Check to see if there is a Dataset with the same name as the Stack.
        Optional<Dataset> stackSpecificDataset = allDatasets.stream()
                .filter(dataset -> selectedDatasetName.equals(dataset.getName())).findAny();
        if (stackSpecificDataset.isPresent()) {
            // If so only load that one and its children
            selectedDatasets = List.of(stackSpecificDataset.get());
        } else {
            // Otherwise load all of them
            selectedDatasets = allDatasets;
        }
        DirectedAcyclicGraph<Dataset, DefaultEdge> graph = new DirectedAcyclicGraph<>(
                DefaultEdge.class);

        // Add an edge when one Dataset references another in its "externalDatasets" or
        // "ontologyDatasets" node. Throw an exception if the referenced Dataset doesn't
        // exist.
        selectedDatasets.forEach(dataset -> addToGraph(allDatasets, graph, dataset));

        return StreamSupport.stream(graph.spliterator(), false);
    }

    public static List<Dataset> getAllDatasets(Path configPath) {
        List<Dataset> allDatasets;

        try (Stream<Path> files = Files.list(configPath)) {
            // Find all available datasets
            allDatasets = files.filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".json"))
                    .map(DatasetReader::readDataset)
                    .collect(Collectors.toList());
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file(s).", ex);
        }
        return allDatasets;
    }

    public static Dataset readDataset(Path configFile) {
        try {
            updateInjectableValues(configFile);

            return objectMapper.readValue(configFile.toFile(), Dataset.class);
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file '" + configFile + "'.", ex);
        }
    }

    private static void updateInjectableValues(Path configFile) {

        InjectableValues.Std iv = new InjectableValues.Std();

        iv.addValue(Namespace.PROPERTIES_FILE_DIRECTORY_KEY, configFile.getParent());

        iv.addValue(Dataset.NAME_KEY, FileUtils.getFileNameWithoutExtension(configFile));

        objectMapper.setInjectableValues(iv);
    }

    // This ensures that datasets are loaded such that children are loaded before
    // their parents so that things that can refer to multiple datasubsets, like
    // OBDA mappings, can be specified in the parent config and are loaded in after
    // all of the datasubsets have been loaded in.
    private static void addToGraph(List<Dataset> allDatasets, DirectedAcyclicGraph<Dataset, DefaultEdge> graph,
            Dataset current) {
        if (graph.addVertex(current)) {
            current.getReferencedDatasetNames().forEach(datasetName -> {
                Optional<Dataset> potentialChild = allDatasets.stream()
                        .filter(dataset -> dataset.getName().equals(datasetName))
                        .findAny();
                if (potentialChild.isEmpty()) {
                    throw new RuntimeException("Failed to find referenced dataset '"
                            + datasetName + "' referenced in dataset '"
                            + current.getName() + "'.");
                } else {
                    Dataset child = potentialChild.get();
                    addToGraph(allDatasets, graph, child);
                    graph.addEdge(child, current);
                    current.addReferencedDataset(child);
                }
            });
        }
    }

}
