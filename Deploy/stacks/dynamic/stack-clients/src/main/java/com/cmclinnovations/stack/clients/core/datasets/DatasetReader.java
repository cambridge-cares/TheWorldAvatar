package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;

import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.databind.InjectableValues;
import com.fasterxml.jackson.databind.ObjectMapper;

public class DatasetReader {

    private DatasetReader() {
    }

    private static final ObjectMapper objectMapper = JsonHelper.getMapper();
    private static final Path configPath = Path.of("/inputs/config");

    public static Stream<Dataset> getStackSpecificDatasets(Map<String, Dataset> allDatasets) {
        Collection<Dataset> selectedDatasets;

        // Check to see if there is a Dataset with the same name as the Stack.
        Dataset stackSpecificDataset = allDatasets.get(StackClient.getStackName());
        if (null != stackSpecificDataset) {
            // If so only load that one and its children
            selectedDatasets = List.of(stackSpecificDataset);
        } else {
            // Otherwise load all of them
            selectedDatasets = allDatasets.values();
        }
        DirectedAcyclicGraph<Dataset, DefaultEdge> graph = new DirectedAcyclicGraph<>(
                DefaultEdge.class);

        // Add an edge when one Dataset references another in its "externalDatasets"
        // node. Throw an exception if the referenced Dataset doesn't exist.
        selectedDatasets.forEach(dataset -> addToGraph(allDatasets, graph, dataset));

        return StreamSupport.stream(graph.spliterator(), false);
    }

    public static Map<String, Dataset> getAllDatasets() {
        Map<String, Dataset> allDatasets;

        try (Stream<Path> files = Files.list(configPath)) {
            // Find all available datasets
            allDatasets = files.filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".json"))
                    .map(DatasetReader::readInputDataset)
                    .collect(Collectors.toMap(Dataset::getName, dataset -> dataset));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file(s).", ex);
        }
        return allDatasets;
    }

    public static Dataset readInputDataset(Path configFile) {
        try {
            updateInjectableValues(configFile);

            return objectMapper.readValue(configFile.toFile(), Dataset.class);
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file '" + configFile + "'.", ex);
        }
    }

    private static void updateInjectableValues(Path configFile) {

        InjectableValues.Std iv = new InjectableValues.Std();

        iv.addValue(Namespace.PROPERTIES_FILE_DIRECTORY_KEY, configPath);

        iv.addValue(Dataset.NAME_KEY, FileUtils.getFileNameWithoutExtension(configFile));

        objectMapper.setInjectableValues(iv);
    }

    // This ensures that datasets are loaded such that children are loaded before
    // their parents so that things that can refer to multiple datasubsets, like
    // OBDA mappings, can be specified in the parent config and are loaded in after
    // all of the datasubsets have been loaded in.
    private static void addToGraph(Map<String, Dataset> allDatasets, DirectedAcyclicGraph<Dataset, DefaultEdge> graph,
            Dataset current) {
        if (graph.addVertex(current)) {
            current.getExternalDatasetNames().forEach(datasetName -> {
                Dataset child = allDatasets.get(datasetName);
                if (null == child) {
                    throw new RuntimeException("Failed to find external dataset '"
                            + datasetName + "' referenced in dataset '"
                            + current.getName() + "'.");
                }
                addToGraph(allDatasets, graph, child);
                graph.addEdge(child, current);
                current.addExternalDataset(child);
            });
        }
    }

}
