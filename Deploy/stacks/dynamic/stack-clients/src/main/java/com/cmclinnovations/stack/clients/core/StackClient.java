package com.cmclinnovations.stack.clients.core;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;

import com.cmclinnovations.stack.clients.core.datasets.Dataset;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class StackClient {

    public static final String STACK_NAME_KEY = "STACK_NAME";
    public static final String STACK_NAME_LABEL = "com.docker.stack.namespace";
    public static final String PROJECT_NAME_LABEL = "com.docker.compose.project";
    public static final String SCRATCH_DIR = "/stack_scratch";
    public static final String GEOTIFFS_DIR = "/geotiffs";

    private static final ObjectMapper objectMapper = new ObjectMapper();

    private static final String stackName;

    private static final Map<String, String> stackNameLabelMap;

    private static boolean inStack = true;

    static {
        String envVarStackName = System.getenv(StackClient.STACK_NAME_KEY);
        stackName = (null != envVarStackName) ? envVarStackName : "Test_Stack";

        stackNameLabelMap = Map.of(STACK_NAME_LABEL, stackName, PROJECT_NAME_LABEL, stackName);
    }

    private StackClient() {
    }

    public static String getStackName() {
        return stackName;
    }

    public static String prependStackName(String name) {
        return stackName + "_" + name;
    }

    public static Map<String, String> getStackNameLabelMap() {
        return stackNameLabelMap;
    }

    public static boolean isInStack() {
        return inStack;
    }

    public static void setInStack(boolean inStack) {
        StackClient.inStack = inStack;
    }

    public static void uploadInputDatasets() {
        DirectedAcyclicGraph<Dataset, DefaultEdge> graph = new DirectedAcyclicGraph<>(
                DefaultEdge.class);
        try (Stream<Path> files = Files.list(Path.of("/inputs/config"))) {
            // Add Datasets to a DAG as vertices
            files.filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".json"))
                    .map(StackClient::readInputDataset)
                    .forEach(graph::addVertex);

            // Check to see if there is a Dataset with the same name as the Stack.
            Optional<Dataset> stackSpecificDataset = graph.vertexSet().stream()
                    .filter(dataset -> dataset.getName().equals(StackClient.getStackName()))
                    .findAny();

            if (stackSpecificDataset.isPresent()) {
                // if so only load that one and its children.

                // Add an edge when one Dataset references another in its "externalDatasets"
                // node. Throw an exception if the referenced Dataset doesn't exist.
                graph.vertexSet().forEach(parentDataset -> parentDataset.getExternalDatasets()
                        .forEach(referencedDatasetName -> graph.vertexSet().stream()
                                .filter(dataset -> dataset.getName().equals(referencedDatasetName))
                                .findFirst().ifPresentOrElse(
                                        childDataset -> graph.addEdge(parentDataset, childDataset),
                                        () -> {
                                            throw new RuntimeException("Failed to find external dataset '"
                                                    + referencedDatasetName + "' referenced in dataset '"
                                                    + parentDataset.getName() + "'.");
                                        })));

                stackSpecificDataset.get().loadData();
                graph.getDescendants(stackSpecificDataset.get()).forEach(Dataset::loadData);
            } else {
                // Otherwise load all of the Datasets.
                graph.vertexSet().forEach(Dataset::loadData);
            }
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file(s).", ex);
        }
    }

    public static Dataset readInputDataset(Path configFile) {
        try {
            Dataset dataset = objectMapper.readValue(configFile.toFile(), Dataset.class);
            if (null == dataset.getName()) {
                dataset.setName(FileUtils.getFileNameWithoutExtension(configFile));
            }
            return dataset;
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file '" + configFile + "'.", ex);
        }
    }

}
