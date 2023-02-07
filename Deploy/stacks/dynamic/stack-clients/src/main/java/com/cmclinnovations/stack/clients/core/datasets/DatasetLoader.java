package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.blazegraph.Namespace;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.services.OntopService;
import com.cmclinnovations.stack.services.ServiceManager;
import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.fasterxml.jackson.databind.InjectableValues;
import com.fasterxml.jackson.databind.ObjectMapper;

public class DatasetLoader {

    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final Path configPath = Path.of("/inputs/config");

    private static final ServiceManager serviceManager = new ServiceManager(false);

    private DatasetLoader() {
    }

    public static void uploadInputDatasets() {
        DirectedAcyclicGraph<Dataset, DefaultEdge> graph = new DirectedAcyclicGraph<>(
                DefaultEdge.class);

        try (Stream<Path> files = Files.list(configPath)) {
            // Add Datasets to a DAG as vertices
            files.filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".json"))
                    .map(DatasetLoader::readInputDataset)
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

                loadData(stackSpecificDataset.get());
                graph.getDescendants(stackSpecificDataset.get()).forEach(DatasetLoader::loadData);
            } else {
                // Otherwise load all of the Datasets.
                graph.vertexSet().forEach(DatasetLoader::loadData);
            }
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file(s).", ex);
        }
    }

    private static void updateInjectableValues(Path configFile) {

        InjectableValues.Std iv = new InjectableValues.Std();

        iv.addValue(Namespace.PROPERTIES_FILE_DIRECTORY_KEY, configPath);

        iv.addValue(Dataset.NAME_KEY, FileUtils.getFileNameWithoutExtension(configFile));

        objectMapper.setInjectableValues(iv);
    }

    public static Dataset readInputDataset(Path configFile) {
        try {
            updateInjectableValues(configFile);

            return objectMapper.readValue(configFile.toFile(), Dataset.class);
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file '" + configFile + "'.", ex);
        }
    }

    public static void loadData(Dataset dataset) {
        if (!dataset.isSkip()) {
            List<DataSubset> dataSubsets = dataset.getDataSubsets();
            // Ensure PostGIS database exists, if specified
            if (dataSubsets.stream().anyMatch(DataSubset::usesPostGIS)) {
                PostGISClient.getInstance().createDatabase(dataset.getDatabase());
            }

            // Ensure Blazegraph namespace exists, if specified
            if (dataSubsets.stream().anyMatch(DataSubset::usesBlazegraph)) {
                BlazegraphClient.getInstance().createNamespace(dataset.getNamespace(),
                        dataset.getNamespaceProperties());
            }

            if (!dataset.getGeoserverStyles().isEmpty()
                    || dataSubsets.stream().anyMatch(DataSubset::usesGeoServer)) {
                GeoServerClient geoServerClient = GeoServerClient.getInstance();
                String workspaceName = dataset.getWorkspaceName();
                // Ensure GeoServer workspace exists
                geoServerClient.createWorkspace(workspaceName);
                // Upload styles to GeoServer
                dataset.getGeoserverStyles().forEach(style -> geoServerClient.loadStyle(style,
                        workspaceName));
            }

            dataSubsets.forEach(subset -> subset.load(dataset));

            List<String> ontopMappings = dataset.getOntopMappings();
            if (!ontopMappings.isEmpty()) {

                String newOntopServiceName = EndpointNames.ONTOP + "-" + dataset.getName();

                ServiceConfig newOntopServiceConfig = serviceManager.duplicateServiceConfig(EndpointNames.ONTOP,
                        newOntopServiceName);

                newOntopServiceConfig.setEnvironmentVariable(OntopService.ONTOP_DB_NAME, dataset.getDatabase());
                newOntopServiceConfig.getEndpoints()
                        .replaceAll((endpointName, connection) -> new Connection(
                                connection.getUrl(),
                                connection.getUri(),
                                URI.create(connection.getExternalPath().toString()
                                        .replace(EndpointNames.ONTOP, newOntopServiceName))));

                serviceManager.initialiseService(StackClient.getStackName(), newOntopServiceName);

                OntopClient ontopClient = OntopClient.getInstance(newOntopServiceName);
                Path directory = dataset.getDirectory();
                ontopMappings.forEach(mapping -> ontopClient.updateOBDA(directory.resolve(mapping)));
            }
        }
    }

}
