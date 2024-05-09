package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
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

        try (Stream<Path> files = Files.list(configPath)) {
            // Find all available datasets
            Map<String, Dataset> allDatasets = files.filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".json"))
                    .map(DatasetLoader::readInputDataset)
                    .collect(Collectors.toMap(Dataset::getName, dataset -> dataset));

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

            graph.iterator().forEachRemaining(DatasetLoader::loadData);

        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file(s).", ex);
        }
    }

    // This ensures that datasets are loaded such that children are loaded before
    // their parents so that things that can refer to multiple datasubsets, like
    // OBDA mappings, can be specified in the parent config and are loaded in after
    // all of the datasubsets have been loaded in.
    private static void addToGraph(Map<String, Dataset> allDatasets, DirectedAcyclicGraph<Dataset, DefaultEdge> graph,
            Dataset current) {
        if (graph.addVertex(current)) {
            current.getExternalDatasets().forEach(datasetName -> {
                Dataset child = allDatasets.get(datasetName);
                if (null == child) {
                    throw new RuntimeException("Failed to find external dataset '"
                            + datasetName + "' referenced in dataset '"
                            + current.getName() + "'.");
                }
                addToGraph(allDatasets, graph, child);
                graph.addEdge(child, current);
            });
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
        Path directory = dataset.getDirectory();

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

            if (dataset.getStaticGeoServerData() != null) {
                GeoServerClient geoServerClient = GeoServerClient.getInstance();
                geoServerClient.loadIcons(directory, dataset.getStaticGeoServerData().getIconsDir());
                geoServerClient.loadOtherFiles(directory, dataset.getStaticGeoServerData().getOtherFiles());
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
                ontopMappings.forEach(mapping -> ontopClient.updateOBDA(directory.resolve(mapping)));

                if(PostGISClient.DEFAULT_DATABASE_NAME.equals(dataset.getDatabase())){
                    OntopClient defaultOntopClient = OntopClient.getInstance();
                    dataset.getOntopMappings().forEach(mapping -> defaultOntopClient.updateOBDA(directory.resolve(mapping)));
                }
            }
        }
    }

}
