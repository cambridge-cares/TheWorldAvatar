package com.cmclinnovations.stack.clients.ontop;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.jena.rdf.model.Model;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ConstructQuery;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.datasets.CopyDatasetQuery;
import com.cmclinnovations.stack.clients.utils.TempFile;
import com.moandjiezana.toml.Toml;
import com.moandjiezana.toml.TomlWriter;

public class OntopClient extends ClientWithEndpoint<OntopEndpointConfig> {

    protected static final Logger LOGGER = LoggerFactory.getLogger(OntopClient.class);

    public static final String ONTOP_MAPPING_FILE = "ONTOP_MAPPING_FILE";
    public static final String ONTOP_ONTOLOGY_FILE = "ONTOP_ONTOLOGY_FILE";
    public static final String ONTOP_SPARQL_RULES_FILE = "ONTOP_SPARQL_RULES_FILE";

    private static Map<String, OntopClient> instances = new HashMap<>();

    public static OntopClient getInstance() {
        return getInstance(EndpointNames.ONTOP);
    }

    public static OntopClient getInstance(String containerName) {
        return instances.computeIfAbsent(containerName, OntopClient::new);
    }

    private OntopClient(String containerName) {
        super(containerName, OntopEndpointConfig.class);
    }

    public void uploadOntology(String catalogNamespace, List<String> ontologyDatasets) {
        ConstructQuery query = CopyDatasetQuery.getConstructQuery(ontologyDatasets);

        Model model = BlazegraphClient.getInstance().getRemoteStoreClient(catalogNamespace)
                .executeConstruct(query.getQueryString());

        writeTurtleToFile(model);
    }

    public void updateOBDA(Path newMappingFilePath) {
        String containerId = getContainerId(getContainerName());
        Path ontopMappingFilePath = getEnvironmentVariable(containerId, ONTOP_MAPPING_FILE)
                .map(Path::of)
                .orElseThrow(() -> new RuntimeException("Environment variable '" + ONTOP_MAPPING_FILE
                        + " not set through Docker for '" + "ontop" + "' container."));

        try {
            SQLPPMappingImplementation mapping = new SQLPPMappingImplementation();

            if (fileExists(containerId, ontopMappingFilePath.toString())) {

                if (null == newMappingFilePath) {
                    // A mapping file already exists and no new one has been passed to be added.
                    return;
                }
                try (TempFile localTempOntopMappingFilePath = SQLPPMappingImplementation
                        .createTempOBDAFile(ontopMappingFilePath);
                        OutputStream outputStream = Files.newOutputStream(localTempOntopMappingFilePath.getPath())) {
                    outputStream.write(retrieveFile(containerId, ontopMappingFilePath.toString()));
                    mapping.addMappings(localTempOntopMappingFilePath.getPath());
                }
            }

            if (null != newMappingFilePath) {
                mapping.addMappings(newMappingFilePath);
            }
            try (TempFile localTempOntopMappingFilePath = SQLPPMappingImplementation
                    .createTempOBDAFile(ontopMappingFilePath)) {
                mapping.serialize(localTempOntopMappingFilePath.getPath());

                sendFilesContent(containerId,
                        Map.of(ontopMappingFilePath.getFileName().toString(),
                                Files.readAllBytes(localTempOntopMappingFilePath.getPath())),
                        ontopMappingFilePath.getParent().toString());
            }
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Failed to write out combined Ontop mapping file '" + ontopMappingFilePath + "'.", ex);
        }
    }

    public void uploadRules(List<Path> rules) {
        String containerId = getContainerId(getContainerName());
        Path sparqlRulesFilePath = getEnvironmentVariable(containerId, ONTOP_SPARQL_RULES_FILE)
                .map(Path::of)
                .orElseThrow(() -> new RuntimeException("Environment variable '" + ONTOP_SPARQL_RULES_FILE
                        + " not set through Docker for '" + "ontop" + "' container."));
        SparqlRulesFile sparqlRules = new SparqlRulesFile();

        rules.forEach(file -> {
            Toml tomlRules = new Toml().read(file.toFile());
            sparqlRules.addRules(tomlRules.to(SparqlRulesFile.class));
        });

        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            TomlWriter tomlWriter = new TomlWriter();
            tomlWriter.write(sparqlRules, outputStream);
            sendFilesContent(containerId,
                    Map.of(sparqlRulesFilePath.getFileName().toString(), outputStream.toByteArray()),
                    sparqlRulesFilePath.getParent().toString());
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Failed to write SPARQL Rules file.", ex);
        }
    }

    private void writeTurtleToFile(Model model) {
        String containerId = getContainerId(getContainerName());
        Path ontopOntologyFilePath = getEnvironmentVariable(containerId, ONTOP_ONTOLOGY_FILE)
                .map(Path::of)
                .orElseThrow(() -> new RuntimeException("Environment variable '" + ONTOP_ONTOLOGY_FILE
                        + " not set through Docker for '" + "ontop" + "' container."));

        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            model.write(outputStream, "TURTLE");
            sendFilesContent(containerId,
                    Map.of(ontopOntologyFilePath.getFileName().toString(), outputStream.toByteArray()),
                    ontopOntologyFilePath.getParent().toString());
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

    }
}
