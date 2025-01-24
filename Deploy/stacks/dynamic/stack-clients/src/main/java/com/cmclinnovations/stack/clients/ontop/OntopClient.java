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
import com.cmclinnovations.stack.clients.utils.SparqlRulesFile;
import com.cmclinnovations.stack.clients.utils.TempFile;

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
        Path ontopMappingFilePath = getFilePath(containerId, ONTOP_MAPPING_FILE);

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

                sendFileContent(containerId, ontopMappingFilePath,
                        Files.readAllBytes(localTempOntopMappingFilePath.getPath()));
            }
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Failed to write out combined Ontop mapping file '" + ontopMappingFilePath + "'.", ex);
        }
    }

    public void uploadRules(List<Path> ruleFiles) {
        String containerId = getContainerId(getContainerName());
        Path sparqlRulesFilePath = getFilePath(containerId, ONTOP_SPARQL_RULES_FILE);
        SparqlRulesFile sparqlRules = new SparqlRulesFile(ruleFiles);

        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            sparqlRules.write(outputStream);
            sendFileContent(containerId, sparqlRulesFilePath, outputStream.toByteArray());
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Failed to write SPARQL Rules file.", ex);
        }
    }

    private void writeTurtleToFile(Model model) {
        String containerId = getContainerId(getContainerName());
        Path ontopOntologyFilePath = getFilePath(containerId, ONTOP_ONTOLOGY_FILE);

        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            model.write(outputStream, "TURTLE");
            sendFileContent(containerId, ontopOntologyFilePath, outputStream.toByteArray());
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

    }

    private Path getFilePath(String containerId, String filenameKey) {
        return getEnvironmentVariable(containerId, filenameKey)
                .map(Path::of)
                .orElseThrow(() -> new RuntimeException("Environment variable '" + filenameKey
                        + " not set through Docker for '" + getContainerName() + "' container."));
    }

}
