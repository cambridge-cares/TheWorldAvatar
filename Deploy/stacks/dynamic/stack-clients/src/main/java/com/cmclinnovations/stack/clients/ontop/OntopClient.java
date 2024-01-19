package com.cmclinnovations.stack.clients.ontop;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.utils.TempFile;

public class OntopClient extends ContainerClient implements ClientWithEndpoint {

    public static final String ONTOP_MAPPING_FILE = "ONTOP_MAPPING_FILE";

    private static Map<String, OntopClient> instances = new HashMap<>();

    private OntopEndpointConfig ontopEndpoint;

    private final String containerName;

    public static OntopClient getInstance() {
        return getInstance(EndpointNames.ONTOP);
    }

    public static OntopClient getInstance(String containerName) {
        return instances.computeIfAbsent(containerName, OntopClient::new);
    }

    private OntopClient(String containerName) {
        this.containerName = containerName;
    }

    @Override
    public OntopEndpointConfig getEndpoint() {
        if (null == ontopEndpoint) {
            ontopEndpoint = readEndpointConfig(containerName, OntopEndpointConfig.class);
        }
        return ontopEndpoint;
    }

    public void updateOBDA(Path newMappingFilePath) {
        String containerId = getContainerId(containerName);
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
}
