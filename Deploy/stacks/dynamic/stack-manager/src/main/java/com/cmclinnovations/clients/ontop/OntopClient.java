package com.cmclinnovations.clients.ontop;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import com.cmclinnovations.clients.docker.ContainerClient;

public class OntopClient extends ContainerClient {

    public static final String ONTOP_MAPPING_FILE = "ONTOP_MAPPING_FILE";

    public void updateOBDA(Path newMappingFilePath) {
        String containerId = getDockerClient().getContainerId("ontop");
        Path ontopMappingFilePath = getDockerClient().getEnvironmentVariable(containerId, ONTOP_MAPPING_FILE)
                .map(Path::of)
                .orElseThrow(() -> new RuntimeException("Environment variable '" + ONTOP_MAPPING_FILE
                        + " not set through Docker for '" + "ontop" + "' container."));
        Path localTempOntopMappingFilePath = null;
        boolean exceptionThrown = false;
        try {
            SQLPPMappingImplementation mapping = new SQLPPMappingImplementation();

            localTempOntopMappingFilePath = SQLPPMappingImplementation.createTempOBDAFile(ontopMappingFilePath);

            if (getDockerClient().fileExists(containerId, ontopMappingFilePath.toString())) {
                if (null == newMappingFilePath) {
                    // A mapping file already exists and no new one has been passed to be added.
                    return;
                }
                try (OutputStream outputStream = Files.newOutputStream(localTempOntopMappingFilePath)) {
                    outputStream.write(getDockerClient().retrieveFile(containerId, ontopMappingFilePath.toString()));
                    mapping.addMappings(localTempOntopMappingFilePath);
                }
            }

            if (null != newMappingFilePath) {
                mapping.addMappings(newMappingFilePath);
            }
            localTempOntopMappingFilePath = SQLPPMappingImplementation.createTempOBDAFile(ontopMappingFilePath);
            mapping.serialize(localTempOntopMappingFilePath);

            try {
                Map<String, byte[]> files = Map.of(ontopMappingFilePath.getFileName().toString(),
                        Files.readAllBytes(localTempOntopMappingFilePath));
                getDockerClient().sendFiles(containerId, files, ontopMappingFilePath.getParent().toString());
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        } catch (IOException ex) {
            exceptionThrown = true;
            throw new RuntimeException(
                    "Failed to write out combined Ontop mapping file '" + ontopMappingFilePath + "'.", ex);
        } catch (Throwable ex) {
            exceptionThrown = true;
            throw ex;
        } finally {
            if (null != localTempOntopMappingFilePath && Files.exists(localTempOntopMappingFilePath)) {
                try {
                    Files.delete(localTempOntopMappingFilePath);
                } catch (Exception ex2) {
                    if (exceptionThrown) {
                        // Don't worry about this exception as any previously thrown exception is more
                        // important.
                    } else {
                        throw new RuntimeException(ex2);
                    }
                }
            }
        }
    }
}
