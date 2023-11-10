package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.utils.LocalTempDir;
import com.cmclinnovations.stack.clients.utils.TempDir;

import uk.ac.cam.cares.jps.base.converter.TBoxGeneration;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class TBoxCSV extends DataSubset {

    @Override
    public boolean usesBlazegraph() {
        return !isSkip();
    }

    @Override
    void loadInternal(Dataset dataset) {

        TBoxGeneration tBoxGenerator = new TBoxGeneration();

        RemoteStoreClient remoteStoreClient = BlazegraphClient.getInstance()
                .getRemoteStoreClient(dataset.getNamespace());

        Path subdirectory = this.getSubdirectory();
        if (null == subdirectory) {
            throw new RuntimeException("No 'subdirectory' specified - required for TBoxCSV data");
        }
        Path datasubsetDir = dataset.getDirectory().resolve(subdirectory);
        if (!Files.exists(datasubsetDir)) {
            throw new RuntimeException("Data subset directory '" + datasubsetDir + "' does not exist.");
        }

        // Create a temporary directory to contain the generated OWL files
        try (TempDir outputDir = new LocalTempDir();
                // List all of the CSV files in the datasubset's directory
                Stream<Path> csvFiles = Files.list(datasubsetDir)
                        .filter(path -> path.getFileName().toString().endsWith(".csv"))) {
            csvFiles.forEach(csvFile -> {
                // Calculate path for current OWL file
                Path owlFile = outputDir.getPath()
                        .resolve(csvFile.getFileName().toString().replace(".csv", ".owl"));

                // Generate OWL file from CSV file
                tBoxGenerator.generateTBox(
                        csvFile.toAbsolutePath().toString(),
                        owlFile.toAbsolutePath().toString());

                try {
                    // Upload OWL file to the triple store
                    remoteStoreClient.uploadFile(owlFile.toFile());
                } catch (Exception ex) {
                    throw new RuntimeException(
                            "Failed to upload ontology file '" + owlFile + "' to the endpoint at '" +
                                    BlazegraphClient.getInstance().getEndpoint().getUrl(dataset.getNamespace())
                                    + "'.",
                            ex);
                }
            });
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Failed to create temporary directory for OWL files generated for the '" + getName()
                            + "' data subset.",
                    ex);
        }
    }

}
