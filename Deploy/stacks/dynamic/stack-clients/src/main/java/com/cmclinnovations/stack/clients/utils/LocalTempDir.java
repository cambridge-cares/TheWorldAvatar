package com.cmclinnovations.stack.clients.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.commons.io.FileUtils;

import com.cmclinnovations.stack.clients.core.StackClient;

public class LocalTempDir extends TempDir {

    public LocalTempDir() throws IOException {
        super(Files.createTempDirectory(Path.of(StackClient.SCRATCH_DIR), "tmp"));
    }

    @Override
    public void close() throws RuntimeException {
        Path path = getPath();
        if (Files.isDirectory(path)) {
            try {
                FileUtils.deleteDirectory(path.toFile());
            } catch (IOException ex) {
                throw new RuntimeException("Failed to delete temporary directory '" + path + "'", ex);
            }
        }
    }

    @Override
    public void copyFrom(Path sourcePath) {
        Path targetDir = getPath();
        if (Files.isDirectory(sourcePath)) {
            try {
                FileUtils.copyDirectory(sourcePath.toFile(), targetDir.toFile());
            } catch (IOException ex) {
                throw new RuntimeException("Failed to copy directory '" + sourcePath + "' into '" + targetDir + "'.",
                        ex);
            }
        } else if (Files.isRegularFile(sourcePath)) {
            try {
                Files.copy(sourcePath, targetDir.resolve(sourcePath.getFileName()));
            } catch (IOException ex) {
                throw new RuntimeException("Failed to copy file '" + sourcePath + "' into '" + targetDir + "'.", ex);
            }
        } else {
            throw new RuntimeException("Couldn't copy '" + sourcePath + "' into '" + targetDir
                    + "' as the source was niether a file nor a directory.");
        }
    }
}
