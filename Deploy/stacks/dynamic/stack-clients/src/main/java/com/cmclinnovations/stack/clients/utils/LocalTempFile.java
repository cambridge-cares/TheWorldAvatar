package com.cmclinnovations.stack.clients.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class LocalTempFile extends LocalTempPath implements TempFile {

    public LocalTempFile(Path path) {
        super(path);
    }

    @Override
    public void close() throws RuntimeException {
        Path path = getPath();
        try {
            Files.deleteIfExists(path);
        } catch (IOException ex) {
            throw new RuntimeException("Failed to delete temp file '" + path + "'.", ex);
        }
    }

}
