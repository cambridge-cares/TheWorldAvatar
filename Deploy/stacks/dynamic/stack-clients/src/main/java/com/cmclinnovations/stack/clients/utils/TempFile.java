package com.cmclinnovations.stack.clients.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFile implements AutoCloseable {

    private final Path path;

    public TempFile(Path path) {
        this.path = path;
    }

    public Path getPath() {
        return path;
    }

    @Override
    public String toString() {
        return path.toString();
    }

    @Override
    public void close() throws IOException {
        Files.deleteIfExists(path);
    }
}
