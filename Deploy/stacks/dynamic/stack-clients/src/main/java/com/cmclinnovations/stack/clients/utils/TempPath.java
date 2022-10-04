package com.cmclinnovations.stack.clients.utils;

import java.nio.file.Path;

public abstract class TempPath implements AutoCloseable {

    private final Path path;

    protected TempPath(Path path) {
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
    public abstract void close() throws RuntimeException;

}
