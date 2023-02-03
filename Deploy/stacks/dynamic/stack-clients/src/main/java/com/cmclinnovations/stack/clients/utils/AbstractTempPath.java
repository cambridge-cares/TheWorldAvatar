package com.cmclinnovations.stack.clients.utils;

import java.nio.file.Path;

public abstract class AbstractTempPath implements TempPath {

    private final Path path;

    protected AbstractTempPath(Path path) {

        this.path = path;
    }

    @Override
    public Path getPath() {
        return path;
    }

    @Override
    public String toString() {
        return path.toString();
    }

}
