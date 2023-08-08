package com.cmclinnovations.stack.clients.utils;

import java.nio.file.Path;

public abstract class TempDir extends TempPath {

    protected TempDir(Path path) {
        super(path);
    }

    public abstract void copyFrom(Path sourceDir);
}
