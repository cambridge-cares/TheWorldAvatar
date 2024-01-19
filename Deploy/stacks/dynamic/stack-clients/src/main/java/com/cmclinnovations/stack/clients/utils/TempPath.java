package com.cmclinnovations.stack.clients.utils;

import java.nio.file.Path;

public interface TempPath extends AutoCloseable {

    @Override
    void close() throws RuntimeException;

    Path getPath();

}
