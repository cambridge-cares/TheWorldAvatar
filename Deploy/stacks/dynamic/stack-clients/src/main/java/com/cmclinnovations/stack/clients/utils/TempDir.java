package com.cmclinnovations.stack.clients.utils;

import java.nio.file.Path;

public interface TempDir extends TempPath {

    void copyFrom(Path sourceDir);

}
