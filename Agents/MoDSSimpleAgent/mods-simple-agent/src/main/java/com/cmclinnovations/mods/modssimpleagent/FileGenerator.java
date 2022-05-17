package com.cmclinnovations.mods.modssimpleagent;

import java.io.IOException;
import java.nio.file.Path;

public interface FileGenerator {

    public static class FileGenerationException extends IOException {

        public FileGenerationException(String message) {
            super(message);
        }

        public FileGenerationException(String message, Throwable cause) {
            super(message, cause);
        }

    }

    void marshal(Path filePath) throws FileGenerationException;
}
