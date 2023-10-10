package com.cmclinnovations.mods.modssimpleagent.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;

public final class FileUtils {

    private FileUtils() {
    }

    /**
     * Copies a whole directory tree
     * 
     * @param sourceDirectory      base directory to be copied from
     * @param destinationDirectory location where directory is to be copied to
     */
    public static void copyDirectory(Path sourceDirectory, Path destinationDirectory) throws FileGenerationException {
        if (!Files.exists(destinationDirectory)) {
            try {
                Files.createDirectories(destinationDirectory);
            } catch (IOException ex) {
                throw new FileGenerationException(
                        "Failed to create destination directory '" + destinationDirectory.toAbsolutePath() + "'.", ex);
            }
        }

        try (Stream<Path> stream = Files.walk(sourceDirectory)) {

            stream.filter(Files::isRegularFile).forEach(source -> {
                Path destination = destinationDirectory
                        .resolve(source.toString().substring(sourceDirectory.toString().length() + 1));
                try {
                    Files.copy(source, destination);
                } catch (IOException ex) {
                    throw new ResponseStatusException(
                            HttpStatus.NO_CONTENT,
                            "Failed to copy '"
                                    + destinationDirectory + "` to `" + sourceDirectory + "'.",
                            ex);
                }
            });
        } catch (IOException ex) {
            throw new FileGenerationException("Failed to walk source directory '" + sourceDirectory + "'.", ex);
        }
    }
}
