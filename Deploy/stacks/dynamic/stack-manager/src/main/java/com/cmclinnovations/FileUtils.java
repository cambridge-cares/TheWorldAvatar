package com.cmclinnovations;

import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

public final class FileUtils {

    private FileUtils() {
    }

    public static void ensureScriptsExecutable(Path dir) throws IOException {
        try (Stream<Path> dirContent = Files.list(dir)) {
            dirContent.filter(path -> Files.isRegularFile(path) && !Files.isExecutable(path))
                    .forEach(file -> file.toFile().setExecutable(true, false));
        }
    }

    public static String sanitiseFilename(Path filepath) {
        // Extract the file name and replace most non-alphanumeric characters with "_"
        return filepath.getFileName().toString().replaceAll("[^\\w.]", "_");
    }

    private static String removeExtension(String filename) {
        int index = filename.lastIndexOf(".");
        if (index > 0) {
            return filename.substring(0, index);
        } else {
            // If "index" is "-1" (there is no ".") or 0 (it is the first character) then
            // just return the whole filename
            return filename;
        }
    }

    public static String getFileNameWithoutExtension(Path path) {
        String filename = path.getFileName().toString();
        return removeExtension(filename);
    }

    public static String getFileNameWithoutExtension(URL url) {
        String[] pathComponents = url.getPath().split("/");
        return removeExtension(pathComponents[pathComponents.length - 1]);
    }

    public static boolean filterOnExtension(Path path, String extension) {
        return path.getFileName().toString().endsWith(extension);
    }
}
