package uk.ac.cam.cares.jps.agent.isochroneagent;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class FileReader {



    public static InputStream getStream(String path) throws FileNotFoundException {
        return new FileInputStream(path);
    }


    /**
     * Read POI SPARQL files
     */
    public static Map readPOIsparql(Path POI_PATH) {
        try (Stream<Path> files = Files.list(POI_PATH)) {
            // Find all available SPARQL files
            Map<String, String> sparqlFiles = files
                    .filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".sql"))
                    .collect(Collectors.toMap(
                            path -> path.getFileName().toString(),
                            path -> {
                                try {
                                    return new String(Files.readAllBytes(path));
                                } catch (IOException e) {
                                    e.printStackTrace(); // Handle the exception as needed
                                    return null; // Or some default value for failed reads
                                }
                            }
                    ));

            // Process each SPARQL file
            sparqlFiles.forEach((fileName, filePath) -> {
                // Process the SPARQL file here
                // You can replace the following line with your processing logic
                System.out.println("Processing SPARQL file: " + filePath);
            });
            return sparqlFiles;
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read files from the directory.", ex);
        }
    }

    /**
     * Read EdgesTableSQL
     */
    public static Map readEdgesTableSQL(Path EDGESTABLESQL_PATH) {
        try (Stream<Path> files = Files.list(EDGESTABLESQL_PATH)) {
            // Find all available EdgesTableSQL files
            Map<String, String> edgesTableSQLFiles = files
                    .filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".sql"))
                    .collect(Collectors.toMap(
                            path -> path.getFileName().toString(),
                            path -> {
                                try {
                                    return new String(Files.readAllBytes(path));
                                } catch (IOException e) {
                                    e.printStackTrace(); // Handle the exception as needed
                                    return null; // Or some default value for failed reads
                                }
                            }
                    ));

            // Process each EdgesTableSQL file
            edgesTableSQLFiles.forEach((fileName, filePath) -> {
                // Process the EdgesTableSQL file here
                // You can replace the following line with your processing logic
                System.out.println("Processing EdgesTableSQL file: " + filePath);
            });

            return edgesTableSQLFiles;
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read files from the directory.", ex);
        }
    }
}
