package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * Provide helper methods to handle simple input and output operations.
 *
 * @author qhouyee
 */
public class IOHelper {
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);

    /**
     * Creates a temporary file in the local temp folder.
     *
     * @return file path of the temporary file created.
     */
    protected static Path createTempFile() {
        try {
            return Files.createTempFile(null, ".ttl");
        } catch (IOException e) {
            LOGGER.error("Temporary file cannot be created! See error message for more details: " + e);
            throw new JPSRuntimeException("Temporary file cannot be created! See error message for more details: " + e);
        }
    }

    /**
     * Reads a file and return its contents as a List of string lines.
     *
     * @param filePath File path to the specified file.
     * @return The list of content lines.
     */
    protected static List<String> readFile(Path filePath) {
        try {
            return Files.readAllLines(filePath);
        } catch (IOException e) {
            LOGGER.error(filePath + " cannot be accessed or opened!" + e);
            throw new JPSRuntimeException(filePath + " cannot be accessed or opened!" + e);
        }
    }

    /**
     * Write a string to a specified file.
     *
     * @param filePath File path to the specified file.
     * @param lines    String to be added to the file.
     */
    protected static void writeLinesToFile(Path filePath, String lines) {
        try {
            Files.write(filePath, lines.getBytes(), StandardOpenOption.APPEND);
        } catch (IOException e) {
            LOGGER.error(lines + " cannot be written to temporary file! " + e);
            throw new JPSRuntimeException(lines + " cannot be written to temporary file! " + e);
        }
    }

    /**
     * Write a string to a specified file.
     *
     * @param filePath   File path to the specified file.
     * @param statements Statements that should be written.
     */
    protected static void writeTempTTLFile(Path filePath, LinkedHashSet<Statement> statements) {
        try (OutputStream outputStream = new FileOutputStream(filePath.toString())) {
            // Add the statements into a new model
            Model model = ModelFactory.createDefaultModel();
            model.add(new ArrayList<>(statements));
            RDFDataMgr.write(outputStream, model, Lang.TTL);
        } catch (IOException e) {
            LOGGER.error("Statements cannot be written to temporary file! " + e);
            throw new JPSRuntimeException("Statements cannot be written to temporary file! " + e);
        }
    }

    /**
     * Replace the target file with source file.
     *
     * @param source File path to the source file.
     * @param target File path to the target file.
     */
    protected static void replaceTargetFileWithSource(Path source, Path target) {
        try {
            Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
            IOHelper.deleteTempFile(source);
        } catch (IOException e) {
            LOGGER.error("Source cannot be copied. Read error message for more details: " + e);
            throw new JPSRuntimeException("Source cannot be copied. Read error message for more details: " + e);
        }
    }

    /**
     * Deletes the temporary file if it exists in the specified path.
     *
     * @param filePath File path to the temporary file.
     */
    public static void deleteTempFile(Path filePath) {
        try {
            Files.deleteIfExists(filePath);
        } catch (IOException e) {
            LOGGER.error("Temporary file cannot be deleted! See error message for more details: " + e);
            throw new JPSRuntimeException("Temporary file cannot be deleted! See error message for more details: " + e);
        }
    }
}
