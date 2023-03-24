package uk.ac.cam.cares.jps.agent.ifc2ontobim.utils;

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
import java.util.ArrayList;
import java.util.LinkedHashSet;
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
     * Standard method to write triples to an intermediate temporary file. This method is intended to store statements
     * at your specified breakpoint to the local disk to prevent heap overflow for larger models.
     *
     * @param statementSet A set of all the statements for writing.
     * @return file path of the temporary file created.
     */
    public static Path writeIntermediateTempFile(LinkedHashSet<Statement> statementSet) {
        LOGGER.info("Writing statements to a temporary file...");
        Path tempFile = createTempFile();
        writeTempTTLFile(tempFile, statementSet);
        LOGGER.info("Temporary file have been successfully generated.");
        return tempFile;
    }

    /**
     * Write a string to a specified file.
     *
     * @param filePath   File path to the specified file.
     * @param statements Statements that should be written.
     */
    private static void writeTempTTLFile(Path filePath, LinkedHashSet<Statement> statements) {
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
