package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.apache.jena.rdf.model.Statement;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;

import java.nio.file.Path;
import java.util.*;

/**
 * The main class to parse and write the converted OntoBIM instances to output file in the right format.
 *
 * @author qhouyee
 */
public class TTLWriter {
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private final Map<String, String> nsMap;
    private final Map<String, String> inverseNsMap;

    /**
     * Standard constructor to set up mappings.
     *
     * @param nsMapping A map object mapping the prefix to their namespaces.
     */
    public TTLWriter(Map<String, String> nsMapping) {
        this.nsMap = nsMapping;
        this.inverseNsMap = this.inverseNamespaceMappings();
    }

    /**
     * Inverse the namespace mappings to namespaces(value): prefix(key) now for ease of parsing the triples later.
     *
     * @return A map with the key and value inversed from nsMapping.
     */
    private Map<String, String> inverseNamespaceMappings() {
        // Create inverse mappings so that it is easier to retrieve prefix from URI
        Map<String, String> inverseMap = new HashMap<>();
        for (var entry : this.nsMap.entrySet()) {
            // Inverse the mapping's keys and values
            inverseMap.put(entry.getValue(), entry.getKey());
        }
        return inverseMap;
    }

    /**
     * Write all statements of one model to the specified TTL file.
     *
     * @param statementSet A set of all the statements for writing.
     * @param ttlFile      A path to the output TTL file.
     * @param classMapping A mapping to relate data IRI of generic elements to their ontoBIM class.
     */
    public void writeTTL(LinkedHashSet<Statement> statementSet, Path ttlFile, Map<String, String> classMapping) {
        LOGGER.info("Writing to TTL file at: " + ttlFile + "...");
        Path outputFile = IOHelper.createTempFile();
        this.writeNamespaces(outputFile);
        // Create an additional intermediate file to store, process, and format statements without messing up the namespaces
        Path tempFile = this.writeIntermediateTempFile(statementSet);
        this.formatContent(tempFile, outputFile, classMapping);
        IOHelper.replaceTargetFileWithSource(outputFile, ttlFile);
        LOGGER.info("OntoBIM instances have been successfully written to " + ttlFile);
    }

    /**
     * This method is an alternate approach to the writeTTL(...) method for larger, more complex IFC files, which could trigger
     * heap overflow and break the program. It is intended to be used in conjunction with the writeIntermediateTempFile(...) method.
     * It consolidates and sorts the several temporary files generated to store the intermediate statements, into the specified TTL file.
     *
     * @param tempFilePaths A list of all the temporary files to merge.
     * @param ttlFile       A path to the output TTL file.
     * @param classMapping  A mapping to relate data IRI of generic elements to their ontoBIM class.
     */
    public void writeTTLWithIntermediateFiles(List<Path> tempFilePaths, Path ttlFile, Map<String, String> classMapping) {
        LOGGER.info("Writing to TTL file at: " + ttlFile + "...");
        Path outputFile = IOHelper.createTempFile();
        this.writeNamespaces(outputFile);
        this.mergeAndFormatIntermediateFiles(tempFilePaths, outputFile, classMapping);
        IOHelper.replaceTargetFileWithSource(outputFile, ttlFile);
        LOGGER.info("OntoBIM instances have been successfully written to " + ttlFile);
    }

    /**
     * Format the namespaces as a valid TTL format and write to a specified file.
     *
     * @param filePath File path to the specified file.
     */
    private void writeNamespaces(Path filePath) {
        // Parse the namespace map into the required format
        StringBuilder namespacePrefix = new StringBuilder();
        for (var entry : this.nsMap.entrySet()) {
            if (!entry.getKey().equals("owl") && !entry.getKey().equals("express") && !entry.getKey().equals("list")){
                // Format as @prefix prefix: <URI> .
                namespacePrefix.append("@prefix ").append(entry.getKey()).append(": <").append(entry.getValue()).append("> . \n");
            }
        }
        IOHelper.writeLinesToFile(filePath, namespacePrefix.toString());
        LOGGER.info("Namespaces prefixes have been appended.");
    }

    /**
     * Standard method to write triples to an intermediate temporary file. This method is intended to store statements
     * at your specified breakpoint to the local disk to prevent heap overflow for larger models.
     *
     * @param statementSet A set of all the statements for writing.
     * @return file path of the temporary file created.
     */
    public Path writeIntermediateTempFile(LinkedHashSet<Statement> statementSet) {
        LOGGER.info("Writing statements to a temporary file...");
        Path tempFile = IOHelper.createTempFile();
        IOHelper.writeTempTTLFile(tempFile, statementSet);
        LOGGER.info("Temporary file have been successfully generated.");
        return tempFile;
    }

    /**
     * Merges all the intermediate files into one consolidated output file, and then delete them once merged.
     *
     * @param tempFilePaths A list of all the temporary files to merge.
     * @param outputFile    A path to the output file.
     * @param classMapping  A mapping to relate data IRI of generic elements to their ontoBIM class.
     */
    private void mergeAndFormatIntermediateFiles(List<Path> tempFilePaths, Path outputFile, Map<String, String> classMapping) {
        for (Path tempFile : tempFilePaths) {
            formatContent(tempFile, outputFile, classMapping);
        }
        LOGGER.info("All intermediate files have been merged and deleted successfully.");
    }

    /**
     * Reads and format a source file's triples into a concise and readable TTL format.
     * Content is transferred to the target file and source file will be deleted.
     *
     * @param source       File path to the source file.
     * @param target       File path to the target file.
     * @param classMapping A mapping to relate data IRI of generic elements to their ontoBIM class.
     */
    private void formatContent(Path source, Path target, Map<String, String> classMapping) {
        List<String> contents = IOHelper.readFile(source);
        // Sort and group these contents into TTL triple formats
        LOGGER.info("Formatting of triples in progress...");
        // If there are contents
        if (!contents.isEmpty()) {
            StringBuilder sbuilder = FileContentSorter.groupTriples(contents, classMapping);
            // Transfer the formatted content to the consolidated file
            IOHelper.writeLinesToFile(target, sbuilder.toString());
        }
        // Delete the file once transferred
        IOHelper.deleteTempFile(source);
        LOGGER.info("Triples have been formatted.");
    }
}
