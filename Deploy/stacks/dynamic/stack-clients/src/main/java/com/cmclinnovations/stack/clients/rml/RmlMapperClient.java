package com.cmclinnovations.stack.clients.rml;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.utils.FileUtils;

import be.ugent.rml.Executor;
import be.ugent.rml.Utils;
import be.ugent.rml.records.RecordsFactory;
import be.ugent.rml.store.QuadStore;
import be.ugent.rml.store.QuadStoreFactory;
import be.ugent.rml.store.RDF4JStore;
import be.ugent.rml.term.NamedNode;

public class RmlMapperClient extends ClientWithEndpoint<RmlMapperEndpointConfig> {
  private static final Logger logger = LoggerFactory.getLogger(RmlMapperClient.class);

  private static final String CSV_FILE_EXTENSION = "csv";
  private static final String YML_FILE_EXTENSION = "yml";

  private static RmlMapperClient instance = null;

  public static RmlMapperClient getInstance() {
    if (null == instance) {
      instance = new RmlMapperClient();
    }
    return instance;
  }

  private RmlMapperClient() {
    super(EndpointNames.RML, RmlMapperEndpointConfig.class);
  }

  /**
   * Parses YARRRML files into RML mappings in the specified directory.
   * 
   * @param dirPath Target directory path.
   */
  public ByteArrayOutputStream parseYarrrmlToRml(Path dirPath) {
    logger.info("Checking and parsing YARRRML files...");
    this.validateDirContents(dirPath);
    return this.genRmlRules(dirPath);
  }

  /**
   * Parses the RML rules into RDF triples that will be uploaded at the target
   * namespace.
   * 
   * @param rmlRules  Input RML rules.
   * @param namespace Target namespace to upload the converted RDF triples.
   */
  public void parseRmlToRDF(InputStream rmlRules, String namespace) {
    logger.info("Reading the RML rules...");
    BlazegraphEndpointConfig blazegraphConfig = super.readEndpointConfig(EndpointNames.BLAZEGRAPH,
        BlazegraphEndpointConfig.class);
    String sparqlEndpoint = blazegraphConfig.getUrl(namespace);
    try {
      // TO DO
      // TO DO
      // RML Rules should be parsed and added with sources and targets
      // Source:
      // :source_000 a rml:LogicalSource;
      // rdfs:label "source label";
      // rml:source "source.csv";
      // rml:referenceFormulation ql:CSV.
      // :map_mappingname_000 a rr:TriplesMap; #to id;do not add
      // rml:logicalSource :source_000.
      // Target:
      // :target_000 a rmlt:LogicalTarget;
      // rdfs:label "sparql";
      // rmlt:serialization formats:Turtle;
      // rmlt:target :sd_000.
      // :sd_000 a sd:Service;
      // sd:supportedLanguage sd:SPARQL11Update;
      // sd:endpoint <SPARQL Endpoint>.
      // :s_000 a rr:SubjectMap; #to id;do not add
      // rml:logicalTarget :target_000.

      QuadStore rmlStore = QuadStoreFactory.read(rmlRules);

      // To parse the data into records eg each csv row is a record
      // As we are interested only in local file access, base path is crucial to
      // determine the absolute path from the root. Mapping path is not required for
      // this simple case
      RecordsFactory factory = new RecordsFactory("/", "/");

      // Output store serialises beyond nquads, but is not needed when uploading
      // directly to Blazegraph
      QuadStore outputStore = new RDF4JStore();

      // Function agent is not required - the library will default to their base
      // functions
      Executor executor = new Executor(rmlStore, factory, outputStore,
          Utils.getBaseDirectiveTurtleOrDefault(rmlRules, "https://theworldavatar.io/kg/"), null);
      QuadStore result = executor.execute(null).get(new NamedNode("rmlmapper://default.store"));

      BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out));
      result.write(out, "turtle");
    } catch (Exception e) {
      logger.error(e.getMessage());
    }
  }

  /**
   * Validates if the specified directory contains only pairs for the data file
   * and their corresponding mappings.
   * 
   * @param dirPath Target directory path.
   */
  private void validateDirContents(Path dirPath) {
    Collection<URI> csvFiles = this.getFiles(dirPath, CSV_FILE_EXTENSION);
    Collection<URI> ymlFiles = this.getFiles(dirPath, YML_FILE_EXTENSION);
    if (csvFiles.size() != ymlFiles.size()) {
      logger.error("Detected missing file pairs with {} csv and {} yml files! Ensure files are even.",
          csvFiles.size(), ymlFiles.size());
      throw new IllegalArgumentException();
    }
  }

  /**
   * Generate RML rules from YARRRML files in the directory if available.
   * 
   * @param dirPath Target directory path.
   */
  private ByteArrayOutputStream genRmlRules(Path dirPath) {
    logger.info("Converting the YARRRML inputs into RML rules...");
    String containerId = super.getContainerId(super.getContainerName());
    Collection<URI> ymlFiles = this.getFiles(dirPath, YML_FILE_EXTENSION);
    StringBuilder inputCommand = new StringBuilder();
    // Iterate to copy each file and add the copied file path to the CMD command
    for (URI file : ymlFiles) {
      String fileName = Paths.get(file).getFileName().toString();
      String targetFilePath = "/" + fileName;
      this.sendFile(containerId, Paths.get(targetFilePath), this.getFileContents(file));
      // Add white space after the previous commands
      inputCommand.append(" -i ").append(targetFilePath);
    }

    // Execute the command and return the RML mappings as streams
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    ByteArrayOutputStream errorStream = new ByteArrayOutputStream();
    String execId = super.createComplexCommand(containerId, "yarrrml-parser", inputCommand.toString())
        .withOutputStream(outputStream)
        .withErrorStream(errorStream)
        .exec();
    super.handleErrors(errorStream, execId, logger);
    return outputStream;
  }

  /**
   * Retrieves the files in the target directory.
   * 
   * @param dirPath       Target directory path.
   * @param fileExtension The file extension of interest.
   */
  private Collection<URI> getFiles(Path dirPath, String fileExtension) {
    try {
      logger.info(MessageFormat.format("Getting all {0} files from directory {1} ...", fileExtension,
          dirPath.toAbsolutePath()));
      URL dirUrl = dirPath.toUri().toURL();
      return FileUtils.listFiles(dirUrl, fileExtension);
    } catch (URISyntaxException | MalformedURLException ex) {
      logger.error(ex.getMessage());
      throw new IllegalArgumentException("Invalid directory path: " + ex.getMessage());
    } catch (IOException ex) {
      logger.error(ex.getMessage());
      throw new RuntimeException("Failed to list files in the directory: " + ex.getMessage());
    }
  }

  /**
   * Retrieves the contents of the specified file.
   * 
   * @param file Target file.
   */
  private byte[] getFileContents(URI file) {
    try {
      Path path = Paths.get(file);
      return Files.readAllBytes(path);
    } catch (IOException ex) {
      logger.error(ex.getMessage());
      throw new RuntimeException("Unable to read file: " + ex.getMessage());
    }
  }

  /**
   * Copy the file contents into the specified container at the target path.
   * 
   * @param containerId ID of the target container.
   * @param filePath    Target file path location in the container.
   * @param content     Contents to be copied.
   */
  private void sendFile(String containerId, Path filePath, byte[] content) {
    sendFilesContent(containerId, Map.of(filePath.getFileName().toString(), content),
        filePath.getParent().toString());
  }
}
