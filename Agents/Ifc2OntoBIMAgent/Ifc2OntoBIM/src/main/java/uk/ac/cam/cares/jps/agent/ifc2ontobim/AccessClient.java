package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdfconnection.RDFConnection;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.*;

/**
 * A client that performs file operations and network requests
 * such as sending POST request to other urls, uploading a TTL file to a SPARQL endpoint,
 * or retrieving configuration files.
 *
 * @author qhouyee
 */
class AccessClient {
    // Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String TARGET_DIR_ERROR_MSG = "Failed to access target directory at: ";
    private static final String NO_CLIENT_PROPERTIES_MSG = "No config.properties file detected! Please place the file in the config directory.";
    private static final String INACCESSIBLE_CLIENT_PROPERTIES_MSG = "File could not be accessed! See error message for more details: ";
    // Fields
    private static final String CLIENT_PROPERTIES_FILEPATH = System.getProperty("user.dir") + "/config/config.properties";
    protected static final String QUERY_ENDPOINT = "sparql.query.endpoint";
    protected static final String UPDATE_ENDPOINT = "sparql.update.endpoint";
    protected static final String IFC_OWL_CONVERTER_API = "ifc.owl.agent";

    /**
     * Sends a POST request with requested parameters to the specified url.
     *
     * @param url        The specified url.
     * @param jsonParams Request parameters.
     */
    protected static void sendPostRequest(String url, String jsonParams) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = null;
        try {
            request = HttpRequest.newBuilder()
                    .header("Content-Type", "application/json")
                    .uri(URI.create(url))
                    .POST(HttpRequest.BodyPublishers.ofString(jsonParams))
                    .timeout(Duration.ofSeconds(3600))
                    .build();
            // Await response before continue executing the rest of the code
            client.send(request, HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            throw new JPSRuntimeException(e.getMessage() + " If connection is refused, the url is likely invalid!");
        } catch (InterruptedException e) {
            throw new RuntimeException("Thread has been interrupted!" + e.getMessage());
        }
    }

    /**
     * Uploads statements to a specified SPARQL endpoint.
     *
     * @param endpoint     The specified SPARQL endpoint.
     * @param statementSet The statements to be uploaded into the endpoint.
     */
    public static void uploadStatements(String endpoint, LinkedHashSet<Statement> statementSet) {
        // Add the statements into a new model
        Model model = ModelFactory.createDefaultModel();
        model.add(new ArrayList<>(statementSet));
        // Upload the model directly
        try (RDFConnection conn = RDFConnection.connect(endpoint)) {
            conn.load(model);
        }
    }

    /**
     * List all .ttl files in the target directory and store them in a Set
     *
     * @param ttlDir The specified directory.
     * @return A set object containing all the ttl files in the target directory
     */
    public static Set<String> listTTLFiles(String ttlDir) {
        Set<String> fileSet = new HashSet<>();
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(Paths.get(ttlDir))) {
            for (Path path : stream) {
                if (!Files.isDirectory(path) &&
                        path.getFileName().toString().substring(
                                path.getFileName().toString().lastIndexOf('.') + 1).equals("ttl")) {
                    // Add only ttl files to the list
                    fileSet.add(path.toString());
                }
            }
        } catch (IOException e) {
            LOGGER.fatal(TARGET_DIR_ERROR_MSG + ttlDir + "\n" + e.getMessage());
            throw new JPSRuntimeException(TARGET_DIR_ERROR_MSG + ttlDir + "\n" + e.getMessage());
        }
        return fileSet;
    }

    /**
     * Retrieves client properties stored in the properties file.
     *
     * @return The configuration endpoints as mappings.
     */
    protected static Map<String, String> retrieveClientProperties() {
        StringBuilder missingPropertiesErrorMessage = new StringBuilder();
        try (InputStream input = new FileInputStream(CLIENT_PROPERTIES_FILEPATH)) {
            Properties prop = new Properties();
            Map<String, String> config = new HashMap<>();
            LOGGER.debug("Retrieving configuration from " + CLIENT_PROPERTIES_FILEPATH + "...");
            prop.load(input);
            String endpoint = validateProperties(prop, QUERY_ENDPOINT, missingPropertiesErrorMessage);
            config.put(QUERY_ENDPOINT, endpoint);
            endpoint = validateProperties(prop, UPDATE_ENDPOINT, missingPropertiesErrorMessage);
            config.put(UPDATE_ENDPOINT, endpoint);
            endpoint = validateProperties(prop, IFC_OWL_CONVERTER_API, missingPropertiesErrorMessage);
            config.put(IFC_OWL_CONVERTER_API, endpoint);
            String missingMessage = missingPropertiesErrorMessage.toString();
            if (!missingMessage.isEmpty()) {
                LOGGER.error("Missing Properties:\n" + missingMessage);
                throw new JPSRuntimeException("Missing Properties:\n" + missingMessage);
            }
            LOGGER.info("All required configurations have been retrieved!");
            return config;
        } catch (FileNotFoundException e) {
            LOGGER.error(NO_CLIENT_PROPERTIES_MSG);
            throw new JPSRuntimeException(NO_CLIENT_PROPERTIES_MSG);
        } catch (IOException e) {
            LOGGER.error(INACCESSIBLE_CLIENT_PROPERTIES_MSG + e);
            throw new JPSRuntimeException(INACCESSIBLE_CLIENT_PROPERTIES_MSG + e);
        }
    }

    /**
     * Validates the client properties, and return their value if it exists.
     *
     * @param prop                          A Properties object containing the required properties.
     * @param propertyKey                   The property key associated with the value.
     * @param missingPropertiesErrorMessage An error message that will be written if there is no property.
     * @return The value of the endpoints.
     */
    private static String validateProperties(Properties prop, String propertyKey, StringBuilder missingPropertiesErrorMessage) {
        if (prop.getProperty(propertyKey) == null) {
            missingPropertiesErrorMessage.append(propertyKey + " is missing! Please add the input to client.properties.\n");
            LOGGER.error(propertyKey + " is missing! Please add the input to client.properties.");
        } else {
            return prop.getProperty(propertyKey);
        }
        return "";
    }

    /**
     * Perform clean up operation
     *
     * @param ttlFile The file path to the generated IfcOwl TTL file.
     */
    public static void cleanUp(String ttlFile) {
        File file = new File(ttlFile);
        if (file.delete()) {
            LOGGER.debug("Generated IfcOwl file has been removed");
        }
    }
}
