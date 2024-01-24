package uk.ac.cam.cares.jps.agent.carpark.file;

import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Queue;

import static org.junit.jupiter.api.Assertions.*;

class ConfigReaderTest {
    private static final String RDB_URL_KEY = "db.url";
    private static final String RDB_USERNAME_KEY = "db.user";
    private static final String RDB_PASSWORD_KEY = "db.password";
    private static final String SPARQL_QUERY_ENDPOINT_KEY = "sparql.query.endpoint";
    private static final String SPARQL_UPDATE_ENDPOINT_KEY = "sparql.update.endpoint";
    private static final String SPARQL_USERNAME_KEY = "sparql.username";
    private static final String SPARQL_PASSWORD_KEY = "sparql.password";
    private static final String API_AVAILABLE_LOT_ENDPOINT_KEY = "carpark.api.lot.endpoint";
    private static final String API_LOT_TOKEN_KEY = "carpark.api.lot.token";
    private static final String API_PRICING_ENDPOINT_KEY = "carpark.api.pricing.endpoint";
    private static final String SAMPLE_RDB_DB = "jdbc:postgresql://host.docker.internal:5432/carpark";
    private static final String SAMPLE_RDB_USER = "postgres";
    private static final String SAMPLE_RDB_PASS = "postgis";
    private static final String SAMPLE_SPARQL_QUERY_ENDPOINT = "http://host.docker.internal:9999/blazegraph/namespace/carpark/sparql";
    private static final String SAMPLE_SPARQL_UPDATE_ENDPOINT = "http://host.docker.internal:9999/blazegraph/namespace/carpark/sparql";
    private static final String SAMPLE_SPARQL_USER = "admin";
    private static final String SAMPLE_SPARQL_PASS = "carparks";
    private static final String SAMPLE_API_LOT_ENDPOINT = "http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2";
    private static final String SAMPLE_API_LOT_TOKEN = "bjaga8f";
    private static final String SAMPLE_API_PRICING_ENDPOINT = "https://data.gov.sg/api/action/datastore_search";

    @Test
    void testRetrieveRDBConfig_NoConfigFile() {
        String invalidFilePath = "invalid";
        // Verify right exception is thrown
        IOException thrownError = assertThrows(IOException.class, () -> ConfigReader.retrieveRDBConfig(invalidFilePath));
        assertEquals("No file was found from the path " + invalidFilePath, thrownError.getMessage());
    }

    @Test
    void testRetrieveRDBConfig_MissingKeys() throws IOException {
        File config = genSampleAPIConfigFile("", "", "");
        try {
            // Execute method and ensure right error is thrown
            IOException thrownError = assertThrows(IOException.class, () -> ConfigReader.retrieveRDBConfig(config.getAbsolutePath()));
            assertEquals(String.format("Missing property: %s in the file %s", RDB_URL_KEY, config.getAbsolutePath()), thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveRDBConfig_MissingInputs() throws IOException {
        File config = genSampleRDBConfigFile(SAMPLE_RDB_DB, "", SAMPLE_RDB_PASS);
        try {
            // Execute method and ensure right error is thrown
            IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class, () -> ConfigReader.retrieveRDBConfig(config.getAbsolutePath()));
            assertEquals(String.format("Property %s cannot be empty in the file %s", RDB_USERNAME_KEY, config.getAbsolutePath()), thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveRDBConfig_Success() throws IOException {
        File config = genSampleRDBConfigFile(SAMPLE_RDB_DB, SAMPLE_RDB_USER, SAMPLE_RDB_PASS);
        try {
            // Execute method
            Queue<String> result = ConfigReader.retrieveRDBConfig(config.getAbsolutePath());
            // Verify results are expected
            assertEquals(SAMPLE_RDB_DB, result.poll());
            assertEquals(SAMPLE_RDB_USER, result.poll());
            assertEquals(SAMPLE_RDB_PASS, result.poll());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveSparqlConfig_NoConfigFile() {
        String invalidFilePath = "invalid";
        // Verify right exception is thrown
        IOException thrownError = assertThrows(IOException.class, () -> ConfigReader.retrieveSparqlConfig(invalidFilePath));
        assertEquals("No file was found from the path " + invalidFilePath, thrownError.getMessage());
    }

    @Test
    void testRetrieveSparqlConfig_MissingKeys() throws IOException {
        File config = genSampleAPIConfigFile("", "", "");
        try {
            // Execute method and ensure right error is thrown
            IOException thrownError = assertThrows(IOException.class, () -> ConfigReader.retrieveSparqlConfig(config.getAbsolutePath()));
            assertEquals(String.format("Missing property: %s in the file %s", SPARQL_QUERY_ENDPOINT_KEY, config.getAbsolutePath()), thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveSparqlConfig_MissingInputs() throws IOException {
        File config = genSampleSparqlConfigFile(SAMPLE_SPARQL_QUERY_ENDPOINT, "", "", "");
        try {
            // Execute method and ensure right error is thrown
            IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class, () -> ConfigReader.retrieveSparqlConfig(config.getAbsolutePath()));
            assertEquals(String.format("Property %s cannot be empty in the file %s", SPARQL_UPDATE_ENDPOINT_KEY, config.getAbsolutePath()), thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveSparqlConfig_SuccessWithoutAuthentication() throws IOException {
        File config = genSampleUnauthenticatedSparqlConfigFile(SAMPLE_SPARQL_QUERY_ENDPOINT, SAMPLE_SPARQL_UPDATE_ENDPOINT);
        try {
            // Execute method
            Queue<String> result = ConfigReader.retrieveSparqlConfig(config.getAbsolutePath());
            // Verify results are expected
            assertEquals(2, result.size());
            assertEquals(SAMPLE_SPARQL_QUERY_ENDPOINT, result.poll());
            assertEquals(SAMPLE_SPARQL_UPDATE_ENDPOINT, result.poll());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveSparqlConfig_SuccessWithAuthentication() throws IOException {
        File config = genSampleSparqlConfigFile(SAMPLE_SPARQL_QUERY_ENDPOINT, SAMPLE_SPARQL_UPDATE_ENDPOINT, SAMPLE_SPARQL_USER, SAMPLE_SPARQL_PASS);
        try {
            // Execute method
            Queue<String> result = ConfigReader.retrieveSparqlConfig(config.getAbsolutePath());
            // Verify results are expected
            assertEquals(4, result.size());
            assertEquals(SAMPLE_SPARQL_QUERY_ENDPOINT, result.poll());
            assertEquals(SAMPLE_SPARQL_UPDATE_ENDPOINT, result.poll());
            assertEquals(SAMPLE_SPARQL_USER, result.poll());
            assertEquals(SAMPLE_SPARQL_PASS, result.poll());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveAPIConfig_NoConfigFile() {
        String invalidFilePath = "invalid";
        // Verify right exception is thrown
        IOException thrownError = assertThrows(IOException.class, () -> ConfigReader.retrieveAPIConfig(invalidFilePath));
        assertEquals("No file was found from the path " + invalidFilePath, thrownError.getMessage());
    }

    @Test
    void testRetrieveAPIConfig_MissingKeys() throws IOException {
        File config = genSampleRDBConfigFile("", "", "");
        try {
            // Execute method and ensure right error is thrown
            IOException thrownError = assertThrows(IOException.class, () -> ConfigReader.retrieveAPIConfig(config.getAbsolutePath()));
            assertEquals(String.format("Missing property: %s in the file %s", API_AVAILABLE_LOT_ENDPOINT_KEY, config.getAbsolutePath()), thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveAPIConfig_MissingInputs() throws IOException {
        File config = genSampleAPIConfigFile(SAMPLE_API_LOT_ENDPOINT, "", SAMPLE_API_PRICING_ENDPOINT);
        try {
            // Execute method and ensure right error is thrown
            IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class, () -> ConfigReader.retrieveAPIConfig(config.getAbsolutePath()));
            assertEquals(String.format("Property %s cannot be empty in the file %s", API_LOT_TOKEN_KEY, config.getAbsolutePath()), thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveAPIConfig_Success() throws IOException {
        File config = genSampleAPIConfigFile(SAMPLE_API_LOT_ENDPOINT, SAMPLE_API_LOT_TOKEN, SAMPLE_API_PRICING_ENDPOINT);
        try {
            // Execute method
            Queue<String> result = ConfigReader.retrieveAPIConfig(config.getAbsolutePath());
            // Verify results are expected
            assertEquals(SAMPLE_API_LOT_ENDPOINT, result.poll());
            assertEquals(SAMPLE_API_LOT_TOKEN, result.poll());
            assertEquals(SAMPLE_API_PRICING_ENDPOINT, result.poll());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    public static File genSampleRDBConfigFile(String db, String user, String pass) throws IOException {
        File file = new File(System.getProperty("user.dir") + "/config/client.properties");
        createFileAndDirectoryIfUnavailable(file);
        PrintWriter writer = new PrintWriter(new FileWriter(file, true));
        writer.println(RDB_URL_KEY + "=" + db);
        writer.println(RDB_USERNAME_KEY + "=" + user);
        writer.println(RDB_PASSWORD_KEY + "=" + pass);
        writer.close();
        return file;
    }

    public static File genSampleUnauthenticatedSparqlConfigFile(String queryEndpoint, String updateEndpoint) throws IOException {
        return genSampleSparqlConfigFile(queryEndpoint, updateEndpoint, "", "");
    }

    public static File genSampleSparqlConfigFile(String queryEndpoint, String updateEndpoint, String user, String pass) throws IOException {
        File file = new File(System.getProperty("user.dir") + "/config/client.properties");
        createFileAndDirectoryIfUnavailable(file);
        PrintWriter writer = new PrintWriter(new FileWriter(file, true));
        writer.println(SPARQL_QUERY_ENDPOINT_KEY + "=" + queryEndpoint);
        writer.println(SPARQL_UPDATE_ENDPOINT_KEY + "=" + updateEndpoint);
        writer.println(SPARQL_USERNAME_KEY + "=" + user);
        writer.println(SPARQL_PASSWORD_KEY + "=" + pass);
        writer.close();
        return file;
    }

    public static File genSampleAPIConfigFile(String apiLotEndpoint, String apiLotToken, String apiPricingEndpoint) throws IOException {
        File file = new File(System.getProperty("user.dir") + "/config/api.properties");
        createFileAndDirectoryIfUnavailable(file);
        PrintWriter writer = new PrintWriter(new FileWriter(file, true));
        writer.println(API_AVAILABLE_LOT_ENDPOINT_KEY + "=" + apiLotEndpoint);
        writer.println(API_LOT_TOKEN_KEY + "=" + apiLotToken);
        writer.println(API_PRICING_ENDPOINT_KEY + "=" + apiPricingEndpoint);
        writer.close();
        return file;
    }

    private static void createFileAndDirectoryIfUnavailable(File file) {
        // Check if the directory exists, create it if it doesn't
        if (!file.getParentFile().exists()) {
            boolean result = file.getParentFile().mkdirs();
            if (!result) {throw new RuntimeException("Directory does not exist. But failed to create directory...");}
        }
        // Check if file exists or not
        if (!file.exists()) {
            try {
                boolean result = file.createNewFile();
                if (!result) {throw new RuntimeException("File does not exist. But failed to create file...");}
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
