package com.cmclinnovations.featureinfo.config;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Ignore;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;

/**
 * Testing for the ConfigStore class.
 */
public class ConfigStoreTest {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ConfigStoreTest.class);

    /**
     * Test config store.
     */
    private static final ConfigStore CONFIG = new ConfigStore();

     /**
     * Read in mock config file before running tests.
     */
    @BeforeAll
    public static void setup() {
        try (InputStream is = NamespaceGetterTest.class.getResourceAsStream("/mock-config-file.json")) {
            BufferedReader bufferReader = new BufferedReader(new InputStreamReader(is));
            StringBuilder stringBuilder = new StringBuilder();

            String eachStringLine;
            while ((eachStringLine = bufferReader.readLine()) != null) {
                stringBuilder.append(eachStringLine).append("\n");
            }
            
            CONFIG.loadFile(stringBuilder.toString());
            FeatureInfoAgent.CONFIG = CONFIG;

        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not read mock config file!");
        }

        // Write a temporary query file
        try {
            String tmpdir = System.getProperty("java.io.tmpdir");
            Path tmpQuery = Paths.get(tmpdir, "ConfigStoreTest.sparql");
            Files.writeString(tmpQuery, "SAMPLE-QUERY");

            // Add to the config
            CONFIG.addMetaQueryForClass("SAMPLE-QUERY-CLASS", tmpQuery.toString());
            LOGGER.info("Written temporary query file for testing (" + tmpQuery.toString() + ").");
        } catch(IOException exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not write temporary query file!");
        }
    }

    /**
     * Test reading the mock configuration file.
     * 
     * @throws Exception
     */
    @Test
    public void readConfigFile() throws Exception {
        Assertions.assertEquals(
            4,
            CONFIG.metaQueries.size(), 
            "Expected number of loaded metadata queries is not correct!"
        );
        Assertions.assertEquals(
            3,
            CONFIG.timeQueries.size(), 
            "Expected number of loaded timeseries queries is not correct!"
        );
        Assertions.assertTrue(
            CONFIG.metaQueries.containsKey("com.cmclinnovations.kg.ClassOne"),
            "Could not find expected key in registered classes!"
        );
        Assertions.assertTrue(
            CONFIG.timeQueries.containsKey("com.cmclinnovations.kg.ClassOne"),
            "Could not find expected key in registered classes!"
        );
    }
    
    /**
     * Given a class, test that the corresponding query file can be read.
     */
    @Test
    public void testReadQuery() throws IOException {
        // Create a handler
        String mockClass = "SAMPLE-QUERY-CLASS";

        // Read the query file
        String query = CONFIG.getMetaQuery(mockClass);

        // Check result
        Assertions.assertNotNull(query, "Expected a non-null return result!");
        Assertions.assertFalse(query.isBlank(), "Did not expect an empty String result!");
        Assertions.assertEquals("SAMPLE-QUERY", query, "Query read from file did not match expected result!");
    }


    /**
     * Tests that the local (i.e. in-stack) Blazegraph instance can be queried to
     * dynamically determine what namespaces are available.
     * 
     * @throws Exception
     */
    @Ignore
    @Test
    public void testBlazegraph() throws Exception {
        // TODO: Will only work when the functionality to spin up a test stack instance with test data, has been implemented.
    }

    /**
     * Tests that the local (i.e. in-stack) Ontop instance can be discovered.
     * 
     * @throws Exception
     */
    @Ignore
    @Test
    public void testOntop() throws Exception {
        // TODO: Will only work when the functionality to spin up a test stack instance with test data, has been implemented.
    }

    /**
     * Tests that the local (i.e. in-stack) PostGreSQL instance can be discovered.
     * 
     * @throws Exception
     */
    @Ignore
    @Test
    public void testPostgres() throws Exception {
        // TODO: Will only work when the functionality to spin up a test stack instance with test data, has been implemented.
    }



}
// End of class.