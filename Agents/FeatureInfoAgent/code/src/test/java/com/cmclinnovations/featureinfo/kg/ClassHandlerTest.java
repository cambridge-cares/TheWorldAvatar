package com.cmclinnovations.featureinfo.kg;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.AdditionalMatchers;
import org.mockito.ArgumentMatchers;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.EndpointType;
import com.cmclinnovations.featureinfo.config.NamespaceGetterTest;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * Tests for the ClassHandler class.
 */
public class ClassHandlerTest {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ClassHandler.class);

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

        // Add mock endpoints to the config
        CONFIG.addEndpoint(new ConfigEndpoint("ONTOP", "http://my-fake-ontop.com/", null, null, EndpointType.ONTOP));
        CONFIG.addEndpoint(new ConfigEndpoint("POSTGRES", "http://my-fake-postgres.com/", null, null, EndpointType.POSTGRES));
        CONFIG.addEndpoint(new ConfigEndpoint("blazegraph-test", "http://my-fake-blazegraph.com/namespace/blazegraph-test/sparql", null, null, EndpointType.BLAZEGRAPH));

        // Write a temporary query file
        try {
            String tmpdir = System.getProperty("java.io.tmpdir");
            Path tmpQuery = Paths.get(tmpdir, "MetaHandlerTest.sparql");
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
     * Clean up after all tests have executed.
     */
    @AfterAll
    public static void cleanup() {
          // Delete the temporary query file
          try {
            String tmpdir = System.getProperty("java.io.tmpdir");
            Path tmpQuery = Paths.get(tmpdir, "MetaHandlerTest.sparql");
            Files.deleteIfExists(tmpQuery);

            LOGGER.info("Removed temporary query file (" + tmpQuery.toString() + ").");
        } catch(IOException exception) {
            exception.printStackTrace(System.out);
            throw new RuntimeException("Could not delete temporary query file!");
        }
    }

    /**
     * Tests that an array of class names can be determined for a given IRI.
     * 
     * Note that this does not test the functionality of the queries, simply that the
     * MetaHandler class can handle sending the query and parsing the result.
     */
    @Test
    public void getClasses() {
        // Create a handler
        ClassHandler handler = new ClassHandler("http://some-fake-iri/", new ArrayList<>());

        try {
            // Set up a mock RemoteStoreClient with a set response
            RemoteStoreClient rsClient = mock(RemoteStoreClient.class);
            handler.setClient(rsClient);

            // Respond to the non-ONTOP query with a fake class
            when(rsClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.anyString()))
                .thenReturn(
                    new org.json.JSONArray("[{\"class\": \"com.cmclinnovations.kg.ClassOne\"}]")
                );

            // Ask the handler to query to determine the classes
            List<String> classMatches = handler.getClasses();

            // Check result
            Assertions.assertNotNull(classMatches, "Expected a non-null return result!");
            Assertions.assertFalse(classMatches.isEmpty(), "Did not expect an empty String!");
            Assertions.assertTrue(classMatches.contains("com.cmclinnovations.kg.ClassOne"), "Could not find expected class within collection!");

        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            Assertions.fail("Unexpected exception thrown when trying to determine classes for mock IRI.");
        }
    }

    /**
     * Tests that an array of class names can be determined for a given IRI; this differs
     * from the previous test in that it intentionally fails the simple class query, and
     * tests the functionality to run a second query that contains the ONTOP service.
     * 
     * Note that this does not test the functionality of the queries, simply that the
     * MetaHandler class can handle sending the query and parsing the result.
     */
    @Test
    public void getClassesOntop() {
        // Create a handler
        ClassHandler handler = new ClassHandler("http://some-fake-iri/", new ArrayList<>());

        try {
            // Set up a mock RemoteStoreClient with a set response
            RemoteStoreClient rsClient = mock(RemoteStoreClient.class);
            handler.setClient(rsClient);

            // Respond to the non-ONTOP query with nothing
            when(rsClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                AdditionalMatchers.not(ArgumentMatchers.contains("ontop")))
            ).thenReturn(
                 new org.json.JSONArray("[]")
            );

            // Respond to the ONTOP query with a fake class
            when(rsClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.contains("ontop"))
            ).thenReturn(
                new org.json.JSONArray("[{\"class\": \"com.cmclinnovations.kg.ClassOne\"}]")
            );

            // Ask the handler to query to determine the classes
            List<String> classMatches = handler.getClasses();

            // Check result
            Assertions.assertNotNull(classMatches, "Expected a non-null return result!");
            Assertions.assertFalse(classMatches.isEmpty(), "Did not expect an empty String!");
            Assertions.assertTrue(classMatches.contains("com.cmclinnovations.kg.ClassOne"), "Could not find expected class within collection!");
   

        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            Assertions.fail("Unexpected exception thrown when trying to determine classes for mock IRI.");
        }
    }

    /**
     * Tests that an actual query file can be determined and read into memory.
     */
    @Test
    public void readQueryFile() {
          // Create a handler
          ClassHandler handler = new ClassHandler("http://some-fake-iri/", new ArrayList<>());

          try {
              // Set up a mock RemoteStoreClient with a set response
              RemoteStoreClient rsClient = mock(RemoteStoreClient.class);
              handler.setClient(rsClient);
  
              // Respond to the non-ONTOP query with a fake class
              when(rsClient.executeFederatedQuery(
                  ArgumentMatchers.anyList(),
                  ArgumentMatchers.anyString()))
                  .thenReturn(
                      new org.json.JSONArray("[{\"class\": \"SAMPLE-QUERY-CLASS\"}]")
                  );
  
              // Ask the handler to query to determine the classes
              String classMatch = handler.getClassMatch();
  
              // Check result
              Assertions.assertNotNull(classMatch, "Expected a non-null return result!");
              Assertions.assertFalse(classMatch.isEmpty(), "Did not expect an empty String!");
              Assertions.assertEquals("SAMPLE-QUERY-CLASS", classMatch, "Could not find expected class within collection!");
  
              // Get the query for that class
              String queryContent = CONFIG.getMetaQuery(classMatch);
              Assertions.assertNotNull(queryContent, "Expected a non-null return result!");
              Assertions.assertFalse(queryContent.isEmpty(), "Did not expect an empty String!");
              Assertions.assertEquals("SAMPLE-QUERY", queryContent, "Could not find expected class within collection!");

          } catch(Exception exception) {
              exception.printStackTrace(System.out);
              Assertions.fail("Unexpected exception thrown when trying to read a sample query file!");
          }
    }

}
// End of class.