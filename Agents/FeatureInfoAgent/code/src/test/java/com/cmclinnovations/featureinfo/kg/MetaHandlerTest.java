package com.cmclinnovations.featureinfo.kg;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.EndpointType;
import com.cmclinnovations.featureinfo.config.NamespaceGetterTest;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * Tests for the MetaHandler class.
 */
public class MetaHandlerTest {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(MetaHandlerTest.class);

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
            CONFIG.addMetaQueryForClass("SAMPLE-CLASS", tmpQuery.toString());
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
     * Tests that a query can be submitted and the response parsed.
     */
    @Test
    public void testQuery() {
        // Create a handler
        MetaHandler handler = new MetaHandler("http://some-fake-iri/", "SAMPLE-CLASS", CONFIG.getBlazegraphEndpoints());

        try {
            // Set up a mock RemoteStoreClient with a set response
            RemoteStoreClient rsClient = mock(RemoteStoreClient.class);
            handler.setClient(rsClient);

            // Respond to the metadata query with some fake data
            when(rsClient.executeQuery(
                eq("SAMPLE-QUERY")))
                .thenReturn(
                    new org.json.JSONArray("[{\"Property\":\"PropertyOne\",\"Value\":\"ValueOne\"},{\"Property\":\"PropertyTwo\",\"Value\":\"ValueTwo\"}]")
                );

            // Set up a mock response
            HttpServletResponse httpResponse = mock(HttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(httpResponse.getWriter()).thenReturn(printWriter);

            // Create expected object
            JSONArray expected = new JSONArray();
            JSONObject object = new JSONObject();
            object.put("PropertyOne", "ValueOne");
            object.put("PropertyTwo", "ValueTwo");
            expected.put(object);

            // Get the metadata result
            JSONArray result = handler.getData(httpResponse);

            // Compare
            Assertions.assertNotNull(result, "Null result encountered!");
            Assertions.assertTrue(expected.similar(result), "Resulting JSONArray did not match expected one!");
          
        } catch(Exception exception) {
            exception.printStackTrace(System.out);
            Assertions.fail("Unexpected exception thrown when trying to run query!");
        }
    }

}
// End of class.