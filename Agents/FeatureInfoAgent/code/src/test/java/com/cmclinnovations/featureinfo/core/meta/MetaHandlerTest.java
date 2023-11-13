package com.cmclinnovations.featureinfo.core.meta;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;

import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.skyscreamer.jsonassert.JSONAssert;

import com.cmclinnovations.featureinfo.TestUtils;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.ConfigStoreTest;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * Tests for the MetaHandler class.
 */
public class MetaHandlerTest {
    
    /**
     * Temporary directory to store test data.
     */
    private static final Path TEMP_DIR = Paths.get(System.getProperty("java.io.tmpdir"));

    /**
     * Copy test data out from the resources directory so it can be loaded in the same
     * manner that files are at runtime.
     * 
     * @throws IOException if temporary test data cannot be created.
     */
    @BeforeAll
    public static void setup() throws IOException {
        FileUtils.deleteDirectory(TEMP_DIR.resolve("mock-config-01").toFile());

        // Copy out test data sets to the temporary directory.
        File mockDir01 = new File(ConfigStoreTest.class.getResource("/mock-config-01").getFile());
        Assertions.assertTrue(
            TestUtils.copyFilesRecusively(mockDir01, TEMP_DIR.toFile()),
            "Could not export test data from within JAR to temporary directory!"
        );
    }

    /**
     * Clean up after tests finish.
     */
    @AfterAll
    public static void cleanUp() throws Exception {
         FileUtils.deleteDirectory(TEMP_DIR.resolve("mock-config-01").toFile());
    }

    /**
     * Tests the MetaHandler by attempting to get data for a single matching
     * configuration entry.
     * 
     * @throws Exception if SPARQL fails.
     */
    @Test
    public void testSingleMatchQuery() throws Exception {
        Path configFile = TEMP_DIR.resolve("mock-config-01/config.json");

        // Mock a config store based on the real config file
        ConfigStore configStore = TestUtils.mockConfig(configFile);
        
        // Initialise a metahandler
        MetaHandler metaHandler = new MetaHandler(
            "https://test-stack/features/feature-one",
            Optional.empty(),
            configStore
        );       

        // Set a mock KG client
        metaHandler.setClient(mockClient());

        // Attempt to get metadata
        JSONObject result = metaHandler.getData(
            configStore.getConfigEntries().subList(0, 1),
            TestUtils.mockResponse()
        );

        // Expected response
        JSONObject expected = new JSONObject(
            """
            {
                "Name": "Art Vandelay",
                "Job(s)": ["Architect","Importer-Exporter","Latex Manufacturer"]
            }
            """
        );
        JSONAssert.assertEquals(expected, result, false);
    }

    /**
     * Tests the MetaHandler by attempting to get data for a multiple matching
     * configuration entries.
     * 
     * @throws Exception if SPARQL fails.
     */
    @Test
    public void testMultipleMatchQuery() throws Exception {
        Path configFile = TEMP_DIR.resolve("mock-config-01/config.json");

        // Mock a config store based on the real config file
        ConfigStore configStore = TestUtils.mockConfig(configFile);
        
        // Initialise a metahandler
        MetaHandler metaHandler = new MetaHandler(
            "https://test-stack/features/feature-one",
            Optional.empty(),
            configStore
        );       

        // Set a mock KG client
        metaHandler.setClient(mockClient());

        // Attempt to get metadata
        JSONObject result = metaHandler.getData(
            configStore.getConfigEntries().subList(0, 2),
            TestUtils.mockResponse()
        );

        // Expected response
        JSONObject expected = new JSONObject(
            """
            {
                "Name": ["Art Vandelay", "Cosmo Kramer"],
                "Job(s)": ["Architect", "Importer-Exporter", "Latex Manufacturer"],
                "Nickname(s)": "The Assman"
            }
            """
        );
        JSONAssert.assertEquals(expected, result, false);
    }

    /**
     * Returns a mocked RemoteStoreClient for mocked interaction with KGs.
     * 
     * @returns mocked RemoteStoreClient instance.
     */
    private RemoteStoreClient mockClient() throws Exception {
        RemoteStoreClient spiedClient = Mockito.mock(RemoteStoreClient.class);

        Mockito.when(
            spiedClient.executeQuery("classOneMeta")
        ).thenReturn(
            new JSONArray(
                """
                [
                    {\"Property\":\"Name\",\"Value\":\"Art Vandelay\"},
                    {\"Property\":\"Job(s)\",\"Value\":\"Architect\"},
                    {\"Property\":\"Job(s)\",\"Value\":\"Importer-Exporter\"},
                    {\"Property\":\"Job(s)\",\"Value\":\"Latex Manufacturer\"}
                ]
                """
            )
        );

        Mockito.when(
            spiedClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.eq("classOneMeta")
            )
        ).thenReturn(
            new JSONArray(
                """
                [
                    {\"Property\":\"Name\",\"Value\":\"Art Vandelay\"},
                    {\"Property\":\"Job(s)\",\"Value\":\"Architect\"},
                    {\"Property\":\"Job(s)\",\"Value\":\"Importer-Exporter\"},
                    {\"Property\":\"Job(s)\",\"Value\":\"Latex Manufacturer\"}
                ]
                """
            )
        );

        Mockito.when(
            spiedClient.executeQuery("classTwoMeta")
        ).thenReturn(
            new JSONArray(
                """
                [
                    {\"Property\":\"Name\",\"Value\":\"Cosmo Kramer\"},
                    {\"Property\":\"Nickname(s)\",\"Value\":\"The Assman\"}
                ]
                """
            )
        );

        Mockito.when(
            spiedClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                 ArgumentMatchers.eq("classTwoMeta")
            )
        ).thenReturn(
            new JSONArray(
                """
                [
                    {\"Property\":\"Name\",\"Value\":\"Cosmo Kramer\"},
                    {\"Property\":\"Nickname(s)\",\"Value\":\"The Assman\"}
                ]
                """
            )
        );

        return spiedClient;
    }

}
// End of class.