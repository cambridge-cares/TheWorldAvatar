package com.cmclinnovations.featureinfo.core;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import com.cmclinnovations.featureinfo.TestUtils;
import com.cmclinnovations.featureinfo.config.ConfigEntry;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.ConfigStoreTest;
import com.cmclinnovations.featureinfo.config.StackEndpoint;
import com.cmclinnovations.featureinfo.config.StackEndpointType;
import com.cmclinnovations.featureinfo.objects.Request;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * Tests for the ClassHandler class.
 */
public class ClassHandlerTest {
    
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
     */
    @Test
    public void getClasses() throws Exception {
        Path configFile = TEMP_DIR.resolve("mock-config-01/config.json");

        // Create store instance (skipping stack integration)
        ConfigStore configStore = new ConfigStore(configFile.toString());
        configStore.loadDetails(false);

        // Setup mocking for stack endpoints
        ConfigStore spiedConfig = Mockito.spy(configStore);

        Mockito.when(
            spiedConfig.getStackEndpoints(
                ArgumentMatchers.same(StackEndpointType.ONTOP)
            )
        ).thenReturn(
            new ArrayList<>(Arrays.asList(
                new StackEndpoint("https://test-stack/ontop", null, null, StackEndpointType.ONTOP)
            ))
        );

        Mockito.when(
            spiedConfig.getStackEndpoints(
                ArgumentMatchers.same(StackEndpointType.BLAZEGRAPH)
            )
        ).thenReturn(
            new ArrayList<>(Arrays.asList(
                new StackEndpoint("https://test-stack/blazegraph-one", null, null, StackEndpointType.BLAZEGRAPH),
                new StackEndpoint("https://test-stack/blazegraph-two", null, null, StackEndpointType.BLAZEGRAPH)
            ))
        );

        // Mock a RemoteStoreClient instance
        RemoteStoreClient kgClient = mock(RemoteStoreClient.class);

        // Respond to the non-ONTOP query with nothing
        when(
            kgClient.executeFederatedQuery(
                ArgumentMatchers.anyList(),
                ArgumentMatchers.anyString()
            )
        ).thenReturn(
            new org.json.JSONArray("""
                [
                    { "class": "https://theworldavatar.io/mock-domain/ClassOne" },
                    { "class": "https://theworldavatar.io/mock-domain/ClassTwo" },
                    { "class": "https://theworldavatar.io/mock-domain/ClassThree" }
                ]
            """)
        );

        // Create a ClassHandler instance
        ClassHandler handler = new ClassHandler(spiedConfig, kgClient);

        // Run class determination logic
        List<ConfigEntry> matchingEntries = handler.determineClassMatches(
            new Request("https://test-stack/features/feature-one",null)
        );

        Assertions.assertEquals(
            3,
            matchingEntries.size(),
            "Did not return the expected number of class matches!"
        );
    }

}
// End of class.