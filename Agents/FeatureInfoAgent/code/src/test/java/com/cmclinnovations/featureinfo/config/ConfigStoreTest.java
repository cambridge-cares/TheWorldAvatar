package com.cmclinnovations.featureinfo.config;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.cmclinnovations.featureinfo.TestUtils;
import com.cmclinnovations.featureinfo.config.ConfigEntry.ConfigEntryBuilder;

/**
 * Testing for the ConfigStore class.
 */
public class ConfigStoreTest {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ConfigStoreTest.class);

    /**
     * Temporary directory to store test data.
     */
    private static final Path TEMP_DIR = Paths.get(System.getProperty("java.io.tmpdir"));

    /**
     * Copy test data out from the resources directory so it can be loaded in the same
     * manner that files are at runtime.
     */
    @BeforeAll
    public static void setup() {
        // Copy out test data sets to the temporary directory.
        File mockDir01 = new File(ConfigStoreTest.class.getResource("/mock-config-01").getFile());
        Assertions.assertTrue(
            TestUtils.copyFilesRecusively(mockDir01, TEMP_DIR.toFile()),
            "Could not export test data from within JAR to temporary directory!"
        );

        File mockDir02 = new File(ConfigStoreTest.class.getResource("/mock-config-02").getFile());
        Assertions.assertTrue(
            TestUtils.copyFilesRecusively(mockDir02, TEMP_DIR.toFile()),
            "Could not export test data from within JAR to temporary directory!"
        );
    }

    /**
     * Clean up after tests finish.
     */
    @AfterAll
    public static void cleanUp() throws Exception {
        Path mockDir01 = TEMP_DIR.resolve("mock-config-01");
        Files.deleteIfExists(mockDir01);

        Path mockDir02 = TEMP_DIR.resolve("mock-config-02");
        Files.deleteIfExists(mockDir02);
    }

    /**
     * Test reading and parsing the "mock-config-01" data set.
     */
    @Test
    public void loadConfig01() throws Exception {
        Path configFile = TEMP_DIR.resolve("mock-config-01\\config.json");

        // Create store instance (skipping stack integration)
        ConfigStore store = new ConfigStore(configFile.toString());
        store.loadDetails(false);

        // Check for expected number of entries
        Assertions.assertEquals(
            4,
            store.getConfigEntries().size(),
            "Configuration entries did not match expected number!"
        );

        // Check a sample of the configuration entries' content
        Assertions.assertEquals(
            "https://theworldavatar.io/mock-domain/ClassOne",
            store.getConfigEntries().get(0).getClassIRI(),
            "Class IRI for first entry did not match expected result!"
        );
        Assertions.assertEquals(
            "classTwoMeta",
            store.getConfigEntries().get(1).getMetaQueryContent(),
            "Metadata query for second entry did not match expected result!"
        );
        Assertions.assertEquals(
            "classFourTime",
            store.getConfigEntries().get(3).getTimeQueryContent(),
            "Time series query for fourth entry did not match expected result!"
        );
    }

    /**
     * Test reading and parsing the "mock-config-02" data set.
     */
    @Test
    public void loadConfig02() throws Exception {
        Path configFile = TEMP_DIR.resolve("mock-config-02\\config.json");

        // Create store instance (skipping stack integration)
        ConfigStore store = new ConfigStore(configFile.toString());
        store.loadDetails(false);

        // Check for expected number of entries
        Assertions.assertEquals(
            1,
            store.getConfigEntries().size(),
            "Configuration entries did not match expected number!"
        );

        // Check a sample of the configuration entry's content
        Assertions.assertEquals(
            "https://theworldavatar.io/mock-domain/ClassOne",
            store.getConfigEntries().get(0).getClassIRI(),
            "Class IRI for first entry did not match expected result!"
        );
        Assertions.assertEquals(
            "Hello world",
            store.getConfigEntries().get(0).getMetaQueryContent(),
            "Metadata query for second entry did not match expected result!"
        );
        Assertions.assertEquals(
            TimeUnit.HOURS,
            store.getConfigEntries().get(0).getTimeLimitUnit(),
            "Unit for time series limit did not meet expected result!"
        );
        Assertions.assertEquals(
            TimeReference.LATEST,
            store.getConfigEntries().get(0).getTimeReference(),
            "Reference type for time series limit did not meet expected result!"
        );
    }

    /**
     * Tests the ability to clear and reload the configuration.
     */
    @Test
    public void testReload() throws Exception {
        Path configFile = TEMP_DIR.resolve("mock-config-01\\config.json");
        
        // Create store instance (skipping stack integration)
        ConfigStore store = new ConfigStore(configFile.toString());
        store.loadDetails(false);

        // Add new ConfigEntry instances
        ConfigEntryBuilder builder = new ConfigEntryBuilder(configFile.getParent());
        
        store.getConfigEntries().add(builder.build(
            "new-entry-01",
            "https://theworldavatar.io/mock-domain/ClassOne",
            "classOneMeta.sparql"
        ));

        // Assert that new entry added
        Assertions.assertEquals(
            5,
            store.getConfigEntries().size(),
            "Number of configuration entries not as expected after generating a new entry!"
        );

        // Reload configuration
        store.loadDetails(false);

        // Assert back to original number of entries
        Assertions.assertEquals(
            4,
            store.getConfigEntries().size(),
            "Number of configuration entries not as expected after refresh!"
        );
    }
    
}
// End of class.