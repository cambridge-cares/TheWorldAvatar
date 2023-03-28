package uk.ac.cam.cares.jps;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

class ConfigStoreTest {
    private static final String originSparql = "http://localhost:9999/blazegraph/namespace/test/sparql";
    private static final String destinationSparql = "http://host.docker.internal:9999/blazegraph/namespace/target/sparql";

    @Test
    void testRetrieveSPARQLConfigNoFile() {
        // Verify right exception is thrown
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, ConfigStore::retrieveSPARQLConfig);
        assertEquals("No endpoint.properties file detected! Please place the file in the config directory.", thrownError.getMessage());
    }

    @Test
    void testRetrieveSPARQLConfigMissingInputs() throws IOException {
        File config = TestConfigUtils.genSampleConfigFile(false, originSparql, destinationSparql);
        try {
            // Execute method and ensure right error is thrown
            JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, ConfigStore::retrieveSPARQLConfig);
            assertEquals("Missing Properties:\n" +
                    "sparql.destination.endpoint is missing! Please add the input to endpoint.properties.\n", thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveSPARQLConfig() throws IOException {
        File config = TestConfigUtils.genSampleConfigFile(true, originSparql, destinationSparql);
        try {
            // Execute method
            String[] result = ConfigStore.retrieveSPARQLConfig();
            // Verify results are expected
            assertEquals(originSparql, result[0]);
            assertEquals(destinationSparql, result[1]);
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }
}