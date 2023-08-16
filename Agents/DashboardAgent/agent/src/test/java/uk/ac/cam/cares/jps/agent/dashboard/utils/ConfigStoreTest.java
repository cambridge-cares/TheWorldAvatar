package uk.ac.cam.cares.jps.agent.dashboard.utils;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

class ConfigStoreTest {
    private static final String DATABASE_USER = "admin";
    private static final String DATABASE_PASSWORD = "pass";

    @Test
    void testRetrieveCredentialsNoFile() {
        // Verify right exception is thrown
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, ConfigStore::retrieveCredentials);
        assertEquals("No credentials.properties file detected! Please place the file in the config directory.", thrownError.getMessage());
    }
    @Test
    void testRetrieveCredentialsMissingPassword() throws IOException {
        File config = TestUtils.genSampleCredFile(false, DATABASE_USER, DATABASE_PASSWORD);
        try {
            // Execute method and ensure right error is thrown
            JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, ConfigStore::retrieveCredentials);
            assertEquals("Missing Properties:\n" +
                    "dashboard.pass is missing! Please add the input to credentials.properties.\n", thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveCredentials() throws IOException {
        File config = TestUtils.genSampleCredFile(true, DATABASE_USER, DATABASE_PASSWORD);
        try {
            // Execute method
            String[] result = ConfigStore.retrieveCredentials();
            // Verify results are expected
            assertEquals(DATABASE_USER, result[0]);
            assertEquals(DATABASE_PASSWORD, result[1]);
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

}