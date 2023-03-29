package uk.ac.cam.cares.jps;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

class ConfigStoreTest {
    private static final String originSparql = "http://localhost:9999/blazegraph/namespace/test/sparql";
    private static final String destinationSparql = "http://host.docker.internal:9999/blazegraph/namespace/target/sparql";
    private static final String srcDb = "jdbc:postgresql://localhost:5432/db";
    private static final String srcUser = "postgres";
    private static final String srcPass = "pass1";
    private static final String tgtDb = "jdbc:postgresql://host.docker.internal:5432/db";
    private static final String tgtUser = "user";
    private static final String tgtPass = "pass2";

    @Test
    void testRetrieveSPARQLConfigNoFile() {
        // Verify right exception is thrown
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, ConfigStore::retrieveSPARQLConfig);
        assertEquals("No endpoint.properties file detected! Please place the file in the config directory.", thrownError.getMessage());
    }

    @Test
    void testRetrieveSPARQLConfigMissingInputs() throws IOException {
        File config = TestConfigUtils.genSampleSPARQLConfigFile(false, originSparql, destinationSparql);
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
        File config = TestConfigUtils.genSampleSPARQLConfigFile(true, originSparql, destinationSparql);
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

    @Test
    void testRetrieveSQLConfigNoFile() {
        // Verify right exception is thrown
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, ConfigStore::retrieveSQLConfig);
        assertEquals("No endpoint.properties file detected! Please place the file in the config directory.", thrownError.getMessage());
    }

    @Test
    void testRetrieveSQLConfigMissingInputs() throws IOException {
        File config = TestConfigUtils.genSampleSQLConfigFile(false, srcDb, srcUser, srcPass, tgtDb, tgtUser, tgtPass);
        try {
            // Execute method and ensure right error is thrown
            JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, ConfigStore::retrieveSQLConfig);
            assertEquals("Missing Properties:\n" +
                    "src.db.user is missing! Please add the input to endpoint.properties.\n"+
                    "src.db.password is missing! Please add the input to endpoint.properties.\n", thrownError.getMessage());
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }

    @Test
    void testRetrieveSQLConfig() throws IOException {
        File config = TestConfigUtils.genSampleSQLConfigFile(true, srcDb, srcUser, srcPass, tgtDb, tgtUser, tgtPass);
        try {
            // Execute method
            String[] result = ConfigStore.retrieveSQLConfig();
            // Verify results are expected
            assertEquals(srcDb, result[0]);
            assertEquals(srcUser, result[1]);
            assertEquals(srcPass, result[2]);
            assertEquals(tgtDb, result[3]);
            assertEquals(tgtUser, result[4]);
            assertEquals(tgtPass, result[5]);
        } finally {
            // Always delete generated config file
            config.delete();
        }
    }
}