package uk.ac.cam.cares.jps.agent.status;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.status.define.TestType;

/**
 *
 * @author Michael
 */
public class TestRegistry_Test {

    /**
     * Reads a sample registry file and checks that the number of expected Test Definitions are
     * created and stored.
     */
    @Test
    public void test() {
        Path registryFile = Paths.get("test-data", "test-registry.json");
        assertTrue(Files.exists(registryFile), "Could not find sample 'test-registry.json' file!");

        // Read sample registry
        TestRegistry.readRegistryFile(registryFile);

        // Check against expected counts
        assertEquals(7, TestRegistry.getDefinedTests().size(), "Expected 7 test definitions!");
        assertEquals(5, TestRegistry.getDefinedTests(TestType.AVAILABILITY).size(), "Expected 5 AVAILABILITY test definitions!");
        assertEquals(2, TestRegistry.getDefinedTests(TestType.QUERY_COUNT).size(), "Expected 2 QUERY_COUNT test definitions!");
    }

}
// End of class.