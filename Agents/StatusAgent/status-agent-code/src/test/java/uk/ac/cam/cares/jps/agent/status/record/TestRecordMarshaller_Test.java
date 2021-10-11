
package uk.ac.cam.cares.jps.agent.status.record;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.Test;

/**
 *
 * @author Michael
 */
public class TestRecordMarshaller_Test {
    
    /**
     * 
     */
    @Test
    public void testReading() {
        Path file = Paths.get("test-data", "test-records.json");
        assertTrue(Files.exists(file), "Could not find sample 'test-records.json' file!");
        
        // Read the sample record store
        TestRecordStore store = TestRecordStoreMarshaller.readRecords(file);
        assertNotNull(store, "Could not read sample 'test-records.json' file!");
        assertEquals(3, store.getRecords().size(), "Expected 3 sample TestRecord instances!");
    }
}
// End of class.