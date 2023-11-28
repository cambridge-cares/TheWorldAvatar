package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.nio.file.*;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IOHelperTest {
    private final String testString = "test string";

    @Test
    void testCreateTempFile() {
        Path file = IOHelper.createTempFile();
        assertTrue(Files.exists(file));
        // Always delete the temporary file generated
        IOHelper.deleteTempFile(file);
    }

    @Test
    void testReadFile() {
        Path file = IOHelper.createTempFile();
        // If the function did not work, there would be an exception rather than an empty list
        List<String> result = IOHelper.readFile(file);
        // As no text is written, the result should be empty
        assertTrue(result.isEmpty());
        // Always delete the temporary file generated
        IOHelper.deleteTempFile(file);
    }

    @Test
    void testReadFileFail() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> IOHelper.readFile(Paths.get("")));
        assertTrue(thrownError.getMessage().contains(" cannot be accessed or opened!"));
    }

    @Test
    void testWriteLinesToFile() {
        Path file = IOHelper.createTempFile();
        IOHelper.writeLinesToFile(file, testString);
        List<String> result = IOHelper.readFile(file);
        assertEquals(testString, result.get(0));
        // Always delete the temporary file generated
        IOHelper.deleteTempFile(file);
    }

    @Test
    void testReplaceTargetFileWithSource() {
        Path src = IOHelper.createTempFile();
        Path tgt = IOHelper.createTempFile();
        IOHelper.writeLinesToFile(src, testString);
        IOHelper.replaceTargetFileWithSource(src, tgt);

        // Test outcomes
        List<String> result = IOHelper.readFile(tgt);
        assertFalse(Files.exists(src)); // Src file should be deleted in the function
        assertEquals(testString, result.get(0)); // Tgt file should have src file's contents
        IOHelper.deleteTempFile(tgt); // Delete the remaining file
    }

    @Test
    void testDeleteTempFile() {
        Path file = IOHelper.createTempFile();
        IOHelper.deleteTempFile(file);
        assertFalse(Files.exists(file));
    }

    @Test
    void testDeleteTempFileFail() {
        assertThrows(JPSRuntimeException.class, () -> IOHelper.deleteTempFile(Paths.get("")));
    }
}
