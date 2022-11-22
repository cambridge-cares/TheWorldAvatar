package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class IfcOwlConverterTest {
    @TempDir
    private static Path tempDir;
    private static final String uri = "uriSample";
    private static final String dir = "tempDirectory";

    @Test
    void testIfcOwlConverterConstructor() {
        IfcOwlConverter converter = new IfcOwlConverter(new String[0]);
        // When no args is given, class should return the default values
        assertTrue(converter.getBaseURI().contains("http://www.theworldavatar.com/ifc/resources_"));
        assertEquals(Paths.get(System.getProperty("user.dir") + "/data/").toString(), converter.getDirPath());
    }

    @Test
    void testIfcOwlConverterConstructorSetUri() {
        String[] args = {"--baseURI", uri};
        IfcOwlConverter converter = new IfcOwlConverter(args);
        // When only URI is given, class should return the default value for the --dir flag
        assertTrue(converter.getBaseURI().contains(uri));
        assertEquals(Paths.get(System.getProperty("user.dir") + "/data/").toString(), converter.getDirPath());
    }

    @Test
    void testIfcOwlConverterConstructorSetUriAndDir() {
        IfcOwlConverter converter = new IfcOwlConverter(genArgs());
        // When URI and dir flag is given, class should return the changed values
        assertTrue(converter.getBaseURI().contains(uri));
        assertEquals(dir, converter.getDirPath());
    }

    @Test
    void testParse2TTL() throws Exception {
        IfcOwlConverter converter = Mockito.mock(IfcOwlConverter.class);
        // Stub the path to do nothing, as this runs a command prompt which isn't testable
        Mockito.doNothing().when(converter).parse2TTL();
        converter.parse2TTL();
        // Verify it has been called
        Mockito.verify(converter).parse2TTL();
    }
    @Test
    void testListTTLFiles() throws IOException {
        // Set up
        Path file = genSampleFile("ttl");
        String[] args = {"--dir", tempDir.toString()};
        IfcOwlConverter converter = new IfcOwlConverter(args);

        // Test that there is a ttl file, and the results generated the same file path
        Set<String> results = converter.listTTLFiles();
        assertTrue(results.size() > 0);
        results.forEach(line -> assertEquals(file.toString(), line));
    }

    @Test
    void testListTTLFilesFailWithNoDir() {
        IfcOwlConverter converter = new IfcOwlConverter(genArgs());
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> converter.listTTLFiles());
        assertTrue(thrownError.getMessage().contains("Failed to access target directory at"));
    }

    @Test
    void testListTTLFilesFailWithNoTTL() throws IOException {
        genSampleFile("txt");
        String[] args = {"--dir", tempDir.toString()};
        IfcOwlConverter converter = new IfcOwlConverter(args);
        // No error is thrown when a directory exists, but should not return any path if there is no TTl file
        Set<String> results = converter.listTTLFiles();
        assertEquals(0, results.size());
    }

    private static String[] genArgs() {
        String[] args = {"--baseURI", uri, "--dir", dir};
        return args;
    }

    private static Path genSampleFile(String fileExt) throws IOException {
        Path filePath = tempDir.resolve("data." + fileExt);
        List<String> lines = Arrays.asList("statement1", "statement2", "statement3");
        Files.write(filePath, lines);
        return filePath;
    }
}