package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.*;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.*;

public class TSPropertiesHandlerTest {
    @TempDir
    private static Path tempDir;
    private static Map<String, List<?>> testMap;
    private Map<String, String> testIRIMappings;
    private TSPropertiesHandler testHandler;

    @BeforeAll
    static void initTestMap() {
        testMap = new HashMap<>();
        List<LocalDate> dateList = new ArrayList<>(
                Arrays.asList(LocalDate.parse("2022-10-15"), LocalDate.parse("2022-11-15")));
        List<Double> sampleList = new ArrayList<>(Arrays.asList(5.0, 15.0));
        testMap.put("dates", dateList);
        // Duplicate values does not matter as the test is more concerned with the keys
        testMap.put("headingone", sampleList);
        testMap.put("headingtwo", sampleList);
    }

    @Test
    @Order(0)
    void testTSPropertiesHandlerConstructor() throws IOException {
        assertNotNull(new TSPropertiesHandler(testMap, "dates"), "Standard constructor failed to be constructed");
    }

    @Test
    @Order(1)
    void testTSPropertiesHandlerConstructorException() {
        Map<String, List<?>> testEmptyMap=new HashMap<>();
        IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, () -> new TSPropertiesHandler(testEmptyMap, "dates"), "IllegalArgumentException was expected");
        assertEquals("Readings can not be empty!", thrown.getMessage());
    }

    @Test
    @Order(2)
    @DisplayName("Test when there is no file, and a new file will be generated")
    void testGenerateIRIMappingsNoFile() throws IOException {
        testHandler = new TSPropertiesHandler(testMap, "dates");
        Path propertiesPath = tempDir.resolve("excel.properties");
        testIRIMappings = testHandler.generateIRIMappings(propertiesPath.toString());
        assertAll(
                () -> assertTrue(Files.exists(propertiesPath)),
                () -> assertTrue(testIRIMappings.containsKey("headingone")),
                () -> assertTrue(testIRIMappings.containsKey("headingtwo")),
                () -> assertFalse(testIRIMappings.containsKey("dates")),
                () -> assertTrue(Pattern.matches(".*headingone_(.{36})$", testIRIMappings.get("headingone"))),
                () -> assertTrue(Pattern.matches(".*headingtwo_(.{36})$", testIRIMappings.get("headingtwo")))
        );
    }
    @Test
    @Order(3)
    @DisplayName("Test when there is missing headers in the existing file, and regenerates a new file")
    void testGenerateIRIMappingsFileWithMissingHeaders() throws IOException {
        testHandler = new TSPropertiesHandler(testMap, "dates");
        Path propertiesPath = tempDir.resolve("excel.properties");
        // Generate a properties file that is missing header two
        Properties prop = new Properties();
        prop.setProperty("headingone", "testiri");
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }

        assertTrue(Files.exists(propertiesPath));
        testIRIMappings = testHandler.generateIRIMappings(propertiesPath.toString());
        try (InputStream input = Files.newInputStream(propertiesPath)) {
            prop.load(input);
            assertAll(
                    // check iriMappings
                    () -> assertTrue(testIRIMappings.containsKey("headingone")),
                    () -> assertTrue(testIRIMappings.containsKey("headingtwo")),
                    () -> assertFalse(testIRIMappings.containsKey("dates")),
                    () -> assertNotEquals("testiri", testIRIMappings.get("headingone")), // previous content is gone
                    () -> assertTrue(Pattern.matches(".*headingone_(.{36})$", testIRIMappings.get("headingone"))),
                    () -> assertTrue(Pattern.matches(".*headingtwo_(.{36})$", testIRIMappings.get("headingtwo"))),
                    // Check properties file
                    () -> assertTrue(Pattern.matches(".*headingone_(.{36})$", prop.getProperty("headingone"))),
                    () -> assertTrue(Pattern.matches(".*headingtwo_(.{36})$", prop.getProperty("headingtwo")))
            );
        }
    }
    @Test
    @Order(4)
    @DisplayName("Test when there is missing IRIs in the existing file, and generates those IRIs")
    void testGenerateIRIMappingsFileEmptyIRI() throws IOException {
        testHandler = new TSPropertiesHandler(testMap, "dates");
        Path propertiesPath = tempDir.resolve("excel.properties");
        // Generate a properties file that has one empty iri
        Properties prop = new Properties();
        prop.setProperty("headingone", "testiri");
        prop.setProperty("headingtwo", "");
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }

        assertTrue(Files.exists(propertiesPath));
        testIRIMappings = testHandler.generateIRIMappings(propertiesPath.toString());
        try (InputStream input = Files.newInputStream(propertiesPath)) {
            prop.load(input);
            assertAll(
                    // check iriMappings
                    () -> assertTrue(testIRIMappings.containsKey("headingone")),
                    () -> assertTrue(testIRIMappings.containsKey("headingtwo")),
                    () -> assertFalse(testIRIMappings.containsKey("dates")),
                    () -> assertEquals("testiri", testIRIMappings.get("headingone")), // previous content is gone
                    () -> assertTrue(Pattern.matches(".*headingtwo_(.{36})$", testIRIMappings.get("headingtwo"))),
                    // Check properties file
                    () -> assertEquals("testiri", prop.getProperty("headingone")),
                    () -> assertNotEquals("", prop.getProperty("headingtwo")),
                    () -> assertTrue(Pattern.matches(".*headingtwo_(.{36})$", prop.getProperty("headingtwo")))
            );
        }
    }

}
