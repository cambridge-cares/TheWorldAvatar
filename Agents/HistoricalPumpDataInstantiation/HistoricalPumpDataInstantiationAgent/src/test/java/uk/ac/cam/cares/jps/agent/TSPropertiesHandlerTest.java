package uk.ac.cam.cares.jps.agent;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.*;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.*;

class TSPropertiesHandlerTest {
    @TempDir
    private static Path tempDir;
    private Map<String, Map<String, String>> testIRIMappings;
    private TSPropertiesHandler testHandler;
    private TSPropertiesHandler testGroupHandler;
    private static Map<String, Map<String, List<?>>> sampleReadings;
    private static Map<String, Map<String, List<?>>> sampleGroupReadings;
    private static final Instant INSTANT_1 = Instant.parse("2022-12-07T07:50:12.732616Z");
    private static final Instant INSTANT_2 = Instant.parse("2022-12-08T07:10:12.190461Z");
    private static final Instant INSTANT_3 = Instant.parse("2022-12-09T07:28:12.19055Z");
    private static final String BASE_KEY = "base";
    private static final String YEAR_KEY = "year";
    private static final String PROP_FILE = "excel.properties";
    private static final String HEADING_ONE = "headingone";
    private static final String HEADING_TWO = "headingtwo";
    private static final String TESTIRI = "testIri";
    private static final String[] GROUP_ARR = {"building", "station"};
    private static final String UNDERSCORE = "_";



    @BeforeAll
    static void initSampleReadings() {
        // Set up nested map and their values
        Map<String, List<?>> nestedMap = new HashMap<>();
        List<Instant> instantList = new ArrayList<>(Arrays.asList(INSTANT_1, INSTANT_2, INSTANT_3));
        List<Double> sampleList = new ArrayList<>(Arrays.asList(5.0, 15.0));
        nestedMap.put(YEAR_KEY, instantList);
        // Duplicate values does not matter as the test is more concerned with the IRI generated
        nestedMap.put(HEADING_ONE, sampleList);
        nestedMap.put(HEADING_TWO, sampleList);
        // Add to readings for one time series
        sampleReadings = new HashMap<>();
        sampleReadings.put(BASE_KEY, nestedMap);
        // Add to readings for multiple time series
        sampleGroupReadings = new HashMap<>();
        sampleGroupReadings.put(GROUP_ARR[0],nestedMap);
        sampleGroupReadings.put(GROUP_ARR[1],nestedMap);
    }

    @BeforeEach
    void init() throws IOException {
        testHandler = new TSPropertiesHandler(sampleReadings, YEAR_KEY);
        testGroupHandler = new TSPropertiesHandler(sampleGroupReadings, YEAR_KEY);
    }

    @Test
    void testTSPropertiesHandlerConstructor() throws IOException {
        assertNotNull(new TSPropertiesHandler(sampleReadings, YEAR_KEY), "Standard constructor failed to be constructed");
    }

    @Test
    void testTSPropertiesHandlerConstructorFail() {
        Map<String, Map<String, List<?>>> testEmptyMap = new HashMap<>();
        IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, () -> new TSPropertiesHandler(testEmptyMap, YEAR_KEY), "IllegalArgumentException was expected");
        assertEquals("Readings can not be empty!", thrown.getMessage());
    }

    @Test
    void testGenerateIRIMappingsNoFile() throws IOException {
        Path propertiesPath = tempDir.resolve(PROP_FILE);
        // Check there is no preceding mapping file
        assertFalse(Files.exists(propertiesPath));
        // Execute method
        testIRIMappings = testHandler.generateIRIMappings(propertiesPath.toString());
        try {
            // Check properties have been generated
            assertTrue(Files.exists(propertiesPath));
            // Check IRI mappings generated have the following keys
            assertTrue(testIRIMappings.get(BASE_KEY).containsKey(HEADING_ONE));
            assertTrue(testIRIMappings.get(BASE_KEY).containsKey(HEADING_TWO));
            assertFalse(testIRIMappings.get(BASE_KEY).containsKey(YEAR_KEY));
            // Check IRI mappings generated have the following values
            assertTrue(Pattern.matches(".*headingone_(.{36})$", testIRIMappings.get(BASE_KEY).get(HEADING_ONE)));
            assertTrue(Pattern.matches(".*headingtwo_(.{36})$", testIRIMappings.get(BASE_KEY).get(HEADING_TWO)));
        } finally {
            new File(propertiesPath.toString()).delete();
        }
    }

    @Test
    void testGenerateIRIMappingsFileWithMissingHeaders() throws IOException {
        Path propertiesPath = tempDir.resolve(PROP_FILE);
        // Generate a properties file that is missing header two
        Properties prop = new Properties();
        prop.setProperty(HEADING_ONE, TESTIRI);
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }
        // Check properties file exist
        assertTrue(Files.exists(propertiesPath));
        //  Execute method
        testIRIMappings = testHandler.generateIRIMappings(propertiesPath.toString());
        try {
            // Verify the key and values
            assertTrue(testIRIMappings.get(BASE_KEY).containsKey(HEADING_ONE));
            assertTrue(testIRIMappings.get(BASE_KEY).containsKey(HEADING_TWO));
            assertFalse(testIRIMappings.get(BASE_KEY).containsKey(YEAR_KEY));
            assertNotEquals(TESTIRI, testIRIMappings.get(BASE_KEY).get(HEADING_ONE)); // Check previous content is updated
            assertTrue(Pattern.matches(".*headingone_(.{36})$", testIRIMappings.get(BASE_KEY).get(HEADING_ONE)));
            assertTrue(Pattern.matches(".*headingtwo_(.{36})$", testIRIMappings.get(BASE_KEY).get(HEADING_TWO)));
            // Verify properties file
            try (InputStream input = Files.newInputStream(propertiesPath)) {
                prop.load(input);
                assertTrue(Pattern.matches(".*headingone_(.{36})$", prop.getProperty(HEADING_ONE)));
                assertTrue(Pattern.matches(".*headingtwo_(.{36})$", prop.getProperty(HEADING_TWO)));
            }
        } finally {
            new File(propertiesPath.toString()).delete();
        }
    }

    @Test
    void testGenerateIRIMappingsFileEmptyIRI() throws IOException {
        Path propertiesPath = tempDir.resolve(PROP_FILE);
        // Generate a properties file that has one empty iri
        Properties prop = new Properties();
        prop.setProperty(HEADING_ONE, TESTIRI);
        prop.setProperty(HEADING_TWO, "");
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }
        // Check properties file exist
        assertTrue(Files.exists(propertiesPath));
        //  Execute method
        testIRIMappings = testHandler.generateIRIMappings(propertiesPath.toString());
        try {
            // Verify the key and values
            assertTrue(testIRIMappings.get(BASE_KEY).containsKey(HEADING_ONE));
            assertTrue(testIRIMappings.get(BASE_KEY).containsKey(HEADING_TWO));
            assertFalse(testIRIMappings.get(BASE_KEY).containsKey(YEAR_KEY));
            assertEquals(TESTIRI, testIRIMappings.get(BASE_KEY).get(HEADING_ONE)); // Check HEADING_ONE content remains
            assertTrue(Pattern.matches(".*headingtwo_(.{36})$", testIRIMappings.get(BASE_KEY).get(HEADING_TWO)));
            // Verify properties file
            try (InputStream input = Files.newInputStream(propertiesPath)) {
                prop.load(input);
                assertEquals(TESTIRI, prop.getProperty(HEADING_ONE));
                assertNotEquals("", prop.getProperty(HEADING_TWO));
                assertTrue(Pattern.matches(".*headingtwo_(.{36})$", prop.getProperty(HEADING_TWO)));
            }
        } finally {
            new File(propertiesPath.toString()).delete();
        }
    }

    @Test
    void testGenerateIRIMappingsNoFileGroupTS() throws IOException {
        Path propertiesPath = tempDir.resolve(PROP_FILE);
        // Check there is no preceding mapping file
        assertFalse(Files.exists(propertiesPath));
        // Execute method
        testIRIMappings = testGroupHandler.generateIRIMappings(propertiesPath.toString());
        try {
            // Check properties have been generated
            assertTrue(Files.exists(propertiesPath));
            // Check IRI mappings generated have the following keys
            // Group 1
            assertTrue(testIRIMappings.get(GROUP_ARR[0]).containsKey(HEADING_ONE));
            assertTrue(testIRIMappings.get(GROUP_ARR[0]).containsKey(HEADING_TWO));
            assertFalse(testIRIMappings.get(GROUP_ARR[0]).containsKey(YEAR_KEY)); // This key was removed
            // Group 2
            assertTrue(testIRIMappings.get(GROUP_ARR[1]).containsKey(HEADING_ONE));
            assertTrue(testIRIMappings.get(GROUP_ARR[1]).containsKey(HEADING_TWO));
            assertFalse(testIRIMappings.get(GROUP_ARR[1]).containsKey(YEAR_KEY)); // This key was removed
            // Check IRI mappings generated have the following values
            // Group 1
            assertTrue(Pattern.matches(".*building_headingone_(.{36})$", testIRIMappings.get(GROUP_ARR[0]).get(HEADING_ONE)));
            assertTrue(Pattern.matches(".*building_headingtwo_(.{36})$", testIRIMappings.get(GROUP_ARR[0]).get(HEADING_TWO)));
            // Group 2
            assertTrue(Pattern.matches(".*station_headingone_(.{36})$", testIRIMappings.get(GROUP_ARR[1]).get(HEADING_ONE)));
            assertTrue(Pattern.matches(".*station_headingtwo_(.{36})$", testIRIMappings.get(GROUP_ARR[1]).get(HEADING_TWO)));
        } finally {
            new File(propertiesPath.toString()).delete();
        }
    }

    @Test
    void testGenerateIRIMappingsFileWithMissingHeadersGroupTS() throws IOException {
        Path propertiesPath = tempDir.resolve(PROP_FILE);
        // Generate a properties file that is missing header two
        Properties prop = new Properties();
        prop.setProperty(GROUP_ARR[0] + UNDERSCORE + HEADING_ONE, TESTIRI);
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }
        // Check properties file exist
        assertTrue(Files.exists(propertiesPath));
        //  Execute method
        testIRIMappings = testGroupHandler.generateIRIMappings(propertiesPath.toString());
        try {
            // Check IRI mappings generated have the following keys
            // Group 1
            assertTrue(testIRIMappings.get(GROUP_ARR[0]).containsKey(HEADING_ONE));
            assertTrue(testIRIMappings.get(GROUP_ARR[0]).containsKey(HEADING_TWO));
            assertFalse(testIRIMappings.get(GROUP_ARR[0]).containsKey(YEAR_KEY)); // This key was removed
            // Group 2
            assertTrue(testIRIMappings.get(GROUP_ARR[1]).containsKey(HEADING_ONE));
            assertTrue(testIRIMappings.get(GROUP_ARR[1]).containsKey(HEADING_TWO));
            assertFalse(testIRIMappings.get(GROUP_ARR[1]).containsKey(YEAR_KEY)); // This key was removed
            // Check IRI mappings generated have the following values
            // Group 1
            assertTrue(Pattern.matches(".*building_headingone_(.{36})$", testIRIMappings.get(GROUP_ARR[0]).get(HEADING_ONE)));
            assertTrue(Pattern.matches(".*building_headingtwo_(.{36})$", testIRIMappings.get(GROUP_ARR[0]).get(HEADING_TWO)));
            // Group 2
            assertTrue(Pattern.matches(".*station_headingone_(.{36})$", testIRIMappings.get(GROUP_ARR[1]).get(HEADING_ONE)));
            assertTrue(Pattern.matches(".*station_headingtwo_(.{36})$", testIRIMappings.get(GROUP_ARR[1]).get(HEADING_TWO)));
            // Verify properties file
            try (InputStream input = Files.newInputStream(propertiesPath)) {
                prop.load(input);
                assertTrue(Pattern.matches(".*building_headingone_(.{36})$", prop.getProperty(GROUP_ARR[0] + UNDERSCORE + HEADING_ONE)));
                assertTrue(Pattern.matches(".*building_headingtwo_(.{36})$", prop.getProperty(GROUP_ARR[0] + UNDERSCORE +HEADING_TWO)));
                assertTrue(Pattern.matches(".*station_headingone_(.{36})$", prop.getProperty(GROUP_ARR[1] + UNDERSCORE + HEADING_ONE)));
                assertTrue(Pattern.matches(".*station_headingtwo_(.{36})$", prop.getProperty(GROUP_ARR[1] + UNDERSCORE +HEADING_TWO)));
            }
        } finally {
            new File(propertiesPath.toString()).delete();
        }
    }

    @Test
    void testGenerateIRIMappingsFileEmptyIRIGroupTS() throws IOException {
        Path propertiesPath = tempDir.resolve(PROP_FILE);
        // Generate a properties file that has one empty iri
        Properties prop = new Properties();
        prop.setProperty(GROUP_ARR[0] + UNDERSCORE +HEADING_ONE, TESTIRI);
        prop.setProperty(GROUP_ARR[0] + UNDERSCORE +HEADING_TWO, "");
        prop.setProperty(GROUP_ARR[1] + UNDERSCORE +HEADING_ONE, "");
        prop.setProperty(GROUP_ARR[1] + UNDERSCORE +HEADING_TWO, "");
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }
        // Check properties file exist
        assertTrue(Files.exists(propertiesPath));
        //  Execute method
        testIRIMappings = testGroupHandler.generateIRIMappings(propertiesPath.toString());
        try {
            // Check IRI mappings generated have the following keys
            // Group 1
            assertTrue(testIRIMappings.get(GROUP_ARR[0]).containsKey(HEADING_ONE));
            assertTrue(testIRIMappings.get(GROUP_ARR[0]).containsKey(HEADING_TWO));
            assertFalse(testIRIMappings.get(GROUP_ARR[0]).containsKey(YEAR_KEY)); // This key was removed
            assertEquals(TESTIRI, testIRIMappings.get(GROUP_ARR[0]).get(HEADING_ONE)); // Check HEADING_ONE content remains
            assertTrue(Pattern.matches(".*building_headingtwo_(.{36})$", testIRIMappings.get(GROUP_ARR[0]).get(HEADING_TWO)));
            // Verify properties file
            try (InputStream input = Files.newInputStream(propertiesPath)) {
                prop.load(input);
                assertEquals(TESTIRI, prop.getProperty(GROUP_ARR[0] + UNDERSCORE +HEADING_ONE));
                assertNotEquals("", prop.getProperty(GROUP_ARR[0] + UNDERSCORE + HEADING_TWO));
                assertTrue(Pattern.matches(".*building_headingtwo_(.{36})$", prop.getProperty(GROUP_ARR[0] + UNDERSCORE +HEADING_TWO)));
                assertNotEquals("", prop.getProperty(GROUP_ARR[1] + UNDERSCORE + HEADING_ONE));
                assertTrue(Pattern.matches(".*station_headingone_(.{36})$", prop.getProperty(GROUP_ARR[1] + UNDERSCORE + HEADING_ONE)));
                assertNotEquals("", prop.getProperty(GROUP_ARR[1] + UNDERSCORE + HEADING_TWO));
                assertTrue(Pattern.matches(".*station_headingtwo_(.{36})$", prop.getProperty(GROUP_ARR[1] + UNDERSCORE +HEADING_TWO)));
            }
        } finally {
            new File(propertiesPath.toString()).delete();
        }
    }
}