package uk.ac.cam.cares.jps.agent;

import org.apache.commons.io.FileUtils;
import org.json.JSONObject;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.agent.sparql.SparqlAdapter;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;


class HistoricalPumpDataInstantiationAgentTest {
    @TempDir
    private static Path tempDir;
    private static String tempDataDir;
    private static String tempConfigDir;
    private static String[] args;
    private static final String KEY_TIMEHEADER = "timeHeader";
    private static final String KEY_IRI_PREFIX = "iriPrefix";
    private static final String KEY_ADD_TRIPLE = "addTriple";
    private static final String KEY_STARTING_ROW = "startingRow";
    private static final String KEY_MULTI_TS_COL_INDEX = "multiTSColIndex";
    private static final String VALUE_TIMEHEADER = "Year";
    private static final String VALUE_IRI_PREFIX_NS = "namespace/";
    private static final String VALUE_ADD_TRIPLE = "False";
    private static final String VALUE_IRI_PREFIX_IRI = "https://www.example.org/kb/namespace/";
    private static final String VALUE_STARTING_ROW = "2";
    private static final String VALUE_MULTI_TS_COL_INDEX = "2";
    private static final String REQUEST_PARAMS_ERROR = "Request parameters are not defined correctly.";

    @BeforeAll
    static void init() {
        // Note that the current working directory follows the pom.xml location
        // For testing, we will create new temp directories to ensure the method is running properly
        // Set up the required directory path as a string
        tempDataDir = System.getProperty("user.dir") + "/data/";
        tempConfigDir = System.getProperty("user.dir") + "/config/";
    }

    @BeforeEach
    void setup() throws IOException {
        // Create temp directories before each running tests
        Files.createDirectories(Paths.get(tempDataDir));
        Files.createDirectories(Paths.get(tempConfigDir));
        args = new String[]{VALUE_TIMEHEADER, VALUE_ADD_TRIPLE, "1", VALUE_MULTI_TS_COL_INDEX};
        HistoricalPumpDataInstantiationAgent agent = new HistoricalPumpDataInstantiationAgent();
    }

    @AfterEach
    void cleanTempDir() throws IOException {
        File tempDir = new File(tempDataDir);
        FileUtils.cleanDirectory(tempDir);
        tempDir = new File(tempConfigDir);
        FileUtils.cleanDirectory(tempDir);
    }

    @AfterAll
    static void removeTempDir() throws IOException {
        File tempDir = new File(tempDataDir);
        tempDir.delete();
        tempDir = new File(tempConfigDir);
        tempDir.delete();
    }

    @Test
    void testValidateInputEmptyParams() {
        JSONObject json = new JSONObject();
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
    }

    @Test
    void testValidateInputMissingMandatoryParams() {
        JSONObject json = new JSONObject();
        json.put(KEY_TIMEHEADER, VALUE_TIMEHEADER);
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));

        json = new JSONObject();
        json.put(KEY_IRI_PREFIX, VALUE_IRI_PREFIX_NS);
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
    }

    @Test
    void testValidateInputInvalidIRIPrefixParams() {
        JSONObject json = new JSONObject();
        json.put(KEY_TIMEHEADER, VALUE_TIMEHEADER);
        // No ending / for namespace
        json.put(KEY_IRI_PREFIX, "namespace");
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
        // Starting with / for namespace
        json.put(KEY_IRI_PREFIX, "/namespace/");
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
        // Does not start with http://
        json.put(KEY_IRI_PREFIX, "www.example.org/test/namespace/");
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
        // Does not end in /
        json.put(KEY_IRI_PREFIX, "http://www.example.org/test/namespace");
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
    }

    @Test
    void testValidateInputInvalidAddTripleParam() {
        JSONObject json = new JSONObject();
        json.put(KEY_TIMEHEADER, VALUE_TIMEHEADER);
        json.put(KEY_IRI_PREFIX, VALUE_IRI_PREFIX_NS);
        json.put(KEY_ADD_TRIPLE, "fal se");
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
    }

    @Test
    void testValidateInputMandatoryParams() {
        JSONObject json = new JSONObject();
        json.put(KEY_TIMEHEADER, VALUE_TIMEHEADER);
        json.put(KEY_ADD_TRIPLE, VALUE_ADD_TRIPLE);
        // Test first variation of valid IRI prefix
        json.put(KEY_IRI_PREFIX, VALUE_IRI_PREFIX_NS);
        assertTrue(new HistoricalPumpDataInstantiationAgent().validateInput(json));
        // Test second variation of valid IRI prefix
        json.put(KEY_IRI_PREFIX, VALUE_IRI_PREFIX_IRI);
        assertTrue(new HistoricalPumpDataInstantiationAgent().validateInput(json));
    }

    @Test
    void testValidateInputInvalidOptionalStartingRowParam() {
        JSONObject json = new JSONObject();
        json.put(KEY_TIMEHEADER, VALUE_TIMEHEADER);
        json.put(KEY_IRI_PREFIX, VALUE_IRI_PREFIX_NS);
        json.put(KEY_ADD_TRIPLE, VALUE_ADD_TRIPLE);
        json.put(KEY_STARTING_ROW, "fail");
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
    }

    @Test
    void testValidateInputInvalidOptionalMultiTSColParam() {
        JSONObject json = new JSONObject();
        json.put(KEY_TIMEHEADER, VALUE_TIMEHEADER);
        json.put(KEY_IRI_PREFIX, VALUE_IRI_PREFIX_NS);
        json.put(KEY_ADD_TRIPLE, VALUE_ADD_TRIPLE);
        // Invalid non-integer
        json.put(KEY_MULTI_TS_COL_INDEX, "fail");
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
        // Invalid integer <0
        json.put(KEY_MULTI_TS_COL_INDEX, "-5");
        assertFalse(new HistoricalPumpDataInstantiationAgent().validateInput(json));
    }

    @Test
    void testValidateInput() {
        JSONObject json = new JSONObject();
        json.put(KEY_TIMEHEADER, VALUE_TIMEHEADER);
        json.put(KEY_IRI_PREFIX, VALUE_IRI_PREFIX_NS);
        json.put(KEY_ADD_TRIPLE, VALUE_ADD_TRIPLE);
        json.put(KEY_STARTING_ROW, VALUE_STARTING_ROW);
        json.put(KEY_MULTI_TS_COL_INDEX, VALUE_MULTI_TS_COL_INDEX);
        // Test all valid inputs
        assertTrue(new HistoricalPumpDataInstantiationAgent().validateInput(json));
    }

    @Test
    void testProcessRequestParametersInvalidParams() {
        JSONObject json = new JSONObject();
        json.put("wrongKey", "test");
        JSONObject returnMessage = new HistoricalPumpDataInstantiationAgent().processRequestParameters(json);
        assertEquals(REQUEST_PARAMS_ERROR, returnMessage.get("Result").toString());
    }

    @Test
    void testProcessRequestParametersEmptyParams() {
        JSONObject json = new JSONObject();
        JSONObject returnMessage = new HistoricalPumpDataInstantiationAgent().processRequestParameters(json);
        assertEquals(REQUEST_PARAMS_ERROR, returnMessage.get("Result").toString());
    }

    @Test
    void testInitializeAgentNoArgs() {
        args = new String[]{};
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, () ->
                new HistoricalPumpDataInstantiationAgent().initializeAgent(args), "JPSRuntimeException was expected");
        assertEquals("Require 3 arguments! Please send the time header, starting value row, and col index of the group time series."
                , thrown.getMessage());
    }

    @Test
    void testInitializeAgentMissingExcelFile() {
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, () ->
                new HistoricalPumpDataInstantiationAgent().initializeAgent(args), "JPSRuntimeException was expected");
        assertEquals("Could not construct the Excel parser needed to interact with the Excel Workbook!"
                , thrown.getMessage());
    }

    @Test
    void testInitializeAgentParsingExcelError() {
        // Create test Excel workbook
        Path excelFilePath = Paths.get(tempDataDir, "data.xlsx");
        ExcelParserTest.createTestExcelFile(excelFilePath);
        // Execute methods
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, () ->
                new HistoricalPumpDataInstantiationAgent().initializeAgent(args), "JPSRuntimeException was expected");
        assertEquals("Readings could not be retrieved!", thrown.getMessage());
    }

    @Test
    void testInitializeAgentEmptyReadings() {
        // Create test Excel workbook
        Path excelFilePath = Paths.get(tempDataDir, "data.xlsx");
        ExcelParserTest.createTestExcelFile(excelFilePath);
        // Stub the method to return empty readings, this will throw an exception when creating the handler
        Map<String, Map<String, List<?>>> emptyMap = new HashMap<>();
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class,
                (mock, context) -> Mockito.when(mock.parseToHashMap(Mockito.anyInt(), Mockito.anyInt())).thenReturn(emptyMap))) {
            try {
                new HistoricalPumpDataInstantiationAgent().initializeAgent(args);
            } catch (JPSRuntimeException e) {
                assertEquals("Could not construct the time series Properties handler!", e.getMessage());
                // Verify preceding method were called
                Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(Integer.parseInt(args[2]), Integer.parseInt(args[3]));
            }
        }
    }

    @Test
    void testInitializeAgentGenIRIMapError() {
        // Create test Excel workbook
        Path excelFilePath = Paths.get(tempDataDir, "data.xlsx");
        ExcelParserTest.createTestExcelFile(excelFilePath);
        // Mock the ExcelParser constructor and methods performed
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class,
                    // Stub the method to throw an Exception when running
                    (mock, context) -> Mockito.when(mock.generateIRIMappings(Mockito.anyString())).thenThrow(IOException.class))) {
                try {
                    new HistoricalPumpDataInstantiationAgent().initializeAgent(args);
                } catch (JPSRuntimeException e) {
                    assertEquals("IRI mappings could not be generated or retrieved!", e.getMessage());
                    // Verify preceding method were called
                    Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(Integer.parseInt(args[2]), Integer.parseInt(args[3]));
                }
            }
        }
    }

    @Test
    void testInitializeAgentCreateTSClientError() throws Exception {
        // Create test Excel workbook
        Path excelFilePath = Paths.get(tempDataDir, "data.xlsx");
        ExcelParserTest.createTestExcelFile(excelFilePath);
        // Execute method with mocks
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class)) {
                try (MockedConstruction<TimeSeriesClientDecorator> mockDecorator = Mockito.mockConstruction(TimeSeriesClientDecorator.class,
                        // Stub the void method to throw an Exception when running
                        (mock, context) -> Mockito.doThrow(JPSRuntimeException.class).when(mock).initializeTimeSeries(Mockito.anyMap(), Mockito.anyMap()))) {
                    try {
                        new HistoricalPumpDataInstantiationAgent().initializeAgent(args);
                    } catch (JPSRuntimeException e) {
                        assertEquals("Could not construct the time series client needed by the input agent!", e.getMessage());
                        // Verify preceding methods were called
                        Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(1, 2);
                        Mockito.verify(mockHandler.constructed().get(0)).generateIRIMappings(Mockito.anyString());
                    }
                }
            }
        }
    }

    @Test
    void testInitializeAgentInitTsError() throws Exception {
        // Create test Excel workbook
        Path excelFilePath = Paths.get(tempDataDir, "data.xlsx");
        ExcelParserTest.createTestExcelFile(excelFilePath);
        // Create sample client.properties
        Path clientFilePath = Paths.get(tempConfigDir, "client.properties");
        FileManagerTest.genSampleProperties(clientFilePath);
        // Execute method with mocks
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class)) {
                try (MockedConstruction<TimeSeriesClientDecorator> mockDecorator = Mockito.mockConstruction(TimeSeriesClientDecorator.class,
                        // Stub the void method to throw an Exception when running
                        (mock, context) -> Mockito.doThrow(JPSRuntimeException.class).when(mock).initializeTimeSeries(Mockito.anyMap(), Mockito.anyMap()))) {
                    try {
                        new HistoricalPumpDataInstantiationAgent().initializeAgent(args);
                    } catch (JPSRuntimeException e) {
                        assertEquals("Could not initialize time series.", e.getMessage());
                        // Verify preceding methods were called
                        Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(1, 2);
                        Mockito.verify(mockHandler.constructed().get(0)).generateIRIMappings(Mockito.anyString());
                    }
                }
            }
        }
    }

    @Test
    void testInitializeAgentUpdateTSError() throws Exception {
        // Create test Excel workbook
        Path excelFilePath = Paths.get(tempDataDir, "data.xlsx");
        ExcelParserTest.createTestExcelFile(excelFilePath);
        // Create sample client.properties
        Path clientFilePath = Paths.get(tempConfigDir, "client.properties");
        FileManagerTest.genSampleProperties(clientFilePath);
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class)) {
                try (MockedConstruction<TimeSeriesClientDecorator> mockDecorator = Mockito.mockConstruction(TimeSeriesClientDecorator.class,
                        // Stub the void method to throw an Exception when running
                        (mock, context) -> Mockito.doThrow(JPSRuntimeException.class).when(mock).updateData(Mockito.anyMap(), Mockito.anyMap()))) {
                    try {
                        new HistoricalPumpDataInstantiationAgent().initializeAgent(args);
                    } catch (JPSRuntimeException e) {
                        assertEquals("Could not update time series!", e.getMessage());
                        // Verify preceding methods were called
                        Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(Mockito.anyInt(), Mockito.anyInt());
                        Mockito.verify(mockHandler.constructed().get(0)).generateIRIMappings(Mockito.anyString());
                        Mockito.verify(mockDecorator.constructed().get(0)).initializeTimeSeries(Mockito.anyMap(), Mockito.anyMap());
                    }
                }
            }
        }
    }

    @Test
    void testInitializeAgent() throws Exception {
        // Create test Excel workbook
        Path excelFilePath = Paths.get(tempDataDir, "data.xlsx");
        ExcelParserTest.createTestExcelFile(excelFilePath);
        // Create sample client.properties
        Path clientFilePath = Paths.get(tempConfigDir, "client.properties");
        FileManagerTest.genSampleProperties(clientFilePath);
        // Mock all the dependent classes to run the Agent
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class)) {
                try (MockedConstruction<TimeSeriesClientDecorator> mockDecorator = Mockito.mockConstruction(TimeSeriesClientDecorator.class)) {
                    try (MockedConstruction<TimeSeriesClient> mockTSClient = Mockito.mockConstruction(TimeSeriesClient.class)) {
                        JSONObject message = new HistoricalPumpDataInstantiationAgent().initializeAgent(args);
                        assertEquals("Data updated with new readings from Excel Workbook.", message.getString("Result"));
                    }
                }
            }
        }
    }
}