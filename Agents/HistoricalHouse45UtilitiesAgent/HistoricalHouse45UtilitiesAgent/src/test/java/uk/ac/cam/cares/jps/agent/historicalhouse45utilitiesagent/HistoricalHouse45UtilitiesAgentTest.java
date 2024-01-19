package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim.OntoBimAdapter;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class HistoricalHouse45UtilitiesAgentTest {
    @TempDir
    private static Path tempDir;
    private static Path excelData;
    private static String[] args;
    private static final String INPUT_KEY = "clientProperties";
    private static final String ENV_CLIENT_VAR = "TIMESERIES_CLIENTPROPERTIES";
    private static final String REQUEST_PARAMS_ERROR = "Request parameters are not defined correctly.";
    private static final String QUERY_ENDPOINT_KEY = "queryEndpoint";
    private static final String QUERY_ENDPOINT_VALUE = "http://host.docker.internal:9999/blazegraph/namespace/test/sparql";
    private static final String UPDATE_ENDPOINT_KEY = "updateEndpoint";
    private static final String UPDATE_ENDPOINT_VALUE = "http://host.docker.internal:9999/blazegraph/namespace/test/sparql";
    private static final String RDB_URL_KEY = "dbUrl";
    private static final String RDB_URL_VALUE = "jdbc:postgresql://host.docker.internal:5432/test";
    private static final String RDB_USER_KEY = "dbUser";
    private static final String RDB_USER_VALUE = "user";
    private static final String RDB_PASS_KEY = "dbPassword";
    private static final String RDB_PASS_VALUE = "pass";

    @BeforeEach
    void init() {
        Path excelProp = tempDir.resolve("excel.properties");
        excelData = tempDir.resolve("data.xlsx");
        args = new String[]{String.valueOf(excelProp), String.valueOf(excelData)};
        HistoricalHouse45UtilitiesAgent agent = new HistoricalHouse45UtilitiesAgent();
        agent.setDateKey("dates");
        agent.setDateArray(null);
    }

    @Test
    void testValidateInputClientPropertiesNotInSystemEnv() {
        JSONObject json = new JSONObject();
        json.put(INPUT_KEY, ENV_CLIENT_VAR);
        assertFalse(new HistoricalHouse45UtilitiesAgent().validateInput(json));
    }

    @Test
    void testValidateInputWrongClientPropertiesKey() {
        JSONObject json = new JSONObject();
        json.put("wrongKey", "test");
        assertFalse(new HistoricalHouse45UtilitiesAgent().validateInput(json));
    }

    @Test
    void testValidateInputEmptyParams() {
        JSONObject json = new JSONObject();
        assertFalse(new HistoricalHouse45UtilitiesAgent().validateInput(json));
    }

    @Test
    void testProcessRequestParametersInvalidParams() {
        JSONObject json = new JSONObject();
        json.put("wrongKey", "test");
        JSONObject returnMessage = new HistoricalHouse45UtilitiesAgent().processRequestParameters(json);
        assertEquals(REQUEST_PARAMS_ERROR, returnMessage.get("Result").toString());
    }

    @Test
    void testProcessRequestParametersEmptyParams() {
        JSONObject json = new JSONObject();
        JSONObject returnMessage = new HistoricalHouse45UtilitiesAgent().processRequestParameters(json);
        assertEquals(REQUEST_PARAMS_ERROR, returnMessage.get("Result").toString());
    }

    @Test
    void testInitializeAgentNoArgs() {
        String[] args = {};
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, () ->
                new HistoricalHouse45UtilitiesAgent().initializeAgent(args), "JPSRuntimeException was expected");
        assertEquals("Require 2 arguments! Please send only the file path to the Excel properties and workbook. Do note that the properties file need not exist and would be generated on the first build process."
                , thrown.getMessage());
    }

    @Test
    void testInitializeAgentMissingExcelFile() {
        args[1] = "data.xlsx";
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, () ->
                new HistoricalHouse45UtilitiesAgent().initializeAgent(args), "JPSRuntimeException was expected");
        assertEquals("Could not construct the Excel parser needed to interact with the Excel Workbook!"
                , thrown.getMessage());
    }

    @Test
    void testInitializeAgentParsingExcelError() {
        ExcelParserTest.createTestExcelFile(excelData);
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, () ->
                new HistoricalHouse45UtilitiesAgent().initializeAgent(args), "JPSRuntimeException was expected");
        assertEquals("Readings could not be retrieved!", thrown.getMessage());
    }

    @Test
    void testInitializeAgentEmptyReadings() {
        // Stub the method to return empty readings, this will throw an exception when creating the handler
        Map<String, List<?>> emptyMap = new HashMap<>();
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class,
                (mock, context) -> Mockito.when(mock.parseToHashMap(Mockito.anyInt())).thenReturn(emptyMap))) {
            try {
                new HistoricalHouse45UtilitiesAgent().initializeAgent(args);
            } catch (JPSRuntimeException e) {
                assertEquals("Could not construct the time series Properties handler!", e.getMessage());
                // Verify preceding method were called
                Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(HistoricalHouse45UtilitiesAgent.rowStart, null);
            }
        }
    }

    @Test
    void testInitializeAgentGenIRIMapError() {
        // Mock the ExcelParser constructor and methods performed
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class,
                    // Stub the method to throw an Exception when running
                    (mock, context) -> Mockito.when(mock.generateIRIMappings(Mockito.anyString())).thenThrow(IOException.class))) {
                try {
                    new HistoricalHouse45UtilitiesAgent().initializeAgent(args);
                } catch (JPSRuntimeException e) {
                    assertEquals("IRI mappings could not be generated or retrieved!", e.getMessage());
                    // Verify preceding method were called
                    Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(HistoricalHouse45UtilitiesAgent.rowStart, null);
                }
            }
        }
    }

    @Test
    void testInitializeAgentInitTsError() throws Exception {
        HistoricalHouse45UtilitiesAgent.clientConfig = genSampleConfig();
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class)) {
                try (MockedConstruction<DateTSClientDecorator> mockDecorator = Mockito.mockConstruction(DateTSClientDecorator.class,
                        // Stub the void method to throw an Exception when running
                        (mock, context) -> Mockito.doThrow(JPSRuntimeException.class).when(mock).initializeTimeSeries(Mockito.anyMap(), Mockito.anyMap()))) {
                    try {
                        new HistoricalHouse45UtilitiesAgent().initializeAgent(args);
                    } catch (JPSRuntimeException e) {
                        assertEquals("Could not initialize time series.", e.getMessage());
                        // Verify preceding methods were called
                        Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(HistoricalHouse45UtilitiesAgent.rowStart, null);
                        Mockito.verify(mockHandler.constructed().get(0)).generateIRIMappings(Mockito.anyString());
                    }
                }
            }
        }
    }

    @Test
    void testInitializeAgentUpdateTSError() throws Exception {
        HistoricalHouse45UtilitiesAgent.clientConfig = genSampleConfig();
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class)) {
                try (MockedConstruction<DateTSClientDecorator> mockDecorator = Mockito.mockConstruction(DateTSClientDecorator.class,
                        // Stub the void method to throw an Exception when running
                        (mock, context) -> Mockito.doThrow(JPSRuntimeException.class).when(mock).updateData(Mockito.anyMap(), Mockito.anyMap()))) {
                    try {
                        new HistoricalHouse45UtilitiesAgent().initializeAgent(args);
                    } catch (JPSRuntimeException e) {
                        assertEquals("Could not update time series!", e.getMessage());
                        // Verify preceding methods were called
                        Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(HistoricalHouse45UtilitiesAgent.rowStart, null);
                        Mockito.verify(mockHandler.constructed().get(0)).generateIRIMappings(Mockito.anyString());
                        Mockito.verify(mockDecorator.constructed().get(0)).initializeTimeSeries(Mockito.anyMap(), Mockito.anyMap());
                    }
                }
            }
        }
    }

    @Test
    void testInitializeAgent() throws Exception {
        HistoricalHouse45UtilitiesAgent.clientConfig = genSampleConfig();
        // Mock all the dependent classes to run the Agent
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class)) {
                try (MockedConstruction<DateTSClientDecorator> mockDecorator = Mockito.mockConstruction(DateTSClientDecorator.class)) {
                    try (MockedConstruction<TimeSeriesClient> mockTSClient = Mockito.mockConstruction(TimeSeriesClient.class)) {
                        // Mock static methods
                        try (MockedStatic<OntoBimAdapter> mockAdaptor = Mockito.mockStatic(OntoBimAdapter.class)) {
                            new HistoricalHouse45UtilitiesAgent().initializeAgent(args);
                            // Verify all methods are performed
                            Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(HistoricalHouse45UtilitiesAgent.rowStart, null);
                            Mockito.verify(mockHandler.constructed().get(0)).generateIRIMappings(Mockito.anyString());
                            Mockito.verify(mockDecorator.constructed().get(0)).initializeTimeSeries(Mockito.anyMap(), Mockito.anyMap());
                            Mockito.verify(mockDecorator.constructed().get(0)).updateData(Mockito.anyMap(), Mockito.anyMap());
                            mockAdaptor.verify(() -> OntoBimAdapter.addSupplementaryTriples(Mockito.anyString(), Mockito.anyString(), Mockito.any()));
                        }
                    }
                }
            }
        }
    }

    private static Map<String, String> genSampleConfig() {
        Map<String, String> sampleConfig = new HashMap<>();
        sampleConfig.put(QUERY_ENDPOINT_KEY, QUERY_ENDPOINT_VALUE);
        sampleConfig.put(UPDATE_ENDPOINT_KEY, UPDATE_ENDPOINT_VALUE);
        sampleConfig.put(RDB_URL_KEY, RDB_URL_VALUE);
        sampleConfig.put(RDB_USER_KEY, RDB_USER_VALUE);
        sampleConfig.put(RDB_PASS_KEY, RDB_PASS_VALUE);
        return sampleConfig;
    }
}
