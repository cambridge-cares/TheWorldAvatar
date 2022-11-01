package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class HistoricalHouse45UtilitiesAgentTest {
    @TempDir
    private static Path tempDir;
    private static Path excelData;
    private static String[] args;


    @BeforeEach
    void init() {
        Path clientProp = tempDir.resolve("client.properties");
        Path excelProp = tempDir.resolve("excel.properties");
        excelData = tempDir.resolve("data.xlsx");
        args = new String[]{String.valueOf(clientProp), String.valueOf(excelProp),String.valueOf(excelData)};
        HistoricalHouse45UtilitiesAgent agent = new HistoricalHouse45UtilitiesAgent();
        agent.setDateKey("dates");
        agent.setDateArray(null);
    }

    @Test
    void testInitializeAgentNoArgs() {
        String[] args = {};
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, () ->
                new HistoricalHouse45UtilitiesAgent().initializeAgent(args), "JPSRuntimeException was expected");
        assertEquals("Send the file path of two properties files in the following order: 1) Time series client 2) IRI mappings to excel headers. Do note that the second properties file need not exist and would be generated on the first build process."
                , thrown.getMessage());
    }

    @Test
    void testInitializeAgentMissingExcelFile() {
        args[2] = "data.xlsx";
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
    void testInitializeAgentEmptyReadings(){
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
    void testInitializeAgentConstructTsClientError() throws Exception {
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class)) {
                try {
                    new HistoricalHouse45UtilitiesAgent().initializeAgent(args);
                } catch (JPSRuntimeException e) {
                    assertEquals("Could not construct the time series client needed by the input agent!", e.getMessage());
                    // Verify preceding methods were called
                    Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(HistoricalHouse45UtilitiesAgent.rowStart, null);
                    Mockito.verify(mockHandler.constructed().get(0)).generateIRIMappings(args[1]);
                }
            }
        }
    }

    @Test
    void testInitializeAgent() throws Exception {
        // Mock all the dependent classes to run the Agent
        try (MockedConstruction<ExcelParser> mockParser = Mockito.mockConstruction(ExcelParser.class)) {
            try (MockedConstruction<TSPropertiesHandler> mockHandler = Mockito.mockConstruction(TSPropertiesHandler.class)) {
                try (MockedConstruction<DateTSClientDecorator> mockDecorator = Mockito.mockConstruction(DateTSClientDecorator.class)) {
                    try (MockedConstruction<TimeSeriesClient> mockTSClient = Mockito.mockConstruction(TimeSeriesClient.class)) {
                        new HistoricalHouse45UtilitiesAgent().initializeAgent(args);
                        // Verify all methods are performed
                        Mockito.verify(mockParser.constructed().get(0)).parseToHashMap(HistoricalHouse45UtilitiesAgent.rowStart, null);
                        Mockito.verify(mockHandler.constructed().get(0)).generateIRIMappings(Mockito.anyString());
                        Mockito.verify(mockDecorator.constructed().get(0)).initializeTimeSeriesIfNotExist(Mockito.anyMap(), Mockito.anyMap());
                        Mockito.verify(mockDecorator.constructed().get(0)).updateData(Mockito.anyMap(), Mockito.anyMap());
                    }
                }
            }
        }
    }
}
