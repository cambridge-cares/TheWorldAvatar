package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
public class DateTSClientDecoratorTest {
    private DateTSClientDecorator testDecorator;
    @Mock
    private TimeSeriesClient<LocalDate> mockTSClient;
    private static Map<String, String> testMappings;
    private static Map<String, List<?>> testReadings;
    private static final List<String> testIRIs = Arrays.asList("iri1", "iri2", "iri3");
    private static final String testDateKey = "dates";
    private static final int[] testDateArrays = {0, 1, 2};

    @BeforeAll
    static void genTestData() {
        testMappings = new HashMap<>();
        testMappings.put("testKey", "testIRI");
        testReadings = new HashMap<>();
        testReadings.put("testKey", testIRIs);
    }

    @BeforeEach
    void initTSClient() {
        testDecorator = new DateTSClientDecorator(testDateKey);
        testDecorator.setTsClient(mockTSClient);
    }

    @Test
    void testDateTSClientWrapperConstructor() {
        assertAll(
                () -> assertNotNull(new DateTSClientDecorator(testDateKey)),
                () -> assertNotNull(new DateTSClientDecorator(testDateKey, testDateArrays))
        );
    }

    @Test
    void testTimeSeriesExistAllIRIsTrue() throws NoSuchMethodException {
        // Make method accessible
        Method timeSeriesExist = DateTSClientDecorator.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Set the mock to return true for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString())).thenReturn(true);
        assertAll(
                // Empty list should return true
                () -> assertTrue((boolean) timeSeriesExist.invoke(testDecorator, new ArrayList<String>())),
                // Should return true as all IRIs are attached (based on the mock)
                () -> assertTrue((boolean) timeSeriesExist.invoke(testDecorator, testIRIs))
        );
        // Check also that the check was invoked for all keys
        for (String iri : testIRIs) {
            Mockito.verify(mockTSClient).checkDataHasTimeSeries(iri);
        }
    }

    @Test
    void testTimeSeriesExistOneIRIFalse() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = DateTSClientDecorator.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Set the mock to return false on second IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(testIRIs.get(0))).thenReturn(true);
        Mockito.when(mockTSClient.checkDataHasTimeSeries(testIRIs.get(1))).thenReturn(false);
        // Second IRI is not in database (mock) and should return false
        assertFalse((boolean) timeSeriesExist.invoke(testDecorator, testIRIs));
    }

    @Test
    void testTimeSeriesExistAllIRIFalse() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = DateTSClientDecorator.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Set the mock to return false for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString())).thenReturn(false);
        // Should return false as no IRI is attached (based on the mock)
        assertFalse((boolean) timeSeriesExist.invoke(testDecorator, testIRIs));
        // Should have returned false after first IRI
        Mockito.verify(mockTSClient).checkDataHasTimeSeries(testIRIs.get(0));
        Mockito.verify(mockTSClient, Mockito.never()).checkDataHasTimeSeries(testIRIs.get(1));
        Mockito.verify(mockTSClient, Mockito.never()).checkDataHasTimeSeries(testIRIs.get(2));
    }

    @Test
    void testInitializeTimeSeriesIfNotExistCreateAll() {
        // Set the mock to return false for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString())).thenReturn(false);
        testDecorator.initializeTimeSeriesIfNotExist(testReadings, testMappings);
        // Time series should be initialized one time as one Excel sheet has one time series
        Mockito.verify(mockTSClient)
                .initTimeSeries(Mockito.anyList(), Mockito.anyList(), Mockito.anyString());
    }

    @Test
    void testInitializeTimeSeriesIfNotExistCreateNone() {
        // Set the mock to return true for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString())).thenReturn(true);
        testDecorator.initializeTimeSeriesIfNotExist(testReadings, testMappings);
        // Time Series should not be initialized for any mapping
        Mockito.verify(mockTSClient, Mockito.never())
                .initTimeSeries(Mockito.anyList(), Mockito.anyList(), Mockito.anyString());
    }

    @Test
    @DisplayName("Test for updateData method when Date data is available in one Excel column")
    void testUpdateData() {
        // Set up
        List<LocalDateTime> testDates = new ArrayList<>();
        testDates.add(LocalDate.parse("2022-08-14").atStartOfDay());
        testDates.add(LocalDate.parse("2022-08-17").atStartOfDay());
        testDates.add(LocalDate.parse("2022-08-21").atStartOfDay());
        testReadings.put(testDateKey, testDates);

        // Run the update
        testDecorator.updateData(testReadings, testMappings);
        // Capture the time series argument in tsClient.addTimeSeriesData() method
        ArgumentCaptor<TimeSeries<LocalDate>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the method was called once
        Mockito.verify(mockTSClient).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the Time Series object have the correct structure, types, and values as dummy data
        assertAll(
                () -> assertEquals("testIRI", timeSeriesArgument.getValue().getDataIRIs().get(0)),
                () -> assertEquals(3, timeSeriesArgument.getValue().getTimes().size()),
                // LocalDateTime should be read as LocalDate object
                () -> assertEquals(LocalDate.parse("2022-08-14"), timeSeriesArgument.getValue().getTimes().get(0)),
                () -> assertEquals(LocalDate.parse("2022-08-21"), timeSeriesArgument.getValue().getTimes().get(2)),
                () -> assertEquals(3, timeSeriesArgument.getValue().getValuesAsString("testIRI").size()),
                () -> assertEquals("iri3", timeSeriesArgument.getValue().getValuesAsString("testIRI").get(2))
        );
    }

    @Test
    @DisplayName("Test for updateData method when Date data is split across three Excel columns")
    void testUpdateDataMultiColDate() {
        // Set up
        testDecorator = new DateTSClientDecorator(testDateKey, testDateArrays);
        testDecorator.setTsClient(mockTSClient);
        List<LocalDate> testDates = new ArrayList<>();
        testDates.add(LocalDate.parse("2022-08-16"));
        testDates.add(LocalDate.parse("2022-08-21"));
        testDates.add(LocalDate.parse("2022-08-25"));
        testReadings.put(testDateKey, testDates);

        // Run the update
        testDecorator.updateData(testReadings, testMappings);
        // Capture the time series argument in tsClient.addTimeSeriesData() method
        ArgumentCaptor<TimeSeries<LocalDate>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the method was called once
        Mockito.verify(mockTSClient).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the Time Series object have the correct structure, types, and values as dummy data
        assertAll(
                () -> assertEquals("testIRI", timeSeriesArgument.getValue().getDataIRIs().get(0)),
                () -> assertEquals(3, timeSeriesArgument.getValue().getTimes().size()),
                () -> assertEquals(LocalDate.parse("2022-08-16"), timeSeriesArgument.getValue().getTimes().get(0)),
                () -> assertEquals(3, timeSeriesArgument.getValue().getValuesAsString("testIRI").size()),
                () -> assertEquals("iri3", timeSeriesArgument.getValue().getValuesAsString("testIRI").get(2))
        );
    }

}
