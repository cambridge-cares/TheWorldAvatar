package uk.ac.cam.cares.jps.agent.cea.utils;

import static org.mockito.Mockito.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.*;
import java.time.OffsetDateTime;
import java.time.Instant;

public class TimeSeriesHelperTest {
    @Test
    public void testCreateTimeSeries() throws SQLException {
        RemoteStoreClient mockStoreClient = mock(RemoteStoreClient.class);
        RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
        Connection mockConnection = mock(Connection.class);

        doReturn(mockConnection).when(mockRDBClient).getConnection();

        TimeSeriesHelper timeSeriesHelper = new TimeSeriesHelper(mockStoreClient, mockRDBClient);

        try (MockedConstruction<TimeSeriesClient> timeseriesClientMock = mockConstruction(TimeSeriesClient.class,
                (mock, context) -> {
                    doReturn(false).when(mock).checkDataHasTimeSeries(anyString(), any());
                })) {

            timeSeriesHelper.createTimeSeries(new LinkedHashMap<>(), "", new OntologyURIHelper("CEAAgentConfig"));

            verify(timeseriesClientMock.constructed().get(0), times(1)).checkDataHasTimeSeries(anyString(), any());
            verify(timeseriesClientMock.constructed().get(0), times(1)).initTimeSeries(anyList(), anyList(), anyString(), any(), any(), any(), any());
        }

    }

    @Test
    public void testAddDataToTimeSeries() throws SQLException {
        RemoteStoreClient mockStoreClient = mock(RemoteStoreClient.class);
        RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
        Connection mockConnection = mock(Connection.class);

        doReturn(mockConnection).when(mockRDBClient).getConnection();

        TimeSeriesHelper timeSeriesHelper = new TimeSeriesHelper(mockStoreClient, mockRDBClient);
        LinkedHashMap<String, String> iris = new LinkedHashMap<>();
        iris.put("test_value_1", "test_iri_1");
        iris.put("test_value_2", "test_iri_2");

        List<List<?>> values = new ArrayList<>();

        List<String> test_list_1 = new ArrayList<>();
        test_list_1.add("1.5");
        test_list_1.add("2.5");

        List<String> test_list_2 = new ArrayList<>();
        test_list_2.add("3.5");
        test_list_2.add("4.5");

        values.add(test_list_1);
        values.add(test_list_2);

        List<OffsetDateTime> times = new ArrayList<>();
        times.add(OffsetDateTime.now());
        times.add(OffsetDateTime.now());

        try(MockedConstruction<TimeSeriesClient> timeSeriesClientMock = mockConstruction(TimeSeriesClient.class)) {
            timeSeriesHelper.addDataToTimeSeries(values, times, iris);

            // Ensure correct methods on time series client are called
            verify(timeSeriesClientMock.constructed().get(0), times(1)).getMaxTime(anyString(), any());
            verify(timeSeriesClientMock.constructed().get(0), times(1)).getMinTime(anyString(), any());
            verify(timeSeriesClientMock.constructed().get(0), times(1)).addTimeSeriesData(any(), any());
        }
    }

    @Test
    public void testRetrieveData() throws SQLException {
        RemoteStoreClient mockStoreClient = mock(RemoteStoreClient.class);
        RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
        Connection mockConnection = mock(Connection.class);

        doReturn(mockConnection).when(mockRDBClient).getConnection();

        String iri = "test";
        List<String> iris = new ArrayList<>();
        iris.add(iri);

        TimeSeries<Instant> mockTS = mock(TimeSeries.class);

        try(MockedConstruction<TimeSeriesClient> timeSeriesClientMock = mockConstruction(TimeSeriesClient.class,
                (mock, context) -> {doReturn(mockTS).when(mock).getTimeSeries(anyList(), any());
                })) {
            TimeSeriesHelper.retrieveData(iri, mockStoreClient, mockRDBClient, Instant.class);

            // Ensure method to get time series client was invoked once
            verify(timeSeriesClientMock.constructed().get(0), times(1)).getTimeSeries(anyList(), any());
        }
    }
}
