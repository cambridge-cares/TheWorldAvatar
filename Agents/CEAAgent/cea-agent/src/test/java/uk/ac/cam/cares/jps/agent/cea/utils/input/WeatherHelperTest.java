package uk.ac.cam.cares.jps.agent.cea.utils.input;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.utils.TimeSeriesHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import kong.unirest.HttpResponse;
import kong.unirest.Unirest;

import org.json.JSONArray;
import org.json.JSONObject;

import java.nio.Buffer;
import java.util.*;
import java.time.Instant;
import java.time.OffsetDateTime;

public class WeatherHelperTest {
    @Test
    public void testGetWeather() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        WeatherHelper weatherHelper = spy(new WeatherHelper("", "", "", "", ontologyURIHelper));

        doReturn("").when(weatherHelper).runOpenMeteoAgent(anyString(), anyString(), anyString());
        doReturn(1.0).when(weatherHelper).getStationOffset(anyDouble(), anyDouble(), any(), any());

        RemoteStoreClient mockStoreClient = mock(RemoteStoreClient.class);
        RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);

        Map<String, Object> testMap = new HashMap<>();
        testMap.put(WeatherHelper.STORE_CLIENT, mockStoreClient);
        testMap.put(WeatherHelper.RDB_CLIENT, mockRDBClient);

        doReturn(testMap).when(weatherHelper).getWeatherClients(anyString());

        TimeSeries<Instant> mockTS = mock(TimeSeries.class);

        List<Instant> testTimesList = Collections.nCopies(8760, Instant.parse("2023-01-01T00:00:00.00Z"));
        List<Double> testWeatherData = Collections.nCopies(8760, 0.00);

        doReturn(testTimesList).when(mockTS).getTimes();
        doReturn(testWeatherData).when(mockTS).getValuesAsDouble(anyString());

        String testCRS = "32633";

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                // test when there are no retrievable weather data and request to OpenMeteoAgent fails
                JSONArray envelope = new JSONArray();
                envelope.put(new JSONObject().put("envelope", "555438.08#305587.27999#-0.6#555484.04#305587.27999#-0.6#555484.04#305614.87999#-0.6#555438.08#305614.87999#-0.6#555438.08#305587.27999#-0.6"));

                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenReturn(envelope).thenReturn(new JSONArray());

                List<Object> testList = new ArrayList<>();

                assertFalse(weatherHelper.getWeather("", "", "", testCRS, testList));
                assertEquals(0, testList.size());


                // test when there are retrievable weather data
                JSONArray station = new JSONArray();
                station.put(new JSONObject().put("station", "testStation"));
                JSONArray weatherIRIs = new JSONArray();
                weatherIRIs.put(new JSONObject().put("weatherParameter", ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontoems) + "testWeather").put("measure", "testMeasure").put("rdb", "testRDB"));
                JSONArray coordinate = new JSONArray();
                coordinate.put(new JSONObject().put("coordinate", "1.0#1.0"));
                JSONArray elevation = new JSONArray();
                elevation.put(new JSONObject().put("elevation", "1.0"));


                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenReturn(envelope).thenReturn(station).thenReturn(weatherIRIs).thenReturn(coordinate).thenReturn(elevation);

                try (MockedStatic<TimeSeriesHelper> mockTSHelper = mockStatic(TimeSeriesHelper.class)) {
                    mockTSHelper.when(() -> TimeSeriesHelper.retrieveData(anyString(), any(), any(), any()))
                            .thenReturn(mockTS);

                    assertTrue(weatherHelper.getWeather("", "", "", testCRS, testList));

                    mockTSHelper.verify(
                            times(1), () -> TimeSeriesHelper.retrieveData(anyString(), any(), any(), any())
                    );

                    assertEquals(testList.size(), 3);
                    assertEquals(((Map<String, List<String>>) testList.get(1)).size(), 1);
                    assertTrue(((Map<String, List<String>>) testList.get(1)).containsKey("testWeather"));
                    assertEquals(((Map<String, List<String>>) testList.get(1)).get("testWeather").size(), ((List<OffsetDateTime>) testList.get(0)).size());
                    assertEquals(((List<OffsetDateTime>) testList.get(0)).size(), 8760);
            }
        }
    }

    @Test
    public void testRunOpenMeteoAgent() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        WeatherHelper weatherHelper = spy(new WeatherHelper("", "", "", "", ontologyURIHelper));

        try (MockedStatic<Unirest> unirestMock = mockStatic(Unirest.class, RETURNS_MOCKS)) {

            HttpResponse<String> mockResponse = mock(HttpResponse.class);

            doReturn(HttpURLConnection.HTTP_OK).when(mockResponse).getStatus();

            unirestMock.when(() -> Unirest.post(anyString())
                            .header(anyString(), anyString())
                            .body(anyString())
                            .socketTimeout(anyInt())
                            .asString())
                    .thenReturn(mockResponse);

            String result = weatherHelper.runOpenMeteoAgent("", "", "");

            assertEquals(result, "");
        }
    }
}
