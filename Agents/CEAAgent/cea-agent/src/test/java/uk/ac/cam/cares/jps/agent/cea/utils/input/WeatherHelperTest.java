package uk.ac.cam.cares.jps.agent.cea.utils.input;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Polygon;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;
import uk.ac.cam.cares.jps.agent.cea.utils.TimeSeriesHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.*;
import java.net.HttpURLConnection;
import kong.unirest.HttpResponse;
import kong.unirest.Unirest;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.*;
import java.time.Instant;
import java.time.OffsetDateTime;

public class WeatherHelperTest {
    @Test
    public void testGetWeather() {
        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<uk.ac.cam.cares.jps.agent.cea.utils.FileReader> fileReaderMock = mockStatic(uk.ac.cam.cares.jps.agent.cea.utils.FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            WeatherHelper weatherHelper = spy(new WeatherHelper("", "", "", ""));

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

            String testCRS = "4326";

            Coordinate[] coordinates = new Coordinate[]{
                    new Coordinate(0, 0),
                    new Coordinate(0, 5),
                    new Coordinate(5, 5),
                    new Coordinate(5, 0),
                    new Coordinate(0, 0)
            };
            Coordinate[] coordinates1 = new Coordinate[]{
                    new Coordinate(0.1, 0.1),
                    new Coordinate(0.1, 5.1),
                    new Coordinate(5.1, 5.1),
                    new Coordinate(5.1, 0.1),
                    new Coordinate(0.1, 0.1)
            };

            GeometryFactory geometryFactory = new GeometryFactory();

            Polygon polygon = geometryFactory.createPolygon(coordinates);
            Polygon polygon1 = geometryFactory.createPolygon(coordinates1);

            CEAGeometryData testBuilding = new CEAGeometryData(Arrays.asList(polygon), testCRS, "10.0");
            CEAGeometryData testBuilding1 = new CEAGeometryData(Arrays.asList(polygon1), testCRS, "10.0");

            List<CEAGeometryData> testSurroundings = Arrays.asList(testBuilding, testBuilding1);

            try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
                // test when getWeather fails
                List<Object> testList = new ArrayList<>();

                assertFalse(weatherHelper.getWeather(new CEAGeometryData(), new ArrayList<>(), "", testCRS, testList));
                assertEquals(0, testList.size());


                // test when there are retrievable weather data
                JSONArray station = new JSONArray();
                station.put(new JSONObject().put("station", "testStation"));
                JSONArray weatherIRIs = new JSONArray();
                weatherIRIs.put(new JSONObject().put("weatherParameter", "test/testWeather").put("measure", "testMeasure").put("rdb", "testRDB"));
                JSONArray coordinate = new JSONArray();
                coordinate.put(new JSONObject().put("coordinate", "1.0#1.0"));
                JSONArray elevation = new JSONArray();
                elevation.put(new JSONObject().put("elevation", "1.0"));

                accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                        .thenReturn(station).thenReturn(weatherIRIs).thenReturn(coordinate).thenReturn(elevation);

                try (MockedStatic<TimeSeriesHelper> mockTSHelper = mockStatic(TimeSeriesHelper.class)) {
                    mockTSHelper.when(() -> TimeSeriesHelper.retrieveData(anyString(), any(), any(), any()))
                            .thenReturn(mockTS);

                    assertTrue(weatherHelper.getWeather(testBuilding, testSurroundings, "", testCRS, testList));

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
    }

    @Test
    public void testRunOpenMeteoAgent() {
        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontotimeseries=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            WeatherHelper weatherHelper = spy(new WeatherHelper("", "", "", ""));

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
}
