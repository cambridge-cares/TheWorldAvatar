package uk.ac.cam.cares.jps.agent.cea;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mockStatic;

import com.cmclinnovations.stack.clients.core.StackClient;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.utils.AnnualValueHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.TimeSeriesHelper;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;
import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.tasks.RunCEATask;
import uk.ac.cam.cares.jps.agent.cea.utils.datahandler.*;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryQueryHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.input.*;
import uk.ac.cam.cares.jps.agent.cea.utils.endpoint.*;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.ThreadPoolExecutor;

public class CEAAgentTest {
    @Test
    public void testCEAAgent() {
        String content = "access.url=test\ncea.label=test\nweather.label=test\nurl.openmeteoagent=test\n" +
                "terrain.database=test\nterrain.table=test\ncea.database=test" ;

        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());

        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try {
                try (MockedStatic<StackClient> stackClientMock = mockStatic(StackClient.class)) {
                    stackClientMock.when(() -> StackClient.getStackName())
                            .thenReturn("");
                    try (MockedConstruction<EndpointConfig> endpointMock = mockConstruction(EndpointConfig.class,
                            (mock, context) -> {
                                doReturn("").when(mock).getDbUrl(anyString());
                                doReturn("").when(mock).getDbUser();
                                doReturn("").when(mock).getDbPassword();
                            })) {
                        CEAAgent agent = new CEAAgent();
                        assertNotNull(agent);
                    }
                }
            } catch (Exception e) {
                fail();
            }
        }
    }

    @Test
    public void testProcessRequestParameters() throws IllegalAccessException, NoSuchFieldException {
        JSONObject requestParams = new JSONObject();

        // Test the update endpoint
        requestParams.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/update");
        requestParams.put(CEAAgent.KEY_IRI, "['http://www.theworldavatar.com:83/citieskg/namespace/pirmasensEPSG32633/sparql/cityobject/UUID_test/']");
        requestParams.put(CEAAgent.KEY_TARGET_URL, "http://localhost:8086/agents/cea/update");
        requestParams.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.POST);

        JSONArray arrayMock = mock(JSONArray.class);
        when(arrayMock.length()).thenReturn(1);
        when(arrayMock.getString(anyInt())).thenReturn(OffsetDateTime.now().toString()).thenReturn("4.2");
        when(arrayMock.get(anyInt())).thenReturn(arrayMock);

        requestParams.put(CEAConstants.KEY_GRID_CONSUMPTION, arrayMock);
        requestParams.put(CEAConstants.KEY_ELECTRICITY_CONSUMPTION, arrayMock);
        requestParams.put(CEAConstants.KEY_HEATING_CONSUMPTION, arrayMock);
        requestParams.put(CEAConstants.KEY_COOLING_CONSUMPTION, arrayMock);
        requestParams.put(CEAConstants.KEY_PV_ROOF_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PV_WALL_SOUTH_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PV_WALL_NORTH_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PV_WALL_EAST_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PV_WALL_WEST_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_ROOF_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_ROOF_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_WALL_EAST_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_WALL_WEST_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_ROOF_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_ROOF_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_WALL_EAST_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_WALL_WEST_E_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_PLATE_ROOF_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_PLATE_WALL_EAST_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_PLATE_WALL_WEST_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_TUBE_ROOF_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_TUBE_WALL_EAST_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_THERMAL_TUBE_WALL_WEST_SUPPLY, arrayMock);
        requestParams.put(CEAConstants.KEY_ROOF_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAConstants.KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAConstants.KEY_NORTH_WALL_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAConstants.KEY_EAST_WALL_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAConstants.KEY_WEST_WALL_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAAgent.KEY_TIMES, arrayMock);

        JSONObject returnParams;

        String content = "access.url=test\ncea.label=test\nweather.label=test\nurl.openmeteoagent=test\n" +
                "terrain.database=test\nterrain.table=test\ncea.database=test";

        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        InputStream mockInputStream1 = new ByteArrayInputStream(content.getBytes());
        InputStream mockInputStream2 = new ByteArrayInputStream(content.getBytes());


        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream)
                    .thenReturn(mockInputStream1).thenReturn(mockInputStream2);
            try (MockedStatic<StackClient> stackClientMock = mockStatic(StackClient.class)) {
                stackClientMock.when(() -> StackClient.getStackName())
                        .thenReturn("");
                try (MockedConstruction<EndpointConfig> endpointMock = mockConstruction(EndpointConfig.class,
                        (mock, context) -> {
                            doReturn("").when(mock).getDbUrl(anyString());
                            doReturn("").when(mock).getDbUser();
                            doReturn("").when(mock).getDbPassword();
                            doReturn("").when(mock).getOntopUrl();
                        })) {
                    try (MockedStatic<DataManager> dataManagerMock = mockStatic(DataManager.class)) {
                        dataManagerMock.when(() -> DataManager.checkBuildingInitialised(anyString(), anyString())).thenReturn(true);
                        dataManagerMock.when(() -> DataManager.checkDataInitialised(anyString(), any(), any(), anyString())).thenReturn(true);
                        try (MockedConstruction<TimeSeriesHelper> mockTs = mockConstruction(TimeSeriesHelper.class)) {
                            try (MockedStatic<AnnualValueHelper> annualValueHelperMock = mockStatic(AnnualValueHelper.class)) {
                                // test update endpoint
                                CEAAgent agent = new CEAAgent();
                                returnParams = agent.processRequestParameters(requestParams);

                                verify(mockTs.constructed().get(0), times(1)).addDataToTimeSeries(anyList(), anyList(), any());
                                annualValueHelperMock.verify(() -> AnnualValueHelper.instantiateAnnual(anyList(), any(), any()), times(1));
                                assertEquals(requestParams, returnParams);
                            }
                        }
                    }
                    List<String> endpoints = new ArrayList<>();
                    endpoints.add("");
                    endpoints.add("");
                    try (MockedStatic<RouteHelper> routeHelperMock = mockStatic(RouteHelper.class)) {
                        routeHelperMock.when(() -> RouteHelper.checkEndpoint(anyString())).thenReturn(true);
                        routeHelperMock.when(() -> RouteHelper.getRouteEndpoints(anyString())).thenReturn(endpoints);
                        CEAGeometryData testGeometry = new CEAGeometryData(new ArrayList<>(), "", "");
                        try (MockedStatic<GeometryQueryHelper> geometryQueryHelperMock = mockStatic(GeometryQueryHelper.class)) {
                            geometryQueryHelperMock.when(() -> GeometryQueryHelper.getBuildingGeometry(anyString(), anyString(), anyBoolean())).thenReturn(testGeometry);
                            Map<String, Double> usages = new HashMap<>();
                            try (MockedStatic<BuildingUsageHelper> usageHelpermock = mockStatic(BuildingUsageHelper.class)) {
                                usageHelpermock.when(() -> BuildingUsageHelper.getBuildingUsages(anyString(), anyString())).thenReturn(usages);
                                try (MockedStatic<SurroundingsHelper> surroundingsHelperMock = mockStatic(SurroundingsHelper.class)) {
                                    surroundingsHelperMock.when(() -> SurroundingsHelper.getSurroundings(any(), any(), anyString())).thenReturn(new ArrayList<CEAGeometryData>());
                                    try (MockedConstruction<WeatherHelper> weatherHelperMock = mockConstruction(WeatherHelper.class,
                                            (mock, context) -> {
                                                doReturn(false).when(mock).getWeather(any(), anyList(), anyString(), anyString(), anyList());
                                            })) {
                                        byte[] terrain = new byte[1];
                                        try (MockedConstruction<TerrainHelper> terrainHelperMock = mockConstruction(TerrainHelper.class,
                                                (mock, context) -> {
                                                    doReturn(terrain).when(mock).getTerrain(any(), anyList(), anyString());
                                                })) {
                                            try (MockedConstruction<RunCEATask> mockTask = mockConstruction(RunCEATask.class)) {
                                                // test run endpoint
                                                requestParams.remove(CEAAgent.KEY_REQ_URL);
                                                requestParams.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/run");

                                                CEAAgent agent = new CEAAgent();

                                                ThreadPoolExecutor executor = mock(ThreadPoolExecutor.class);
                                                Field CEAExecutor = agent.getClass().getDeclaredField("CEAExecutor");
                                                CEAExecutor.setAccessible(true);
                                                CEAExecutor.set(agent, executor);

                                                returnParams = agent.processRequestParameters(requestParams);

                                                geometryQueryHelperMock.verify(() -> GeometryQueryHelper.getBuildingGeometry(anyString(), anyString(), anyBoolean()), times(1));
                                                usageHelpermock.verify(() -> BuildingUsageHelper.getBuildingUsages(anyString(), anyString()), times(1));
                                                surroundingsHelperMock.verify(() -> SurroundingsHelper.getSurroundings(any(), any(), anyString()), times(1));
                                                verify(weatherHelperMock.constructed().get(0), times(1)).getWeather(any(), anyList(), anyString(), anyString(), anyList());
                                                verify(terrainHelperMock.constructed().get(0), times(1)).getTerrain(any(), anyList(), anyString());
                                                verify(executor, times(1)).execute(mockTask.constructed().get(0));
                                                assertEquals(requestParams, returnParams);
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // test query endpoint
                        requestParams.remove(CEAAgent.KEY_REQ_URL);
                        requestParams.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/query");

                        // test time series data
                        String testUnit = "testUnit";
                        String testScalar = "testScalar";
                        ArrayList<String> testList = mock(ArrayList.class);
                        when(testList.get(0)).thenReturn(testScalar);
                        when(testList.get(1)).thenReturn(testUnit);
                        String testReturnValue = "testAnnual";
                        TimeSeries<OffsetDateTime> timeSeries = mock(TimeSeries.class);

                        try (MockedStatic<DataManager> dataManagerMock = mockStatic(DataManager.class)) {
                            dataManagerMock.when(() -> DataManager.checkBuildingInitialised(anyString(), anyString())).thenReturn(true);
                            try (MockedStatic<DataRetriever> dataRetrieverMock = mockStatic(DataRetriever.class)) {
                                dataRetrieverMock.when(() -> DataRetriever.getDataIRI(anyString(), anyString(), anyString())).thenReturn(testList);
                                dataRetrieverMock.when(() -> DataRetriever.getUnit(anyString())).thenReturn(testUnit);
                                dataRetrieverMock.when(() -> DataRetriever.getNumericalValue(anyString(), anyString())).thenReturn(testScalar);
                                try (MockedStatic<TimeSeriesHelper> timeSeriesHelperMock = mockStatic(TimeSeriesHelper.class)) {
                                    timeSeriesHelperMock.when(() -> TimeSeriesHelper.retrieveData(anyString(), any(), any(), any())).thenReturn(timeSeries);
                                    try (MockedStatic<AnnualValueHelper> annualValueHelperMock = mockStatic(AnnualValueHelper.class)) {
                                        annualValueHelperMock.when(() -> AnnualValueHelper.getInfo(anyString(), anyString(), anyString())).thenReturn("");
                                        annualValueHelperMock.when (() -> AnnualValueHelper.getType(anyString(), anyString())).thenReturn("");
                                        annualValueHelperMock.when(() -> AnnualValueHelper.retrieveAnnualValue(anyString(), anyString(), anyString())).thenReturn(testReturnValue);
                                        CEAAgent agent = new CEAAgent();

                                        returnParams = agent.processRequestParameters(requestParams);
                                        String result = returnParams.get(CEAAgent.CEA_OUTPUTS).toString();

                                        for (String scalar : CEAConstants.SCALARS) {
                                            String expected = "\"" + scalar + "\"" + ":\"testScalar testUnit\"";
                                            assertTrue(result.contains(expected));
                                        }

                                        for (String ts : CEAConstants.TIME_SERIES) {
                                            String expected;
                                            if (ts.contains("Consumption")) {
                                                expected = "\"Annual " + ts + "\"" + ":\"testAnnual testUnit\"";
                                            } else {
                                                expected = "\"Annual ";
                                                if (ts.contains("ESupply")) {
                                                    // PVT annual electricity supply
                                                    expected = expected + ts.split("ESupply")[0] + " Electricity Supply";
                                                } else if (ts.contains("QSupply")) {
                                                    // PVT annual heat supply
                                                    expected = expected + ts.split("QSupply")[0] + " Heat Supply";
                                                } else {
                                                    if (ts.contains("Thermal")) {
                                                        // solar collector annual heat supply
                                                        expected = expected + ts.split("Supply")[0] + " Heat Supply";
                                                    } else if (ts.contains("PV")) {
                                                        // PV annual electricity supply
                                                        expected = expected + ts.split("Supply")[0] + " Electricity Supply";
                                                    }
                                                }
                                                expected = expected + "\"" + ":\"testAnnual testUnit\"";
                                            }
                                            assertTrue(result.contains(expected));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    @Test
    public void testValidateInput() {
        String content = "access.url=test\ncea.label=test\nweather.label=test\nurl.openmeteoagent=test\n" +
                "terrain.database=test\nterrain.table=test\ncea.database=test" ;

        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());

        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedStatic<StackClient> stackClientMock = mockStatic(StackClient.class)) {
                stackClientMock.when(() -> StackClient.getStackName())
                        .thenReturn("");
                try (MockedConstruction<EndpointConfig> endpointMock = mockConstruction(EndpointConfig.class,
                        (mock, context) -> {
                            doReturn("").when(mock).getDbUrl(anyString());
                            doReturn("").when(mock).getDbUser();
                            doReturn("").when(mock).getDbPassword();
                        })) {
                    // check general request

                    CEAAgent agent = new CEAAgent();

                    JSONObject requestParams = new JSONObject();

                    // check failure with empty request params

                    assertThrows(BadRequestException.class, () -> {
                        agent.validateInput(requestParams);
                    });

                    requestParams.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.POST);

                    // check failure with no IRI and request URL
                    assertThrows(BadRequestException.class, () -> {
                        agent.validateInput(requestParams);
                    });

                    requestParams.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/run");

                    // check failure with no IRI
                    assertThrows(BadRequestException.class, () -> {
                        agent.validateInput(requestParams);
                    });

                    requestParams.put(CEAAgent.KEY_IRI, "test");

                    requestParams.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.GET);

                    // check failure with GET http method
                    assertThrows(BadRequestException.class, () -> {
                        agent.validateInput(requestParams);
                    });

                    requestParams.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.POST);

                    // should pass now
                    try {
                        assertTrue((agent.validateInput(requestParams)));
                    } catch (Exception e) {
                        fail();
                    }


                    // test run request
                    JSONObject runRequest = new JSONObject();
                    runRequest.put(CEAAgent.KEY_IRI, "");
                    runRequest.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/run");
                    runRequest.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.POST);

                    // check failure with empty request params
                    assertThrows(BadRequestException.class, () -> {
                        agent.validateInput(runRequest);
                    });

                    runRequest.put(CEAAgent.KEY_IRI, "test");

                    // should pass with only IRI
                    try {
                        assertTrue((agent.validateInput(runRequest)));
                    } catch (Exception e) {
                        fail();
                    }


                    // check update request
                    JSONObject updateRequest = new JSONObject();
                    updateRequest.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/update");
                    updateRequest.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.POST);

                    updateRequest.put(CEAAgent.KEY_IRI, "");
                    updateRequest.put(CEAAgent.KEY_TARGET_URL, "");
                    updateRequest.put(CEAConstants.KEY_GRID_CONSUMPTION, "");
                    updateRequest.put(CEAConstants.KEY_ELECTRICITY_CONSUMPTION, "");
                    updateRequest.put(CEAConstants.KEY_HEATING_CONSUMPTION, "");
                    updateRequest.put(CEAConstants.KEY_COOLING_CONSUMPTION, "");
                    updateRequest.put(CEAConstants.KEY_PV_ROOF_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PV_WALL_SOUTH_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PV_WALL_NORTH_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PV_WALL_EAST_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PV_WALL_WEST_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_ROOF_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_ROOF_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_EAST_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_WEST_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_ROOF_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_ROOF_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_EAST_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_WEST_E_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_ROOF_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_WALL_EAST_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_WALL_WEST_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_ROOF_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_WALL_EAST_SUPPLY, "");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_WALL_WEST_SUPPLY, "");
                    updateRequest.put(CEAAgent.KEY_TIMES, "");

                    // check failure with empty request params
                    assertThrows(BadRequestException.class, () -> {
                        agent.validateInput(updateRequest);
                    });

                    updateRequest.put(CEAAgent.KEY_IRI, "test");

                    // check failure with only IRI
                    assertThrows(BadRequestException.class, () -> {
                        agent.validateInput(updateRequest);
                    });

                    updateRequest.put(CEAAgent.KEY_TARGET_URL, "http://localhost:8086/agents/cea/update");

                    // check failure with only IRI and target url
                    assertThrows(BadRequestException.class, () -> {
                        agent.validateInput(updateRequest);
                    });

                    updateRequest.put(CEAConstants.KEY_GRID_CONSUMPTION, "test");
                    updateRequest.put(CEAConstants.KEY_ELECTRICITY_CONSUMPTION, "test");
                    updateRequest.put(CEAConstants.KEY_HEATING_CONSUMPTION, "test");
                    updateRequest.put(CEAConstants.KEY_COOLING_CONSUMPTION, "test");
                    updateRequest.put(CEAConstants.KEY_PV_ROOF_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PV_WALL_SOUTH_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PV_WALL_NORTH_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PV_WALL_EAST_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PV_WALL_WEST_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_ROOF_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_ROOF_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_EAST_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_WEST_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_ROOF_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_ROOF_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_EAST_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_WEST_E_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_ROOF_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_WALL_EAST_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_PLATE_WALL_WEST_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_ROOF_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_WALL_EAST_SUPPLY, "test");
                    updateRequest.put(CEAConstants.KEY_THERMAL_TUBE_WALL_WEST_SUPPLY, "test");
                    updateRequest.put(CEAAgent.KEY_TIMES, "test");

                    // should pass now
                    try {
                        assertTrue((agent.validateInput(updateRequest)));
                    } catch (Exception e) {
                        fail();
                    }


                    // test query endpoint
                    JSONObject queryRequest = new JSONObject();
                    queryRequest.put(CEAAgent.KEY_IRI, "");
                    queryRequest.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/query");
                    queryRequest.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.POST);

                    // check failure with empty request params
                    assertThrows(BadRequestException.class, () -> {
                        agent.validateInput(queryRequest);
                    });

                    queryRequest.put(CEAAgent.KEY_IRI, "test");

                    // should pass now
                    try {
                        assertTrue((agent.validateInput(queryRequest)));
                    } catch (Exception e) {
                        fail();
                    }
                }
            }
        }
    }
}
