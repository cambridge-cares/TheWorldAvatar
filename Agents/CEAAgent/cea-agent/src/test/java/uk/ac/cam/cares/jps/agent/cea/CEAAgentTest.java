package uk.ac.cam.cares.jps.agent.cea;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import com.cmclinnovations.stack.clients.core.StackClient;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.agent.cea.utils.TimeSeriesHelper;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.data.CEAInputData;
import uk.ac.cam.cares.jps.agent.cea.tasks.RunCEATask;
import uk.ac.cam.cares.jps.agent.cea.utils.datahandler.*;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryQueryHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.input.*;
import uk.ac.cam.cares.jps.agent.cea.utils.endpoint.*;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import java.lang.reflect.Method;
import java.lang.reflect.Field;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.ThreadPoolExecutor;

public class CEAAgentTest {
    @Test
    public void testCEAAgent() {
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

    @Test
    public void testProcessRequestParameters()
            throws IllegalAccessException, NoSuchFieldException {
        try (MockedStatic<StackClient> stackClientMock = mockStatic(StackClient.class)) {
            stackClientMock.when(() -> StackClient.getStackName())
                    .thenReturn("");
            try (MockedConstruction<EndpointConfig> endpointMock = mockConstruction(EndpointConfig.class,
                    (mock, context) -> {
                        doReturn("").when(mock).getDbUrl(anyString());
                        doReturn("").when(mock).getDbUser();
                        doReturn("").when(mock).getDbPassword();
                    })) {
                JSONObject requestParams = new JSONObject();

                // Test empty request params
//                try {
//                    CEAAgent agent = new CEAAgent();
//                    agent.processRequestParameters(requestParams);
//                } catch (Exception e) {
//                    if (e != null) {
//                        assert e instanceof InvocationTargetException;
//                        assertEquals(((InvocationTargetException) e).getTargetException().getClass(),
//                            BadRequestException.class);
//                    }
//                }

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

                try (MockedConstruction<DataManager> dataManagerMock = mockConstruction(DataManager.class,
                        (mock, context) -> {
                            doReturn("building").when(mock).checkBuildingInitialised(anyString(), anyString());
                            doReturn(true).when(mock).checkDataInitialised(anyString(), any(), any(), anyString());
                        })) {
                    try (MockedConstruction<TimeSeriesHelper> mockTs = mockConstruction(TimeSeriesHelper.class)) {
                        // test update endpoint
                        CEAAgent agent = new CEAAgent();
                        returnParams = agent.processRequestParameters(requestParams);

                        verify(mockTs.constructed().get(0), times(1)).addDataToTimeSeries(anyList(), anyList(), any());
                        assertEquals(requestParams, returnParams);
                    }
                }

                    requestParams.remove(CEAAgent.KEY_REQ_URL);
                    requestParams.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/run");

                    List<String> endpoints = new ArrayList<>();
                    endpoints.add("");
                    endpoints.add("");
                    Map<String, Double> usages = new HashMap<>();
                    byte[] terrain = new byte[1];

                    try (MockedStatic<RouteHelper> routeHelperMock = mockStatic(RouteHelper.class)) {
                        routeHelperMock.when(() -> RouteHelper.checkQuadsEnabled(anyString())).thenReturn(true);
                        routeHelperMock.when(() -> RouteHelper.getRouteEndpoints(anyString())).thenReturn(endpoints);
                        try (MockedConstruction<GeometryQueryHelper> geometryQueryHelperMock = mockConstruction(GeometryQueryHelper.class,
                                (mock, context) -> {
                                    doReturn("").when(mock).getBuildingGeometry(anyString(), anyString(), anyString());
                                })) {
                            try (MockedConstruction<BuildingUsageHelper> usageHelpermock = mockConstruction(BuildingUsageHelper.class,
                                    (mock, context) -> {
                                        doReturn(usages).when(mock).getBuildingUsages(anyString(), anyString());
                                    })) {
                                try (MockedConstruction<SurroundingsHelper> surroundingsHelper = mockConstruction(SurroundingsHelper.class,
                                        (mock, context) -> {
                                            doReturn(new ArrayList<CEAInputData>()).when(mock).getSurroundings(anyString(), anyString(), anyList(), anyList());
                                        })) {
                                    try (MockedConstruction<WeatherHelper> weatherHelperMock = mockConstruction(WeatherHelper.class,
                                            (mock, context) -> {
                                                doReturn(false).when(mock).getWeather(anyString(), anyString(), anyString(), anyString(), anyList());
                                            })) {
                                        try (MockedConstruction<TerrainHelper> terrainHelperMock = mockConstruction(TerrainHelper.class,
                                                (mock, context) -> {
                                                    doReturn(terrain).when(mock).getTerrain(anyString(), anyString(), anyString(), anyList(), anyString(), any());
                                                })) {
                                            try (MockedConstruction<RunCEATask> mockTask = mockConstruction(RunCEATask.class)) {
                                                // test run endpoint
                                                CEAAgent agent = new CEAAgent();

                                                ThreadPoolExecutor executor = mock(ThreadPoolExecutor.class);
                                                Field CEAExecutor = agent.getClass().getDeclaredField("CEAExecutor");
                                                CEAExecutor.setAccessible(true);
                                                CEAExecutor.set(agent, executor);

                                                returnParams = agent.processRequestParameters(requestParams);

                                                verify(geometryQueryHelperMock.constructed().get(0), times(3)).getBuildingGeometry(anyString(), anyString(), anyString());
                                                verify(usageHelpermock.constructed().get(0), times(1)).getBuildingUsages(anyString(), anyString());
                                                verify(surroundingsHelper.constructed().get(0), times(1)).getSurroundings(anyString(), anyString(), anyList(), anyList());
                                                verify(weatherHelperMock.constructed().get(0), times(1)).getWeather(anyString(), anyString(), anyString(), anyString(), any());
                                                verify(terrainHelperMock.constructed().get(0), times(1)).getTerrain(anyString(), anyString(), anyString(), anyList(), anyString(), any());
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

                        try(MockedConstruction<DataManager> dataManagerMock = mockConstruction(DataManager.class,
                                (mock, context) -> {
                                    doReturn("building").when(mock).checkBuildingInitialised(anyString(), anyString());
                                })) {
                            try (MockedConstruction<DataRetriever> dataRetrieverMock = mockConstruction(DataRetriever.class,
                                    (mock, context) -> {
                                        doReturn(testList).when(mock).getDataIRI(anyString(), anyString(), anyString());
                                        doReturn(testUnit).when(mock).getUnit(anyString());
                                        doReturn(testScalar).when(mock).getNumericalValue(anyString(), anyString(), anyString());
                                    })) {
                                try (MockedStatic<DataParser> dataParserMock = mockStatic(DataParser.class)) {
                                    dataParserMock.when(() -> DataParser.calculateAnnual(any(), anyString())).thenReturn(testReturnValue);
                                    try (MockedStatic<TimeSeriesHelper> timeSeriesHelperMock = mockStatic(TimeSeriesHelper.class)) {
                                        timeSeriesHelperMock.when(() -> TimeSeriesHelper.retrieveData(anyString(), any(), any(), any())).thenReturn(timeSeries);

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

    @Test
    public void testValidateInput() {
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

    @Test
    public void testRunCEA() throws Exception {
        try (MockedStatic<StackClient> stackClientMock = mockStatic(StackClient.class)) {
            stackClientMock.when(() -> StackClient.getStackName())
                    .thenReturn("");
            try (MockedConstruction<EndpointConfig> endpointMock = mockConstruction(EndpointConfig.class,
                    (mock, context) -> {
                        doReturn("").when(mock).getDbUrl(anyString());
                        doReturn("").when(mock).getDbUser();
                        doReturn("").when(mock).getDbPassword();
                    })) {
                try (MockedConstruction<RunCEATask> mockTask = mockConstruction(RunCEATask.class)) {

                    ThreadPoolExecutor executor = mock(ThreadPoolExecutor.class);

                    CEAAgent agent = new CEAAgent();
                    Method runCEA = agent.getClass().getDeclaredMethod("runCEA", ArrayList.class, ArrayList.class, Integer.class, String.class, byte[].class);
                    assertNotNull(runCEA);
                    runCEA.setAccessible(true);

                    Field CEAExecutor = agent.getClass().getDeclaredField("CEAExecutor");
                    CEAExecutor.setAccessible(true);
                    CEAExecutor.set(agent, executor);

                    Field targetUrl = agent.getClass().getDeclaredField("targetUrl");
                    targetUrl.setAccessible(true);
                    targetUrl.set(agent, "test");

                    ArrayList<CEAInputData> testData = new ArrayList<CEAInputData>();
                    testData.add(new CEAInputData("test", "test", (Map<String, Double>) new HashMap<>().put("MULTI_RES", 1.00), null, null, null, null));
                    ArrayList<String> testArray = new ArrayList<>();
                    testArray.add("testUri");
                    Integer test_thread = 0;
                    String test_CRS = "27700";

                    // Test executor called with run CEA task
                    runCEA.invoke(agent, testData, testArray, test_thread, test_CRS, null);
                    verify(executor, times(1)).execute(mockTask.constructed().get(0));
                }
            }
        }
    }
}
