package uk.ac.cam.cares.jps.agent.cea;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.jooq.exception.IOException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import kong.unirest.*;
import org.apache.jena.query.Query;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.jooq.exception.DataAccessException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.*;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.agent.ceatasks.CEAInputData;
import uk.ac.cam.cares.jps.agent.ceatasks.RunCEATask;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Field;
import java.net.HttpURLConnection;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;

public class CEAAgentTest {
    DockerImageName myImage = DockerImageName.parse("postgis/postgis:14-3.2").asCompatibleSubstituteFor("postgres");
    @Container
    private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>(myImage);

    @BeforeEach
    public void startContainers() throws IOException {
        try{
            postgres.start();
        }
        catch (Exception e) {
            throw new JPSRuntimeException("Docker container startup failed. Please try running tests again");
        }
    }
    @Test
    public void testCEAAgent() {
        CEAAgent agent;

        try {
            agent = new CEAAgent();
            assertNotNull(agent);
        } catch (Exception e) {
            fail();
        }

    }

    @Test
    public void testCEAAgentFields() {
        CEAAgent agent = new CEAAgent();
        ResourceBundle config = ResourceBundle.getBundle("CEAAgentConfig");

        Field URI_ACTION;
        Field URI_UPDATE;
        Field URI_QUERY;
        Field KEY_REQ_METHOD;
        Field KEY_REQ_URL;
        Field KEY_TARGET_URL;
        Field KEY_IRI;
        Field CITY_OBJECT;
        Field CITY_OBJECT_GEN_ATT;
        Field BUILDING;
        Field ENERGY_PROFILE;
        Field DATABASE_SRS;
        Field SURFACE_GEOMETRY;
        Field THEMATIC_SURFACE;
        Field KEY_GRID_CONSUMPTION;
        Field KEY_ELECTRICITY_CONSUMPTION;
        Field KEY_HEATING_CONSUMPTION;
        Field KEY_COOLING_CONSUMPTION;
        Field KEY_PV_ROOF_SUPPLY;
        Field KEY_PV_WALL_NORTH_SUPPLY;
        Field KEY_PV_WALL_SOUTH_SUPPLY;
        Field KEY_PV_WALL_EAST_SUPPLY;
        Field KEY_PV_WALL_WEST_SUPPLY;
        Field KEY_TIMES;
        Field NUM_CEA_THREADS;
        Field CEAExecutor;
        Field TIME_SERIES_CLIENT_PROPS;
        Field tsClient;
        Field timeUnit;
        Field FS;
        Field rdbStoreClient;
        Field storeClient;
        Field ocgmlUri;
        Field ontoUBEMMPUri;
        Field rdfUri;
        Field owlUri;
        Field unitOntologyUri;
        Field ontoBuiltEnvUri;
        Field accessAgentRoutes;
        Field requestUrl;
        Field targetUrl;
        Field geometryRoute;
        Field usageRoute;
        Field ceaRoute;
        Field namedGraph;
        Field CEA_OUTPUTS;

        try {
            URI_ACTION = agent.getClass().getDeclaredField("URI_ACTION");
            assertEquals(URI_ACTION.get(agent), "/cea/run");
            URI_UPDATE = agent.getClass().getDeclaredField("URI_UPDATE");
            assertEquals(URI_UPDATE.get(agent), "/cea/update");
            URI_QUERY = agent.getClass().getDeclaredField("URI_QUERY");
            assertEquals(URI_QUERY.get(agent), "/cea/query");
            KEY_REQ_METHOD = agent.getClass().getDeclaredField("KEY_REQ_METHOD");
            assertEquals(KEY_REQ_METHOD.get(agent), "method");
            KEY_REQ_URL = agent.getClass().getDeclaredField("KEY_REQ_URL");
            assertEquals(KEY_REQ_URL.get(agent), "requestUrl");
            KEY_TARGET_URL = agent.getClass().getDeclaredField("KEY_TARGET_URL");
            assertEquals(KEY_TARGET_URL.get(agent), "targetUrl");
            KEY_IRI = agent.getClass().getDeclaredField("KEY_IRI");
            assertEquals(KEY_IRI.get(agent), "iris");
            CITY_OBJECT = agent.getClass().getDeclaredField("CITY_OBJECT");
            assertEquals(CITY_OBJECT.get(agent), "cityobject");
            CITY_OBJECT_GEN_ATT = agent.getClass().getDeclaredField("CITY_OBJECT_GEN_ATT");
            assertEquals(CITY_OBJECT_GEN_ATT.get(agent), "cityobjectgenericattrib");
            BUILDING = agent.getClass().getDeclaredField("BUILDING");
            assertEquals(BUILDING.get(agent), "building");
            ENERGY_PROFILE = agent.getClass().getDeclaredField("ENERGY_PROFILE");
            assertEquals(ENERGY_PROFILE.get(agent), "energyprofile");
            DATABASE_SRS = agent.getClass().getDeclaredField("DATABASE_SRS");
            assertEquals(DATABASE_SRS.get(agent), "databasesrs");
            SURFACE_GEOMETRY = agent.getClass().getDeclaredField("SURFACE_GEOMETRY");
            assertEquals(SURFACE_GEOMETRY.get(agent), "surfacegeometry");
            THEMATIC_SURFACE = agent.getClass().getDeclaredField("THEMATIC_SURFACE");
            assertEquals(THEMATIC_SURFACE.get(agent), "thematicsurface");
            KEY_GRID_CONSUMPTION = agent.getClass().getDeclaredField("KEY_GRID_CONSUMPTION");
            assertEquals(KEY_GRID_CONSUMPTION.get(agent), "GridConsumption");
            KEY_ELECTRICITY_CONSUMPTION = agent.getClass().getDeclaredField("KEY_ELECTRICITY_CONSUMPTION");
            assertEquals(KEY_ELECTRICITY_CONSUMPTION.get(agent), "ElectricityConsumption");
            KEY_HEATING_CONSUMPTION = agent.getClass().getDeclaredField("KEY_HEATING_CONSUMPTION");
            assertEquals(KEY_HEATING_CONSUMPTION.get(agent), "HeatingConsumption");
            KEY_COOLING_CONSUMPTION = agent.getClass().getDeclaredField("KEY_COOLING_CONSUMPTION");
            assertEquals(KEY_COOLING_CONSUMPTION.get(agent), "CoolingConsumption");
            KEY_PV_ROOF_SUPPLY = agent.getClass().getDeclaredField("KEY_PV_ROOF_SUPPLY");
            assertEquals(KEY_PV_ROOF_SUPPLY.get(agent), "PVRoofSupply");
            KEY_PV_WALL_NORTH_SUPPLY = agent.getClass().getDeclaredField("KEY_PV_WALL_NORTH_SUPPLY");
            assertEquals(KEY_PV_WALL_NORTH_SUPPLY.get(agent), "PVWallNorthSupply");
            KEY_PV_WALL_SOUTH_SUPPLY = agent.getClass().getDeclaredField("KEY_PV_WALL_SOUTH_SUPPLY");
            assertEquals(KEY_PV_WALL_SOUTH_SUPPLY.get(agent), "PVWallSouthSupply");
            KEY_PV_WALL_EAST_SUPPLY = agent.getClass().getDeclaredField("KEY_PV_WALL_EAST_SUPPLY");
            assertEquals(KEY_PV_WALL_EAST_SUPPLY.get(agent), "PVWallEastSupply");
            KEY_PV_WALL_WEST_SUPPLY = agent.getClass().getDeclaredField("KEY_PV_WALL_WEST_SUPPLY");
            assertEquals(KEY_PV_WALL_WEST_SUPPLY.get(agent), "PVWallWestSupply");
            KEY_TIMES = agent.getClass().getDeclaredField("KEY_TIMES");
            assertEquals(KEY_TIMES.get(agent), "times");
            NUM_CEA_THREADS = agent.getClass().getDeclaredField("NUM_CEA_THREADS");
            assertEquals(NUM_CEA_THREADS.get(agent), 1);
            CEAExecutor = agent.getClass().getDeclaredField("CEAExecutor");
            CEAExecutor.setAccessible(true);
            assertFalse(((ExecutorService) CEAExecutor.get(agent)).isTerminated());
            TIME_SERIES_CLIENT_PROPS = agent.getClass().getDeclaredField("TIME_SERIES_CLIENT_PROPS");
            TIME_SERIES_CLIENT_PROPS.setAccessible(true);
            assertEquals(TIME_SERIES_CLIENT_PROPS.get(agent), "timeseriesclient.properties");
            tsClient = agent.getClass().getDeclaredField("tsClient");
            tsClient.setAccessible(true);
            assertNull(tsClient.get(agent));
            timeUnit = agent.getClass().getDeclaredField("timeUnit");
            assertEquals(timeUnit.get(agent), "OffsetDateTime");
            FS = agent.getClass().getDeclaredField("FS");
            FS.setAccessible(true);
            assertEquals(FS.get(agent), System.getProperty("file.separator"));
            rdbStoreClient = agent.getClass().getDeclaredField("rdbStoreClient");
            rdbStoreClient.setAccessible(true);
            assertNull(rdbStoreClient.get(agent));
            storeClient = agent.getClass().getDeclaredField("storeClient");
            storeClient.setAccessible(true);
            assertNull(storeClient.get(agent));
            geometryRoute = agent.getClass().getDeclaredField("geometryRoute");
            geometryRoute.setAccessible(true);
            assertTrue(geometryRoute.get(agent) == null);
            usageRoute = agent.getClass().getDeclaredField("usageRoute");
            usageRoute.setAccessible(true);
            assertTrue(usageRoute.get(agent) == null);
            ceaRoute = agent.getClass().getDeclaredField("ceaRoute");
            ceaRoute.setAccessible(true);
            assertTrue(ceaRoute.get(agent) == null);
            namedGraph = agent.getClass().getDeclaredField("namedGraph");
            namedGraph.setAccessible(true);
            assertTrue(namedGraph.get(agent) == null);
            CEA_OUTPUTS = agent.getClass().getDeclaredField("CEA_OUTPUTS");
            assertEquals(CEA_OUTPUTS.get(agent), "ceaOutputs");

            // Test readConfig()
            ocgmlUri = agent.getClass().getDeclaredField("ocgmlUri");
            ocgmlUri.setAccessible(true);
            assertEquals(ocgmlUri.get(agent), config.getString("uri.ontology.ontocitygml"));
            ontoUBEMMPUri = agent.getClass().getDeclaredField("ontoUBEMMPUri");
            ontoUBEMMPUri.setAccessible(true);
            assertEquals(ontoUBEMMPUri.get(agent), config.getString("uri.ontology.ontoubemmp"));
            rdfUri = agent.getClass().getDeclaredField("rdfUri");
            rdfUri.setAccessible(true);
            assertEquals(rdfUri.get(agent), config.getString("uri.ontology.rdf"));
            owlUri = agent.getClass().getDeclaredField("owlUri");
            owlUri.setAccessible(true);
            assertEquals(owlUri.get(agent), config.getString("uri.ontology.owl"));
            unitOntologyUri = agent.getClass().getDeclaredField("unitOntologyUri");
            unitOntologyUri.setAccessible(true);
            assertEquals(unitOntologyUri.get(agent), config.getString("uri.ontology.om"));
            ontoBuiltEnvUri = agent.getClass().getDeclaredField("ontobuiltenvUri");
            ontoBuiltEnvUri.setAccessible(true);
            assertEquals(ontoBuiltEnvUri.get(agent), config.getString("uri.ontology.ontobuiltenv"));

            requestUrl = agent.getClass().getDeclaredField("requestUrl");
            requestUrl.setAccessible(true);
            assertNull(requestUrl.get(agent));
            targetUrl = agent.getClass().getDeclaredField("targetUrl");
            targetUrl.setAccessible(true);
            assertNull(targetUrl.get(agent));
            accessAgentRoutes = agent.getClass().getDeclaredField("accessAgentRoutes");
            accessAgentRoutes.setAccessible(true);
            Map<String,String> accessAgentMap = (HashMap<String,String>) accessAgentRoutes.get(agent);
            assertEquals(accessAgentMap.get("http://www.theworldavatar.com:83/citieskg/namespace/berlin/sparql/"), config.getString("berlin.targetresourceid"));
            assertEquals(accessAgentMap.get("http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG24500/sparql/"), config.getString("singaporeEPSG24500.targetresourceid"));
            assertEquals(accessAgentMap.get("http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG4326/sparql/"), config.getString("singaporeEPSG4326.targetresourceid"));
            assertEquals(accessAgentMap.get("http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG3857/sparql/"), config.getString("kingslynnEPSG3857.targetresourceid"));
            assertEquals(accessAgentMap.get("http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/"), config.getString("kingslynnEPSG27700.targetresourceid"));
        } catch (NoSuchFieldException | IllegalAccessException e) {
            fail();
        }
    }

    @Test
    public void testProcessRequestParameters()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException, SQLException {

        CEAAgent agent = spy(new CEAAgent());
        Method processRequestParameters = agent.getClass().getDeclaredMethod("processRequestParameters", JSONObject.class);
        JSONObject requestParams = new JSONObject();

        // set route
        Field geometryRoute = agent.getClass().getDeclaredField("ceaRoute");
        geometryRoute.setAccessible(true);
        geometryRoute.set(agent, "test_route");

        RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
        Connection mockConnection = mock(Connection.class);

        Field rdbStoreClient = agent.getClass().getDeclaredField("rdbStoreClient");
        rdbStoreClient.setAccessible(true);
        rdbStoreClient.set(agent, mockRDBClient);

        doReturn(mockConnection).when(mockRDBClient).getConnection();
        doReturn(new JSONArray()).when(mockRDBClient).executeQuery(anyString());

        // Test empty request params
        try {
            processRequestParameters.invoke(agent, requestParams);
        } catch(Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(),
                    BadRequestException.class);
        }

        // test data
        String measure_building = "building";
        String test_measure = "test_uri1";
        String test_unit = "test_uri2";
        String building = "test_building_uri";
        String measure_height = "HeightMeasuredHeigh";
        String test_height = "5.0";
        String measure_footprint = "geometry";
        String test_footprint = "559267.200000246#313892.7999989044#0.0#559280.5400002463#313892.7999989044#0.0#559280.5400002463#313908.7499989033#0.0#559267.200000246#313908.7499989033#0.0#559267.200000246#313892.7999989044#0.0";
        String measure_datatype = "datatype";
        String test_datatype = "http://localhost/blazegraph/literals/POLYGON-3-15";
        String measure_usage = "BuildingUsage";
        String test_usage1 = "<https://www.theworldavatar.com/kg/ontobuiltenv/Office>";
        String test_usage2 = "<https://www.theworldavatar.com/kg/ontobuiltenv/Office>";
        String measure_share = "UsageShare";
        String test_share = "0.5";
        String measure_crs = "CRS";
        String test_crs = "test_crs";
        String testScalar = "testScalar";
        String test_envelope = "555438.08#305587.27999#-0.6#555484.04#305587.27999#-0.6#555484.04#305614.87999#-0.6#555438.08#305614.87999#-0.6#555438.08#305587.27999#-0.6";


        JSONArray expected_building = new JSONArray().put(new JSONObject().put(measure_building, building));
        JSONArray expected_height = new JSONArray().put(new JSONObject().put(measure_height, test_height));
        JSONArray expected_footprint = new JSONArray().put(new JSONObject().put(measure_footprint, test_footprint).put(measure_datatype, test_datatype));
        JSONArray expected_usage = new JSONArray().put(new JSONObject().put(measure_usage, test_usage1).put(measure_share, test_share)).put(new JSONObject().put(measure_usage, test_usage2).put(measure_share, test_share));
        JSONArray expected_crs = new JSONArray().put(new JSONObject().put(measure_crs, test_crs));
        JSONArray expected_iri = new JSONArray().put(new JSONObject().put("measure", test_measure).put("unit", test_unit));
        JSONArray expected_value = new JSONArray().put(new JSONObject().put("value", testScalar));
        JSONArray expected_envelope = new JSONArray().put(new JSONObject().put("envelope", test_envelope));
        JSONArray expected_buildings = new JSONArray().put(new JSONObject().put("cityObject", "http://www.theworldavatar.com:83/citieskg/namespace/pirmasensEPSG32633/sparql/cityobject/UUID_test/")).put(new JSONObject().put("cityObject", "http://localhost/kings-lynn-open-data/cityobject/UUID_447787a5-1678-4246-8658-4036436c1052/"));

        JSONObject expected_endpoints = new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "test").put(JPSConstants.UPDATE_ENDPOINT, "test");

        // Test the update endpoint
        requestParams.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/update");
        requestParams.put(CEAAgent.KEY_IRI, "['http://www.theworldavatar.com:83/citieskg/namespace/pirmasensEPSG32633/sparql/cityobject/UUID_test/']");
        requestParams.put(CEAAgent.KEY_TARGET_URL, "http://localhost:8086/agents/cea/update");
        requestParams.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.POST);

        JSONArray arrayMock = mock(JSONArray.class);
        when(arrayMock.length()).thenReturn(1);
        when(arrayMock.getString(anyInt())).thenReturn(OffsetDateTime.now().toString()).thenReturn("4.2");
        when(arrayMock.get(anyInt())).thenReturn(arrayMock);

        requestParams.put(CEAAgent.KEY_GRID_CONSUMPTION, arrayMock);
        requestParams.put(CEAAgent.KEY_ELECTRICITY_CONSUMPTION, arrayMock);
        requestParams.put(CEAAgent.KEY_HEATING_CONSUMPTION, arrayMock);
        requestParams.put(CEAAgent.KEY_COOLING_CONSUMPTION, arrayMock);
        requestParams.put(CEAAgent.KEY_PV_ROOF_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PV_WALL_SOUTH_SUPPLY,arrayMock);
        requestParams.put(CEAAgent.KEY_PV_WALL_NORTH_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PV_WALL_EAST_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PV_WALL_WEST_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_ROOF_E_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_ROOF_Q_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY,arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY,arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_EAST_E_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_WEST_E_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_ROOF_E_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_ROOF_Q_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY,arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY,arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_EAST_E_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_WEST_E_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_ROOF_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY,arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_EAST_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_WEST_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_ROOF_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY,arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_EAST_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_WEST_SUPPLY, arrayMock);
        requestParams.put(CEAAgent.KEY_ROOF_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAAgent.KEY_SOUTH_WALL_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAAgent.KEY_NORTH_WALL_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAAgent.KEY_EAST_WALL_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAAgent.KEY_WEST_WALL_SOLAR_SUITABLE_AREA, arrayMock);
        requestParams.put(CEAAgent.KEY_TIMES, arrayMock);

        Field namedGraph = agent.getClass().getDeclaredField("namedGraph");
        namedGraph.setAccessible(true);
        namedGraph.set(agent, "testGraph");

        doNothing().when(agent).updateStore(anyString(), anyString());
        doReturn(0.00).when(agent).getStationOffset(anyDouble(), anyDouble(), any(), any());

        JSONObject returnParams;

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expected_building).thenReturn(expected_iri);

            try (MockedConstruction<TimeSeriesClient> mockTs = mockConstruction(TimeSeriesClient.class)) {

                returnParams = (JSONObject) processRequestParameters.invoke(agent, requestParams);
                verify(mockTs.constructed().get(0), times(1)).addTimeSeriesData(any(), any());
                assertEquals(requestParams, returnParams);

            }

            //Test the run endpoint
            requestParams.remove(CEAAgent.KEY_REQ_URL);
            requestParams.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/run");


            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(new JSONArray()).thenReturn(new JSONArray()).thenReturn(expected_height).thenReturn(expected_footprint)
                    .thenReturn(expected_usage).thenReturn(expected_envelope).thenReturn(expected_buildings).thenReturn(expected_height)
                    .thenReturn(expected_footprint).thenReturn(expected_crs).thenReturn(expected_envelope);

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString())).thenReturn(expected_endpoints);

            try (MockedConstruction<RunCEATask> mockTask = mockConstruction(RunCEATask.class)) {
                ThreadPoolExecutor executor = mock(ThreadPoolExecutor.class);
                Field CEAExecutor = agent.getClass().getDeclaredField("CEAExecutor");
                CEAExecutor.setAccessible(true);
                CEAExecutor.set(agent, executor);

                returnParams = (JSONObject) processRequestParameters.invoke(agent, requestParams);
                verify(executor, times(1)).execute(mockTask.constructed().get(0));
                assertEquals(requestParams, returnParams);
            }


            //Test the query endpoint
            requestParams.remove(CEAAgent.KEY_REQ_URL);
            requestParams.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/query");

            //Test time series data
            String testUnit = "testUnit";
            ArrayList<String> testList = mock(ArrayList.class);
            when(testList.get(0)).thenReturn(testScalar);
            when(testList.get(1)).thenReturn(testUnit);
            String testReturnValue = "testAnnual";
            TimeSeries<OffsetDateTime> timeSeries = mock(TimeSeries.class);

            doReturn(testList).when(agent).getDataIRI(anyString(), anyString(), anyString());
            doReturn(testReturnValue).when(agent).calculateAnnual(any(), anyString());
            doReturn(timeSeries).when(agent).retrieveData(anyString(), any(), any(), any());
            doReturn(testUnit).when(agent).getUnit(anyString());

            Field TIME_SERIES = agent.getClass().getDeclaredField("TIME_SERIES");
            List<String> time_series_strings = (List<String>) TIME_SERIES.get(agent);
            Field SCALARS = agent.getClass().getDeclaredField("SCALARS");
            List<String> scalar_strings = (List<String>) SCALARS.get(agent);

            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expected_building).thenReturn(expected_value);

            returnParams = (JSONObject) processRequestParameters.invoke(agent, requestParams);
            String result = returnParams.get(CEAAgent.CEA_OUTPUTS).toString();
            for (String scalar : scalar_strings) {
                String expected = "\"" + scalar + "\"" + ":\"testScalar testUnit\"";
                assertTrue(result.contains(expected));
            }
            for (String ts : time_series_strings) {
                String expected;
                if (ts.contains("Consumption")){
                    expected = "\"Annual " + ts + "\"" + ":\"testAnnual testUnit\"";
                }
                else {
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

    @Test
    public void testValidateInput() {
        CEAAgent agent = new CEAAgent();
        Method validateInput = null;

        try {
            validateInput = agent.getClass().getDeclaredMethod("validateInput", JSONObject.class);
        } catch (Exception e) {
            fail();
        }

        JSONObject requestParams = new JSONObject();

        // check failure with empty request params
        try {
            validateInput.invoke(agent, requestParams);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(),
                    BadRequestException.class);
        }

        requestParams.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.GET);

        // check failure with no IRI or request url or target url
        try {
            validateInput.invoke(agent, requestParams);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(),
                    BadRequestException.class);
        }
        requestParams.put(CEAAgent.KEY_REQ_URL, "http://localhost:8086/agents/cea/run");

        // check failure with no IRI or target url
        try {
            validateInput.invoke(agent, requestParams);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(),
                    BadRequestException.class);
        }

        requestParams.put(CEAAgent.KEY_IRI, "test");

        // check failure with GET http method
        try {
            validateInput.invoke(agent, requestParams);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(),
                    BadRequestException.class);
        }

        requestParams.put(CEAAgent.KEY_REQ_METHOD, HttpMethod.POST);

        // check failure with no target URL
        try {
            validateInput.invoke(agent, requestParams);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(),
                    BadRequestException.class);
        }

        requestParams.put(CEAAgent.KEY_TARGET_URL, "http://localhost:8086/agents/cea/update");

        // should pass now
        try {
            assertTrue((Boolean) validateInput.invoke(agent, requestParams));
        } catch (Exception e) {
            fail();
        }

    }

    @Test
    public void testValidateUpdateInput()  throws NoSuchMethodException, InvocationTargetException, IllegalAccessException{
        CEAAgent agent = new CEAAgent();
        Method validateUpdateInput  = agent.getClass().getDeclaredMethod("validateUpdateInput", JSONObject.class);
        assertNotNull(validateUpdateInput);
        validateUpdateInput.setAccessible(true);

        JSONObject requestParams = new JSONObject();
        requestParams.put(CEAAgent.KEY_IRI, "");
        requestParams.put(CEAAgent.KEY_TARGET_URL, "");
        requestParams.put(CEAAgent.KEY_GRID_CONSUMPTION, "");
        requestParams.put(CEAAgent.KEY_ELECTRICITY_CONSUMPTION, "");
        requestParams.put(CEAAgent.KEY_HEATING_CONSUMPTION, "");
        requestParams.put(CEAAgent.KEY_COOLING_CONSUMPTION, "");
        requestParams.put(CEAAgent.KEY_PV_ROOF_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PV_WALL_SOUTH_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PV_WALL_NORTH_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PV_WALL_EAST_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PV_WALL_WEST_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_ROOF_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_ROOF_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_EAST_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_WEST_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_ROOF_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_ROOF_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_EAST_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_WEST_E_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_ROOF_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_EAST_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_WEST_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_ROOF_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_EAST_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_WEST_SUPPLY, "");
        requestParams.put(CEAAgent.KEY_TIMES, "");

        // check failure with empty request params
        assertTrue((Boolean) validateUpdateInput.invoke(agent, requestParams));

        requestParams.put(CEAAgent.KEY_IRI, "test");

        // check failure with only IRI
        assertTrue((Boolean) validateUpdateInput.invoke(agent, requestParams));

        requestParams.put(CEAAgent.KEY_TARGET_URL, "http://localhost:8086/agents/cea/update");

        // check failure with only IRI and target url
        assertTrue((Boolean) validateUpdateInput.invoke(agent, requestParams));

        requestParams.put(CEAAgent.KEY_GRID_CONSUMPTION, "test");
        requestParams.put(CEAAgent.KEY_ELECTRICITY_CONSUMPTION, "test");
        requestParams.put(CEAAgent.KEY_HEATING_CONSUMPTION, "test");
        requestParams.put(CEAAgent.KEY_COOLING_CONSUMPTION, "test");
        requestParams.put(CEAAgent.KEY_PV_ROOF_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PV_WALL_SOUTH_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PV_WALL_NORTH_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PV_WALL_EAST_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PV_WALL_WEST_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_ROOF_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_ROOF_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_SOUTH_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_SOUTH_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_NORTH_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_NORTH_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_EAST_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_EAST_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_WEST_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_PLATE_WALL_WEST_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_ROOF_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_ROOF_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_SOUTH_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_SOUTH_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_NORTH_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_NORTH_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_EAST_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_EAST_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_WEST_E_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_PVT_TUBE_WALL_WEST_Q_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_ROOF_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_SOUTH_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_NORTH_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_EAST_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_PLATE_WALL_WEST_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_ROOF_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_SOUTH_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_NORTH_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_EAST_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_THERMAL_TUBE_WALL_WEST_SUPPLY, "test");
        requestParams.put(CEAAgent.KEY_TIMES, "test");

        // should pass now
        assertFalse((Boolean) validateUpdateInput.invoke(agent, requestParams));
    }

    @Test
    public void testValidateActionInput() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method validateActionInput = agent.getClass().getDeclaredMethod("validateActionInput", JSONObject.class);
        assertNotNull(validateActionInput);
        validateActionInput.setAccessible(true);

        JSONObject requestParams = new JSONObject();
        requestParams.put(CEAAgent.KEY_IRI, "");

        // check failure with empty request params
        assertTrue((Boolean) validateActionInput.invoke(agent, requestParams));

        requestParams.put(CEAAgent.KEY_IRI, "test");

        // should pass with only IRI
        assertFalse((Boolean) validateActionInput.invoke(agent, requestParams));
    }

    @Test
    public void testValidateQueryInput() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method validateQueryInput = agent.getClass().getDeclaredMethod("validateQueryInput", JSONObject.class);
        assertNotNull(validateQueryInput);
        validateQueryInput.setAccessible(true);

        JSONObject requestParams = new JSONObject();
        requestParams.put(CEAAgent.KEY_IRI, "");

        // check failure with empty request params
        assertTrue((Boolean) validateQueryInput.invoke(agent, requestParams));

        requestParams.put(CEAAgent.KEY_IRI, "test");

        // should pass now
        assertFalse((Boolean) validateQueryInput.invoke(agent, requestParams));
    }

    @Test
    public void testGetList() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException{
        CEAAgent agent = new CEAAgent();
        Method getList = agent.getClass().getDeclaredMethod("getList", JSONObject.class, String.class);
        assertNotNull(getList);
        getList.setAccessible(true);

        JSONObject requestParams = new JSONObject();
        List<String> test_list_1 = new ArrayList<>();
        test_list_1.add("test_value_1");
        test_list_1.add("test_value_2");
        test_list_1.add("test_value_3");
        List<String> test_list_2 = new ArrayList<>();
        test_list_2.add("test_value_4");
        test_list_2.add("test_value_5");

        requestParams.put("test_key_1", test_list_1);
        requestParams.put("test_key_2", test_list_2);

        // test value list retrieved correctly
        List<String> result = (List<String>) getList.invoke(agent, requestParams, "test_key_2" );
        assertEquals(test_list_2, result);

    }

    @Test
    public void testGetTimeSeriesList() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException{
        CEAAgent agent = new CEAAgent();
        Method getTimeSeriesList = agent.getClass().getDeclaredMethod("getTimeSeriesList", JSONObject.class, String.class, Integer.class);
        assertNotNull(getTimeSeriesList);
        getTimeSeriesList.setAccessible(true);

        JSONObject requestParams = new JSONObject();
        List<String> test_list_1 = new ArrayList<>();
        test_list_1.add("1.5");
        test_list_1.add("2.5");
        test_list_1.add("3.5");
        List<String> test_list_2 = new ArrayList<>();
        test_list_2.add("4.5");
        test_list_2.add("5.5");
        List<List<String>> list_of_lists = new ArrayList<>();
        list_of_lists.add(test_list_1);
        list_of_lists.add(test_list_2);

        requestParams.put("test_key_1", list_of_lists);

        List<Double> expected_list = new ArrayList<>();
        expected_list.add(4.5);
        expected_list.add(5.5);

        // test time series retrieved correctly
        List<Double> result = (List<Double>) getTimeSeriesList.invoke(agent, requestParams, "test_key_1" , 1);
        assertEquals(expected_list, result);

    }

    @Test
    public void testGetTimesList() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException{
        CEAAgent agent = new CEAAgent();
        Method getTimesList = agent.getClass().getDeclaredMethod("getTimesList", JSONObject.class, String.class);
        assertNotNull(getTimesList);
        getTimesList.setAccessible(true);

        JSONObject requestParams = new JSONObject();
        List<OffsetDateTime> test_list_1 = new ArrayList<>();
        test_list_1.add(OffsetDateTime.now());
        test_list_1.add(OffsetDateTime.now());
        test_list_1.add(OffsetDateTime.now());
        List<OffsetDateTime> test_list_2 = new ArrayList<>();
        test_list_2.add(OffsetDateTime.now());
        test_list_2.add(OffsetDateTime.now());

        requestParams.put("test_key_1", test_list_1);
        requestParams.put("test_key_2", test_list_2);

        // test times retrieved correctly
        List<Double> result = (List<Double>) getTimesList.invoke(agent, requestParams, "test_key_1" );
        assertEquals(test_list_1, result);

    }

    @Test
    public void testRunCEA() throws Exception {
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

    @Test
    public void testCreateTimeSeries() throws Exception {
        try (MockedConstruction<TimeSeriesClient> mockTs = mockConstruction(TimeSeriesClient.class)) {

            CEAAgent agent = new CEAAgent();
            Method createTimeSeries = agent.getClass().getDeclaredMethod("createTimeSeries", LinkedHashMap.class, String.class);
            assertNotNull(createTimeSeries);
            createTimeSeries.setAccessible(true);

            LinkedHashMap<String, String> fixedIris = new LinkedHashMap<>();

            RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
            Connection mockConnection = mock(Connection.class);

            Field rdbStoreClient = agent.getClass().getDeclaredField("rdbStoreClient");
            rdbStoreClient.setAccessible(true);
            rdbStoreClient.set(agent, mockRDBClient);

            Field ontoUBEMMPUri = agent.getClass().getDeclaredField("ontoUBEMMPUri");
            ontoUBEMMPUri.setAccessible(true);

            doReturn(mockConnection).when(mockRDBClient).getConnection();

            createTimeSeries.invoke(agent, fixedIris, "");

            Field TIME_SERIES = agent.getClass().getDeclaredField("TIME_SERIES");
            List<String> time_series_strings = (List<String>) TIME_SERIES.get(agent);

            // Ensure iris created correctly and time series initialised
            for (String time_series : time_series_strings) {
                assertTrue(fixedIris.get(time_series).contains(ontoUBEMMPUri.get(agent) + time_series));
            }
            verify(mockTs.constructed().get(0), times(1)).initTimeSeries(anyList(), anyList(), anyString(), any(), any(), any(), any());

            String testGraph = "testGraph";

            createTimeSeries.invoke(agent, fixedIris, testGraph);

            // Ensure iris created correctly and time series initialised
            for (String time_series : time_series_strings) {
                assertTrue(fixedIris.get(time_series).contains(testGraph + time_series));
            }
        }
    }

    @Test
    public void testAddDataToTimeSeries() throws Exception {
        try(MockedConstruction<TimeSeriesClient> mockTs = mockConstruction(TimeSeriesClient.class)) {

            CEAAgent agent = new CEAAgent();
            Method addDataToTimeSeries = agent.getClass().getDeclaredMethod("addDataToTimeSeries", List.class, List.class, LinkedHashMap.class);
            assertNotNull(addDataToTimeSeries);
            addDataToTimeSeries.setAccessible(true);

            LinkedHashMap<String, String> iris = new LinkedHashMap<>();
            iris.put("test_value_1", "test_iri_1");
            iris.put("test_value_2", "test_iri_2");

            List<List<String>> values = new ArrayList<>();
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

            RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
            Connection mockConnection = mock(Connection.class);

            Field rdbStoreClient = agent.getClass().getDeclaredField("rdbStoreClient");
            rdbStoreClient.setAccessible(true);
            rdbStoreClient.set(agent, mockRDBClient);

            doReturn(mockConnection).when(mockRDBClient).getConnection();

            addDataToTimeSeries.invoke(agent, values, times, iris);

            // Ensure correct methods on time series client are called
            verify(mockTs.constructed().get(0), times(1)).getMaxTime(anyString(), any());
            verify(mockTs.constructed().get(0), times(1)).getMinTime(anyString(), any());
            verify(mockTs.constructed().get(0), times(1)).addTimeSeriesData(any(), any());
        }
    }

    @Test
    public void testTimeSeriesExist() throws Exception {
        TimeSeriesClient<OffsetDateTime> client = mock(TimeSeriesClient.class);

        CEAAgent agent = new CEAAgent();
        Method timeSeriesExist = agent.getClass().getDeclaredMethod("timeSeriesExist", List.class);
        assertNotNull(timeSeriesExist);
        timeSeriesExist.setAccessible(true);

        List<String> iris = new ArrayList<>();
        iris.add("test_1");
        iris.add("test_2");

        RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
        Connection mockConnection = mock(Connection.class);

        Field rdbStoreClient = agent.getClass().getDeclaredField("rdbStoreClient");
        rdbStoreClient.setAccessible(true);
        rdbStoreClient.set(agent, mockRDBClient);

        doReturn(mockConnection).when(mockRDBClient).getConnection();

        Field tsClient = agent.getClass().getDeclaredField("tsClient");
        tsClient.setAccessible(true);
        tsClient.set(agent, client);

        when(client.checkDataHasTimeSeries(anyString(), any()))
                .thenReturn(false)
                .thenThrow(new DataAccessException("ERROR: relation \"dbTable\" does not exist"))
                .thenReturn(true);

        // Ensure returns result of checkDataHasTimeSeries
        Boolean result = (Boolean) timeSeriesExist.invoke(agent, iris);
        assertFalse(result);

        result = (Boolean) timeSeriesExist.invoke(agent, iris);
        assertFalse(result);

        result = (Boolean) timeSeriesExist.invoke(agent, iris);
        assertTrue(result);

    }

    @Test
    public void testGetNamespace()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/berlin/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getNamespace = agent.getClass().getDeclaredMethod("getNamespace", String.class);
        assertNotNull(getNamespace);
        getNamespace.setAccessible(true);

        // Ensure namespace is extracted correctly
        String result = (String) getNamespace.invoke(agent, uri);
        assertEquals("http://localhost/berlin/", result);

    }

    @Test
    public void testGetGraph()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/berlin/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";
        String graph = "building";

        Method getGraph = agent.getClass().getDeclaredMethod("getGraph", String.class, String.class);
        assertNotNull(getGraph);
        getGraph.setAccessible(true);

        // Ensure Graph IRI is formed correctly
        String result = (String) getGraph.invoke(agent, uri, graph);
        assertEquals("http://localhost/berlin/building/", result);

    }

    @Test
    public void testGetUUID()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/berlin/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getUUID = agent.getClass().getDeclaredMethod("getUUID", String.class);
        assertNotNull(getUUID);
        getUUID.setAccessible(true);

        // Ensure UUID is extracted correctly
        String result = (String) getUUID.invoke(agent, uri);
        assertEquals("UUID_583747b0-1655-4761-8050-4036436a1052", result);

    }

    @Test
    public void testGetBuildingUri()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/berlin/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getBuildingUri = agent.getClass().getDeclaredMethod("getBuildingUri", String.class);
        assertNotNull(getBuildingUri);
        getBuildingUri.setAccessible(true);

        // Ensure UUID is extracted correctly
        String result = (String) getBuildingUri.invoke(agent, uri);
        assertEquals("http://localhost/berlin/building/UUID_583747b0-1655-4761-8050-4036436a1052/", result);

    }

    @Test
    public void testGetRoute()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

        CEAAgent agent = new CEAAgent();
        String uri = "http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG3857/sparql/cityobject/test_UUID";
        ResourceBundle config = ResourceBundle.getBundle("CEAAgentConfig");

        Method getRoute = agent.getClass().getDeclaredMethod("getRoute", String.class);
        assertNotNull(getRoute);
        getRoute.setAccessible(true);

        // Ensure route is retrieved correctly
        String result = (String) getRoute.invoke(agent, uri);
        assertEquals(config.getString("kingslynnEPSG3857.targetresourceid"), result);

    }

    @Test
    public void testGetValue() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();

        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test/";
        String value = "HeightMeasuredHeight";
        String result = "5.2";
        String route = "test_route";

        JSONArray expected = new JSONArray().put(new JSONObject().put(value, result));
        JSONArray expectedBlank = new JSONArray();

        Method getValue = agent.getClass().getDeclaredMethod("getValue", String.class, String.class, String.class);
        assertNotNull(getValue);
        getValue.setAccessible(true);

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            //test with mocked AccessAgentCaller when it returns a string
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expected);
            assertEquals(result, getValue.invoke(agent, uriString, value, route));

            //test with mocked AccessAgentCaller when there is no string to return
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn((expectedBlank));
            assertEquals("", getValue.invoke(agent,uriString, value, route));
        }
    }

    @Test
    public void getGroundGeometry()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method getGroundGeometry = agent.getClass().getDeclaredMethod("getGroundGeometry", JSONArray.class);

        assertNotNull(getGroundGeometry);
        getGroundGeometry.setAccessible(true);

        String geometry1 = "1.0#1.0#0.0#1.0#2.0#0.0#2.0#2.0#0.0#2.0#1.0#0.0#1.0#1.0#0.0";
        String geometry2 = "1.0#1.0#0.0#1.0#1.0#2.0#2.0#1.0#2.0#2.0#1.0#0.0#1.0#1.0#0.0";
        String geometry3 = "1.0#2.0#0.0#2.0#2.0#0.0#2.0#1.0#0.0#1.0#1.0#0.0#1.0#2.0#0.0";
        String geometry4 = "1.0#2.0#1.0#2.0#2.0#1.0#2.0#1.0#0.0#1.0#1.0#1.0#1.0#2.0#1.0";

        JSONArray testArray = new JSONArray();
        testArray.put(new JSONObject().put("geometry", geometry1));
        testArray.put(new JSONObject().put("geometry", geometry2));
        testArray.put(new JSONObject().put("geometry", geometry3));
        testArray.put(new JSONObject().put("geometry", geometry4));

        JSONArray expected = new JSONArray();
        expected.put(new JSONObject().put("geometry", geometry1));
        expected.put(new JSONObject().put("geometry", geometry3));

        JSONArray result = (JSONArray) getGroundGeometry.invoke(agent, testArray);

        assertEquals(expected.length(), result.length());

        for (int i = 0; i < expected.length(); i++){
            assertEquals(expected.getJSONObject(i).get("geometry").toString(), result.getJSONObject(i).get("geometry").toString());
        }
    }

    @Test
    public void testGetQuery()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

        CEAAgent agent = new CEAAgent();
        String case1 = "Lod0FootprintId";
        String case2 = "FootprintSurfaceGeom";
        String case3 = "FootprintThematicSurface";
        String case4 = "HeightMeasuredHeight";
        String case5 = "HeightMeasuredHeigh";
        String case6 = "HeightGenAttr";
        String case7 = "DatabasesrsCRS";
        String case8 = "CRS";
        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getQuery = agent.getClass().getDeclaredMethod("getQuery", String.class, String.class);
        assertNotNull(getQuery);
        getQuery.setAccessible(true);

        // Ensure queries contains correct predicates depending on value sent
        Query q1 = (Query) getQuery.invoke(agent, uri, case1);
        assertTrue(q1.toString().contains("lod0FootprintId"));
        Query q2 = (Query) getQuery.invoke(agent, uri, case2);
        assertTrue(q2.toString().contains("ocgml:GeometryType"));
        Query q3 = (Query) getQuery.invoke(agent, uri, case3);
        assertTrue(q3.toString().contains("ocgml:objectClassId"));
        Query q4 = (Query) getQuery.invoke(agent, uri, case4);
        assertTrue(q4.toString().contains("ocgml:measuredHeight"));
        Query q5 = (Query) getQuery.invoke(agent, uri, case5);
        assertTrue(q5.toString().contains("ocgml:measuredHeigh"));
        Query q6 = (Query) getQuery.invoke(agent, uri, case6);
        assertTrue(q6.toString().contains("ocgml:attrName"));
        Query q7 = (Query) getQuery.invoke(agent, uri, case7);
        assertTrue(q7.toString().contains("ocgml:srid"));
        Query q8 = (Query) getQuery.invoke(agent, uri, case8);
        assertTrue(q8.toString().contains("srid"));
    }

    @Test
    public void testGetGeometryQueryThematicSurface()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/berlin/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getGeometryQueryThematicSurface = agent.getClass().getDeclaredMethod("getGeometryQueryThematicSurface", String.class);
        assertNotNull(getGeometryQueryThematicSurface);
        getGeometryQueryThematicSurface.setAccessible(true);

        // Ensure query contains correct predicate and object
        Query q = (Query) getGeometryQueryThematicSurface.invoke(agent, uri);
        assertTrue(q.toString().contains("ocgml:GeometryType"));
        assertTrue(q.toString().contains("geometry"));
        assertTrue(q.toString().contains("ocgml:objectClassId"));
        assertTrue(q.toString().contains("groundSurfId"));
        assertTrue(q.toString().contains("datatype"));
    }

    @Test
    public void testGetGeometryQuerySurfaceGeom()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/berlin/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getGeometryQuerySurfaceGeom = agent.getClass().getDeclaredMethod("getGeometryQuerySurfaceGeom", String.class);
        assertNotNull(getGeometryQuerySurfaceGeom);
        getGeometryQuerySurfaceGeom.setAccessible(true);

        // Ensure query contains correct predicates and objects
        Query q = (Query) getGeometryQuerySurfaceGeom.invoke(agent, uri);
        assertTrue(q.toString().contains("ocgml:GeometryType"));
        assertTrue(q.toString().contains("geometry"));
        assertTrue(q.toString().contains("datatype"));
    }

    @Test
    public void testGetHeightQueryMeasuredHeigh()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getHeightQueryMeasuredHeigh = agent.getClass().getDeclaredMethod("getHeightQueryMeasuredHeigh", String.class);
        assertNotNull(getHeightQueryMeasuredHeigh);
        getHeightQueryMeasuredHeigh.setAccessible(true);

        // Ensure query contains correct predicate and object
        Query q = (Query) getHeightQueryMeasuredHeigh.invoke(agent, uri);
        assertTrue(q.toString().contains("ocgml:measuredHeigh"));
        assertTrue(q.toString().contains("HeightMeasuredHeigh"));
    }

    @Test
    public void testGetHeightQueryMeasuredHeight()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getHeightQueryMeasuredHeight = agent.getClass().getDeclaredMethod("getHeightQueryMeasuredHeight", String.class);
        assertNotNull(getHeightQueryMeasuredHeight);
        getHeightQueryMeasuredHeight.setAccessible(true);

        // Ensure query contains correct predicate and object
        Query q = (Query) getHeightQueryMeasuredHeight.invoke(agent, uri);
        assertTrue(q.toString().contains("ocgml:measuredHeight"));
        assertTrue(q.toString().contains("HeightMeasuredHeight"));
    }

    @Test
    public void testGetHeightQueryGenAttr()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getHeightQueryGenAttr = agent.getClass().getDeclaredMethod("getHeightQueryGenAttr", String.class);
        assertNotNull(getHeightQueryGenAttr);
        getHeightQueryGenAttr.setAccessible(true);

        // Ensure query contains correct predicate and object
        Query q = (Query) getHeightQueryGenAttr.invoke(agent, uri);
        assertTrue(q.toString().contains("ocgml:attrName"));
        assertTrue(q.toString().contains("ocgml:realVal"));
        assertTrue(q.toString().contains("HeightGenAttr"));
    }

    @Test
    public void testGetDatabasesrsCrsQuery()
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getDatabasesrsCrsQuery = agent.getClass().getDeclaredMethod("getDatabasesrsCrsQuery", String.class);
        assertNotNull(getDatabasesrsCrsQuery);
        getDatabasesrsCrsQuery.setAccessible(true);

        // Ensure query contains correct predicate and object
        Query q = (Query) getDatabasesrsCrsQuery.invoke(agent, uri);
        assertTrue(q.toString().contains("ocgml:srid"));
        assertTrue(q.toString().contains("CRS"));
    }

    @Test
    public void testAddBuildingConsumptionWhere() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        CEAAgent agent = new CEAAgent();
        Method addBuildingConsumptionWhere = agent.getClass().getDeclaredMethod("addBuildingConsumptionWhere", WhereBuilder.class, String.class);
        assertNotNull(addBuildingConsumptionWhere);

        String energyType = "test_type";

        WhereBuilder wb = new WhereBuilder();
        Field rdf = agent.getClass().getDeclaredField("rdfUri");
        rdf.setAccessible(true);
        String rdfUri = (String) rdf.get(agent);
        Field unitOntology = agent.getClass().getDeclaredField("unitOntologyUri");
        unitOntology.setAccessible(true);
        String unitOntologyUri = (String) unitOntology.get(agent);
        Field ontoUBEMMP = agent.getClass().getDeclaredField("ontoUBEMMPUri");
        ontoUBEMMP.setAccessible(true);
        String ontoUBEMMPUri = (String) ontoUBEMMP.get(agent);

        wb.addPrefix("rdf", rdfUri)
                .addPrefix("om", unitOntologyUri)
                .addPrefix("ontoubemmp", ontoUBEMMPUri);

        addBuildingConsumptionWhere.invoke(agent,  wb, energyType );

        String result = wb.build().toString().replaceAll("\\s", "");

        //test string contains expected where data
        String expected_where = "WHERE";
        String expected_triple = "?grid" + "rdf:type\"" + energyType +"\"";
        assertTrue( result.contains(expected_where));
        assertTrue( result.contains(expected_triple));
    }

    @Test
    public void testAddSupplyDeviceWhere() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        CEAAgent agent = new CEAAgent();
        Method addSupplyDeviceWhere = agent.getClass().getDeclaredMethod("addSupplyDeviceWhere", WhereBuilder.class, String.class, String.class, String.class);
        assertNotNull(addSupplyDeviceWhere);

        String facade = "test_facade";
        String panelType = "test_type";
        String energyType = "test_energy";

        WhereBuilder wb = new WhereBuilder();
        Field rdf = agent.getClass().getDeclaredField("rdfUri");
        rdf.setAccessible(true);
        String rdfUri = (String) rdf.get(agent);
        Field unitOntology = agent.getClass().getDeclaredField("unitOntologyUri");
        unitOntology.setAccessible(true);
        String unitOntologyUri = (String) unitOntology.get(agent);
        Field ontoUBEMMP = agent.getClass().getDeclaredField("ontoUBEMMPUri");
        ontoUBEMMP.setAccessible(true);
        String ontoUBEMMPUri = (String) ontoUBEMMP.get(agent);
        Field ontobuiltstructure = agent.getClass().getDeclaredField("ontobuiltstructureUri");
        ontobuiltstructure.setAccessible(true);
        String ontobuiltstructureUri = (String) ontobuiltstructure.get(agent);

        wb.addPrefix("rdf", rdfUri)
                .addPrefix("om", unitOntologyUri)
                .addPrefix("obs", ontobuiltstructureUri)
                .addPrefix("ontoubemmp", ontoUBEMMPUri);

        addSupplyDeviceWhere.invoke(agent, wb, panelType, energyType, facade);

        String result = wb.build().toString().replaceAll("\\s", "");

        //test string contains expected where data
        String expected_where = "WHERE";
        String expected_panel = "?SolarGenerators" + "rdf:type\"" + panelType +"\"";
        String expected_energy = "?supply" + "rdf:type\"" + energyType +"\"";
        assertTrue(result.contains(facade));
        assertTrue(result.contains(expected_where));
        assertTrue(result.contains(expected_panel));
        assertTrue(result.contains(expected_energy));
    }

    @Test
    public void testAddSupplyDeviceAreaWhere() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        CEAAgent agent = new CEAAgent();
        Method addSupplyDeviceAreaWhere = agent.getClass().getDeclaredMethod("addSupplyDeviceAreaWhere", WhereBuilder.class, String.class, String.class);
        assertNotNull(addSupplyDeviceAreaWhere);

        String building = "building_IRI";
        String facadeType = "test_facade";

        WhereBuilder wb = new WhereBuilder();

        Field rdf = agent.getClass().getDeclaredField("rdfUri");
        rdf.setAccessible(true);
        String rdfUri = (String) rdf.get(agent);
        Field unitOntology = agent.getClass().getDeclaredField("unitOntologyUri");
        unitOntology.setAccessible(true);
        String unitOntologyUri = (String) unitOntology.get(agent);
        Field ontobuiltstructure = agent.getClass().getDeclaredField("ontobuiltstructureUri");
        ontobuiltstructure.setAccessible(true);
        String ontobuiltstructureUri = (String) ontobuiltstructure.get(agent);
        Field ontoUBEMMP = agent.getClass().getDeclaredField("ontoUBEMMPUri");
        ontoUBEMMP.setAccessible(true);
        String ontoUBEMMPUri = (String) ontoUBEMMP.get(agent);

        wb.addPrefix("rdf", rdfUri)
                .addPrefix("om", unitOntologyUri)
                .addPrefix("obs", ontobuiltstructureUri)
                .addPrefix("ontoubemmp", ontoUBEMMPUri);

        addSupplyDeviceAreaWhere.invoke(agent,  wb, building, facadeType);

        String result = wb.build().toString().replaceAll("\\s", "");

        //test string contains expected where data
        String expected_where = "WHERE";
        String expected_facade = "obs:hasFacade" + "?facade";
        String expected_facadeType = "?facade" + "rdf:type\"" + facadeType + "\"";
        assertTrue( result.contains(expected_where));
        assertTrue( result.contains(expected_facade));
        assertTrue( result.contains(expected_facadeType));
    }

    @Test
    public void testInitialiseData() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = spy(new CEAAgent());
        Method initialiseData = agent.getClass().getDeclaredMethod("initialiseData", Integer.class, LinkedHashMap.class, String.class, LinkedHashMap.class, LinkedHashMap.class, String.class, String.class);
        assertNotNull(initialiseData);

        LinkedHashMap<String,String> ts_iris_mock = mock(LinkedHashMap.class);
        when(ts_iris_mock.get(anyString())).thenReturn("test");

        LinkedHashMap<String,String> scalar_iris_mock = mock(LinkedHashMap.class);
        when(scalar_iris_mock.get(anyString())).thenReturn("test");

        LinkedHashMap<String,List<String>> scalars_mock = mock(LinkedHashMap.class);
        List<String> test_scalars = new ArrayList<>();
        test_scalars.add("test");
        when(scalars_mock.get(anyString())).thenReturn(test_scalars);

        String route = "test_route";

        Integer testCounter = 0;
        String building = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/energyprofile/Building_UUID_test/";

        doNothing().when(agent).updateStore(anyString(), anyString());
        initialiseData.invoke(agent, testCounter, scalars_mock, building, ts_iris_mock, scalar_iris_mock, route, "");

        //test update store is called once
        verify(agent, times(1)).updateStore(anyString(), anyString());
    }

    @Test
    public void testUpdateScalars() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        CEAAgent agent = spy(new CEAAgent());
        Method updateScalars = agent.getClass().getDeclaredMethod("updateScalars", String.class, LinkedHashMap.class, LinkedHashMap.class, Integer.class, String.class);
        assertNotNull(updateScalars);

        LinkedHashMap<String,String> scalar_iris_mock = mock(LinkedHashMap.class);
        when(scalar_iris_mock.get(anyString())).thenReturn("test");

        LinkedHashMap<String,List<String>> scalars_mock = mock(LinkedHashMap.class);
        List<String> test_scalars = new ArrayList<>();
        test_scalars.add("test");
        when(scalars_mock.get(anyString())).thenReturn(test_scalars);

        String route = "test_route";

        Integer testCounter = 0;

        doNothing().when(agent).updateStore(anyString(), anyString());
        updateScalars.invoke(agent, route,scalar_iris_mock, scalars_mock, testCounter, "");

        //test update store is called twice for each scalar
        Field SCALARS = agent.getClass().getDeclaredField("SCALARS");
        List<String> scalar_strings = (List<String>) SCALARS.get(agent);
        Integer expected = scalar_strings.size()*2;
        verify(agent, times(expected)).updateStore(anyString(), anyString());
    }

    @Test
    public void testGetDataIRI() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();

        String test_value = "PVRoofSupply";
        String measure = "measure";
        String test_measure = "testUri";
        String unit = "unit";
        String test_unit = "kWh";
        String route = "test_route";
        String building = "test_building";

        JSONArray expected = new JSONArray().put(new JSONObject().put(measure, test_measure).put(unit, test_unit));
        JSONArray expectedBlank = new JSONArray();

        Method getDataIRI = agent.getClass().getDeclaredMethod("getDataIRI", String.class, String.class, String.class);
        assertNotNull(getDataIRI);
        getDataIRI.setAccessible(true);

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            //test with mocked AccessAgentCaller when it returns data
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expected);
            ArrayList<String> result = (ArrayList<String>) getDataIRI.invoke(agent, building, test_value, route);
            assertTrue(result.contains(test_measure));
            assertTrue(result.contains(test_unit));

            //test with mocked AccessAgentCaller when there is nothing returned
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn((expectedBlank));
            result = (ArrayList<String>) getDataIRI.invoke(agent, building, test_value, route);

            assertTrue(result.isEmpty());
        }
    }

    @Test
    public void testGetNumericalValue() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();

        String test_measure = "testUri";
        String value = "value";
        String test_value = "35.2";
        String route = "test_route";

        JSONArray expected = new JSONArray().put(new JSONObject().put(value, test_value));
        JSONArray expectedBlank = new JSONArray();

        Method getNumericalValue = agent.getClass().getDeclaredMethod("getNumericalValue", String.class, String.class, String.class);
        assertNotNull(getNumericalValue);
        getNumericalValue.setAccessible(true);

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            //test with mocked AccessAgentCaller when it returns data
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expected);
            String result = (String) getNumericalValue.invoke(agent, test_measure, route, "");
            assertTrue(result.contains(test_value));

            //test with mocked AccessAgentCaller when there is nothing returned
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn((expectedBlank));
            result = (String) getNumericalValue.invoke(agent, test_measure, route, "");

            assertTrue(result.isEmpty());
        }
    }

    @Test
    public void testCheckBuildingInitialised() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();

        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test/";
        String measure = "building";
        String test_measure = "test_uri";
        String route = "test_route";

        JSONArray expected = new JSONArray().put(new JSONObject().put(measure, test_measure));
        JSONArray expectedBlank = new JSONArray();

        Method checkBuildingInitialised = agent.getClass().getDeclaredMethod("checkBuildingInitialised", String.class, String.class);
        assertNotNull(checkBuildingInitialised);
        checkBuildingInitialised.setAccessible(true);

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            //test with mocked AccessAgentCaller when it returns a string.
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(expected);
            assertEquals(test_measure, checkBuildingInitialised.invoke(agent, uriString, route));

            //test with mocked AccessAgentCaller when there is no string to return.
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn((expectedBlank));
            assertEquals("", checkBuildingInitialised.invoke(agent, uriString, route));
        }
    }

    @Test
    public void testInitialiseBuilding() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        CEAAgent agent = spy(new CEAAgent());
        Method initialiseBuilding = agent.getClass().getDeclaredMethod("initialiseBuilding", String.class, String.class, String.class, String.class);
        assertNotNull(initialiseBuilding);

        String route = "test_route";

        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test/";
        Field ontoBuiltEnvUri = agent.getClass().getDeclaredField("ontobuiltenvUri");
        ontoBuiltEnvUri.setAccessible(true);
        String expected = ontoBuiltEnvUri.get(agent) + "Building";

        doNothing().when(agent).updateStore(anyString(), anyString());
        String result = (String) initialiseBuilding.invoke(agent,  uriString, "", route, "");

        //test string contains correct graph and update store is called once
        assertTrue( result.contains(expected));
        verify(agent, times(1)).updateStore(anyString(), anyString());
    }

    @Test
    public void testCreateConsumptionUpdate() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method createConsumptionUpdate = agent.getClass().getDeclaredMethod("createConsumptionUpdate", WhereBuilder.class, String.class, String.class, String.class, String.class);
        assertNotNull(createConsumptionUpdate);

        String consumer = "consumer_IRI";
        String type = "test_type";
        String quantity = "quantity_IRI";
        String measure = "measure_IRI";

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("rdf", "rdfUri")
                .addPrefix("ontoubemmp", "ontoubemmpUri")
                .addPrefix("om", "unitOntologyUri");

        createConsumptionUpdate.invoke(agent,  wb, consumer, type, quantity, measure );

        String result = wb.build().toString();

        assertTrue(result.contains("ontoubemmp:consumesEnergy"));
        assertTrue(result.contains("om:hasValue"));
        assertTrue(result.contains(consumer));
        assertTrue(result.contains(type));
        assertTrue(result.contains(quantity));
        assertTrue(result.contains(measure));
    }

    @Test
    public void testCreateSolarGeneratorSupplyUpdate() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method createSolarGeneratorSupplyUpdate = agent.getClass().getDeclaredMethod("createSolarGeneratorSupplyUpdate", WhereBuilder.class, String.class, String.class, String.class, String.class, String.class, String.class);
        assertNotNull(createSolarGeneratorSupplyUpdate);

        String facade = "facade_IRI";
        String PVPanels = "panels_IRI";
        String generatorType = "PV";
        String quantity = "quantity_IRI";
        String measure = "measure_IRI";

        WhereBuilder wb =
                new WhereBuilder()
                        .addPrefix("rdf", "rdfUri")
                        .addPrefix("om", "unitOntologyUri")
                        .addPrefix("ontoubemmp", "ontoubemmpUri");

        createSolarGeneratorSupplyUpdate.invoke(agent,  wb, facade, PVPanels, generatorType, quantity, measure, "test_energy");

        String result = wb.build().toString();

        assertTrue(result.contains("producesEnergy"));
        assertTrue(result.contains("hasValue"));
        assertTrue(result.contains(facade));
        assertTrue(result.contains(PVPanels));
        assertTrue(result.contains(generatorType));
        assertTrue(result.contains(quantity));
        assertTrue(result.contains(measure));
        assertTrue(result.contains("test_energy"));
    }

    @Test
    public void testCreateSolarSuitableAreaUpdate() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method createSolarGeneratorAreaUpdate = agent.getClass().getDeclaredMethod("createSolarSuitableAreaUpdate", WhereBuilder.class, String.class, String.class, String.class, String.class);
        assertNotNull(createSolarGeneratorAreaUpdate);

        String facade = "facade_IRI";
        String value = "test_value";
        String quantity = "quantity_IRI";
        String measure = "measure_IRI";

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoubemmp", "ontoUBEMMPUri")
                .addPrefix("rdf", "rdfUri")
                .addPrefix("om", "unitOntologyUri");

        createSolarGeneratorAreaUpdate.invoke(agent,  wb, facade, quantity, measure, value);

        String result = wb.build().toString();

        assertTrue( result.contains("om:hasDimension"));
        assertTrue( result.contains("om:hasValue"));
        assertTrue( result.contains("om:hasNumericalValue"));
        assertTrue( result.contains("om:hasUnit"));
        assertTrue( result.contains("om:squareMetre"));
        assertTrue(result.contains(facade));
        assertTrue(result.contains(quantity));
        assertTrue(result.contains(measure));
        assertTrue(result.contains(value));
    }

    @Test
    public void testCheckDataInitialised() throws NoSuchFieldException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = spy(new CEAAgent());
        Method checkDataInitialised = agent.getClass().getDeclaredMethod("checkDataInitialised", String.class, LinkedHashMap.class, LinkedHashMap.class, String.class, String.class);
        assertNotNull(checkDataInitialised);

        //Test time series data
        String testUnit = "testUnit";
        String testIri = "testIri";
        String testBuilding = "testBuilding";
        String route = "test_route";
        ArrayList<String> testList = mock(ArrayList.class);
        when(testList.get(0)).thenReturn(testIri);
        when(testList.get(1)).thenReturn(testUnit);
        doReturn(testList).when(agent).getDataIRI(anyString(), anyString(), anyString());

        LinkedHashMap<String, String> tsIris = new LinkedHashMap();
        LinkedHashMap<String, String> scalarIris = new LinkedHashMap();

        Field TIME_SERIES = agent.getClass().getDeclaredField("TIME_SERIES");
        List<String> time_series_strings = (List<String>) TIME_SERIES.get(agent);
        Field SCALARS = agent.getClass().getDeclaredField("SCALARS");
        List<String> scalar_strings = (List<String>) SCALARS.get(agent);

        Boolean result = (Boolean) checkDataInitialised.invoke(agent, testBuilding, tsIris, scalarIris, route, "");
        assertTrue(result);
        for (String scalar : scalar_strings) {
            assertTrue(scalarIris.get(scalar).contains(testIri));
        }
        for (String ts : time_series_strings) {
            assertTrue(tsIris.get(ts).contains(testIri));
        }
    }

    @Test
    public void testGetUnit() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method getUnit = agent.getClass().getDeclaredMethod("getUnit", String.class);
        assertNotNull(getUnit);

        String test_kwh = "http://www.ontology-of-units-of-measure.org/resource/om-2/kilowattHour";
        String test_m2 = "http://www.ontology-of-units-of-measure.org/resource/om-2/squareMetre";
        String test_other = "test";

        // Ensure units retrieved correctly
        assertEquals(getUnit.invoke(agent, test_kwh), "kWh");
        assertEquals(getUnit.invoke(agent, test_m2), "m^2");
        assertEquals(getUnit.invoke(agent, test_other), "");
    }

    @Test
    public void testRetrieveData() throws Exception {
        try(MockedConstruction<TimeSeriesClient> mockTs = mockConstruction(TimeSeriesClient.class)) {
            CEAAgent agent = new CEAAgent();
            Method retrieveData = agent.getClass().getDeclaredMethod("retrieveData", String.class, RemoteStoreClient.class, RemoteRDBStoreClient.class, Class.class);
            assertNotNull(retrieveData);

            String iri = "test";
            List<String> iris = new ArrayList<>();
            iris.add(iri);

            TimeSeries<Instant> mockT = mock(TimeSeries.class);

            RemoteStoreClient mockStoreClient = mock(RemoteStoreClient.class);

            RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
            Connection mockConnection = mock(Connection.class);

            doReturn(mockConnection).when(mockRDBClient).getConnection();

            retrieveData.invoke(agent, iri, mockStoreClient, mockRDBClient, Instant.class);

            // Ensure method to get time series client was invoked once
            verify(mockTs.constructed().get(0), times(1)).getTimeSeries(anyList(), any());
        }
    }

    @Test
    public void testCalculateAnnual() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException  {
        CEAAgent agent = new CEAAgent();
        Method calculateAnnual = agent.getClass().getDeclaredMethod("calculateAnnual", TimeSeries.class, String.class);
        assertNotNull(calculateAnnual);

        List<String> iris = new ArrayList<>();
        String iri1 = "test_iri_1";
        String iri2 = "test_iri_2";
        iris.add( iri1);
        iris.add( iri2);

        Double value1 = 1.687;
        Double value2 = 2.141;
        Double value3 = 3.621;
        Double value4 = 4.7;

        List<List<?>> values = new ArrayList<>();
        List<Double> test_list_1 = new ArrayList<>();
        test_list_1.add(value1);
        test_list_1.add(value2);
        List<Double> test_list_2 = new ArrayList<>();
        test_list_2.add(value3);
        test_list_2.add(value4);
        values.add(test_list_1);
        values.add(test_list_2);

        List<OffsetDateTime> times = new ArrayList<>();
        times.add(OffsetDateTime.now());
        times.add(OffsetDateTime.now());
        TimeSeries<OffsetDateTime> timeSeries = new TimeSeries<>(times, iris, values);

        Double expected1= 3.83;
        Double expected2= 8.32;

        // Ensure values in time series are summed and rounded correctly
        assertEquals(calculateAnnual.invoke(agent, timeSeries, iri1), expected1.toString());
        assertEquals(calculateAnnual.invoke(agent, timeSeries, iri2), expected2.toString());
    }

    @Test
    public void testExtractFootprint() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, ParseException {
        CEAAgent agent = new CEAAgent();
        Method extractFootprint = agent.getClass().getDeclaredMethod("extractFootprint", JSONArray.class);

        assertNotNull(extractFootprint);
        extractFootprint.setAccessible(true);

        String geometry1 = "1.0#1.0#0.0#2.0#1.0#0.0#2.0#2.0#0.0#1.0#1.0#0.0";
        String geometry2 = "1.0#1.0#0.0#1.0#2.0#0.0#2.0#2.0#0.0#1.0#1.0#0.0";
        String polygonType = "http://localhost/blazegraph/literals/POLYGON-3-12";

        JSONArray testArray = new JSONArray();
        testArray.put(new JSONObject().put("geometry", geometry1).put("datatype", polygonType));
        testArray.put(new JSONObject().put("geometry", geometry2).put("datatype", polygonType));

        String result = (String) extractFootprint.invoke(agent, testArray);

        WKTReader wktReader = new WKTReader();

        Geometry geometry = wktReader.read(result);

        Coordinate[] coordinates = geometry.getCoordinates();

        assertEquals(coordinates.length, 5);
    }

    @Test
    public void testToPolygon() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method toPolygon = agent.getClass().getDeclaredMethod("toPolygon", String.class);

        assertNotNull(toPolygon);
        toPolygon.setAccessible(true);

        String points = "559267.200000246#313892.7999989044#0.0#559280.5400002463#313892.7999989044#0.0#559280.5400002463#313908.7499989033#0.0#559267.200000246#313908.7499989033#0.0#559267.200000246#313892.7999989044#0.0";

        Polygon result = (Polygon) toPolygon.invoke(agent, points);

        String expected = "POLYGON ((559267.200000246 313892.7999989044, 559280.5400002463 313892.7999989044, 559280.5400002463 313908.7499989033, 559267.200000246 313908.7499989033, 559267.200000246 313892.7999989044))";

        assertEquals(expected, result.toString());

    }

    @Test
    public void testCoordinatesToString() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method coordinatesToString = agent.getClass().getDeclaredMethod("coordinatesToString", Coordinate[].class);

        assertNotNull(coordinatesToString);
        coordinatesToString.setAccessible(true);

        Coordinate[] coordinates = new Coordinate[2];

        coordinates[0] = new Coordinate(1.0, 2.0, 3.0);
        coordinates[1] = new Coordinate(4.0, 5.0, 6.0);

        String expected = "1.0#2.0#3.0#4.0#5.0#6.0";

        assertEquals(expected, coordinatesToString.invoke(agent, new Object[] {coordinates}));
    }

    @Test
    public void testInflatePolygon() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method inflatePolygon = agent.getClass().getDeclaredMethod("inflatePolygon", Geometry.class, Double.class);

        assertNotNull(inflatePolygon);
        inflatePolygon.setAccessible(true);

        GeometryFactory gF = new GeometryFactory();
        Coordinate[] testC = new Coordinate[5];

        testC[0] = new Coordinate(1.0, 1.0, 3.01);
        testC[1] = new Coordinate(2.0, 1.0, 3.02);
        testC[2] = new Coordinate(2.0, 2.0, 3.03);
        testC[3] = new Coordinate(1.0, 2.0, 3.03);
        testC[4] = new Coordinate(1.0, 1.0, 3.01);

        Polygon testPolygon = gF.createPolygon(testC);

        Coordinate[] expectedC = new Coordinate[5];

        expectedC[0] = new Coordinate(0.9, 0.9, 3.01);
        expectedC[1] = new Coordinate(2.1, 0.9, 3.02);
        expectedC[2] = new Coordinate(2.1, 2.1, 3.03);
        expectedC[3] = new Coordinate(0.9, 2.1, 3.03);
        expectedC[4] = new Coordinate(0.9, 0.9, 3.01);

        Polygon expectedPolygon = gF.createPolygon(expectedC);

        Polygon resultPolygon = (Polygon) inflatePolygon.invoke(agent, testPolygon, 0.1);
        assertTrue(expectedPolygon.equals(resultPolygon));
    }

    @Test
    public void testDeflatePolygon() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        Method deflatePolygon = agent.getClass().getDeclaredMethod("deflatePolygon", Geometry.class, Double.class);

        assertNotNull(deflatePolygon);
        deflatePolygon.setAccessible(true);

        GeometryFactory gF = new GeometryFactory();
        Coordinate[] testC = new Coordinate[5];

        testC[0] = new Coordinate(1.0, 1.0, 3.01);
        testC[1] = new Coordinate(2.0, 1.0, 3.02);
        testC[2] = new Coordinate(2.0, 2.0, 3.03);
        testC[3] = new Coordinate(1.0, 2.0, 3.03);
        testC[4] = new Coordinate(1.0, 1.0, 3.01);

        Polygon testPolygon = gF.createPolygon(testC);

        Coordinate[] expectedC = new Coordinate[5];

        expectedC[0] = new Coordinate(1.1, 1.1, 3.01);
        expectedC[1] = new Coordinate(1.9, 1.1, 3.02);
        expectedC[2] = new Coordinate(1.9, 1.9, 3.03);
        expectedC[3] = new Coordinate(1.1, 1.9, 3.03);
        expectedC[4] = new Coordinate(1.1, 1.1, 3.01);

        Polygon expectedPolygon = gF.createPolygon(expectedC);

        Polygon resultPolygon = (Polygon) deflatePolygon.invoke(agent, testPolygon, 0.1);
        assertTrue(expectedPolygon.equals(resultPolygon));
    }

    @Test
    public void testToCEAConvention() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException
    {
        CEAAgent agent = new CEAAgent();
        String test_usage = "test";

        Method toCEAConvention = agent.getClass().getDeclaredMethod("toCEAConvention", String.class);
        assertNotNull(toCEAConvention);

        String result = (String) toCEAConvention.invoke(agent, test_usage);
        assertTrue(result.equals("MULTI_RES"));
    }

    @Test
    public void testGetEnvelopeQuery() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getEnvelopeQuery = agent.getClass().getDeclaredMethod("getEnvelopeQuery", String.class);
        assertNotNull(getEnvelopeQuery);
        getEnvelopeQuery.setAccessible(true);

        Query q = (Query) getEnvelopeQuery.invoke(agent, uri);

        assertTrue(q.toString().contains("EnvelopeType"));
    }

    @Test
    public void testGetBuildingsWithinBoundsQuery() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        CEAAgent agent = new CEAAgent();
        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getBuildingsWithinBoundsQuery = agent.getClass().getDeclaredMethod("getBuildingsWithinBoundsQuery", String.class, String.class, String.class);
        assertNotNull(getBuildingsWithinBoundsQuery);
        getBuildingsWithinBoundsQuery.setAccessible(true);

        Query q = (Query) getBuildingsWithinBoundsQuery.invoke(agent, uri, "test", "test");

        Field customDataType = agent.getClass().getDeclaredField("customDataType");
        Field customField = agent.getClass().getDeclaredField("customField");

        assertTrue(q.toString().contains("predicate"));
        assertTrue(q.toString().contains("searchDatatype"));
        assertTrue(q.toString().contains("customFields"));
        assertTrue(q.toString().contains("customFieldsLowerBounds"));
        assertTrue(q.toString().contains("customFieldsUpperBounds"));
        assertTrue(q.toString().contains((String) customDataType.get(agent)));
        assertTrue(q.toString().contains((String) customField.get(agent)));
    }

    @Test
    public void testGetSurroundings() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = spy(new CEAAgent());
        String uri = "http://localhost/kings-lynn-open-data/cityobject/UUID_583747b0-1655-4761-8050-4036436a1052/";

        Method getSurroundings = agent.getClass().getDeclaredMethod("getSurroundings", String.class, String.class, List.class, List.class);
        assertNotNull(getSurroundings);
        getSurroundings.setAccessible(true);

        String geometry = "555438.08#305587.27999#-0.6#555484.04#305587.27999#-0.6#555484.04#305614.87999#-0.6#555438.08#305614.87999#-0.6#555438.08#305587.27999#-0.6";
        List<String> unique = new ArrayList<>();
        unique.add(uri);

        JSONArray geometryArray = new JSONArray().put(new JSONObject().put("envelope", geometry).put("geometry", geometry).put("datatype", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        JSONArray buildingsArray = new JSONArray().put(new JSONObject().put("cityObject", uri)).put(new JSONObject().put("cityObject", "http://localhost/kings-lynn-open-data/cityobject/UUID_447787a5-1678-4246-8658-4036436c1052/"));
        JSONArray heightArray = new JSONArray().put(new JSONObject().put("HeightMeasuredHeigh", 10.0));

        List testSurroundingCoordinate = new ArrayList<>();


        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(geometryArray).thenReturn(buildingsArray).thenReturn(heightArray).thenReturn(geometryArray);

            ArrayList<CEAInputData> result = (ArrayList<CEAInputData>) getSurroundings.invoke(agent, uri, "testRoute", unique, testSurroundingCoordinate);

            assertFalse(result.isEmpty());
            assertTrue(result.get(0).getHeight().equals("10.0"));
            assertNull(result.get(0).getUsage());
            assertNull(result.get(0).getSurrounding());
            assertFalse(testSurroundingCoordinate.isEmpty());
        }
    }

    @Test
    public void testCheckQuadsEnabled() throws NoSuchMethodException {
        CEAAgent agent = spy(new CEAAgent());

        Method checkQuadsEnabled = agent.getClass().getDeclaredMethod("checkQuadsEnabled", String.class);
        assertNotNull(checkQuadsEnabled);

        doReturn(true).when(agent).checkEndpoint(anyString());

        try {
            checkQuadsEnabled.invoke(agent, "");
        } catch (Exception e) {
            assertTrue(e instanceof InvocationTargetException);
            assertTrue(((InvocationTargetException) e).getTargetException().getMessage().contains("ceaEndpoint does not support graph"));
        }
    }

    @Test
    public void testGetWeather() throws NoSuchMethodException, NoSuchFieldException, IllegalAccessException, InvocationTargetException {
        CEAAgent agent = spy(new CEAAgent());

        Method getWeather = agent.getClass().getDeclaredMethod("getWeather", String.class, String.class, String.class, String.class, List.class);
        assertNotNull(getWeather);
        getWeather.setAccessible(true);

        Field ontoemsUri = agent.getClass().getDeclaredField("ontoemsUri");
        assertNotNull(ontoemsUri);
        ontoemsUri.setAccessible(true);

        Field weatherRoute = agent.getClass().getDeclaredField("weatherRoute");
        assertNotNull(weatherRoute);
        weatherRoute.setAccessible(true);

        weatherRoute.set(agent, "testeRoute");

        doReturn("").when(agent).runOpenMeteoAgent(anyString(), anyString());

        String testCRS = "32633";

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            // test when there are no retrievable weather data
            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "testQueryEndpoint").put(JPSConstants.UPDATE_ENDPOINT, "testUpdateEndpoint"));

            JSONArray envelope = new JSONArray();
            envelope.put(new JSONObject().put("envelope", "555438.08#305587.27999#-0.6#555484.04#305587.27999#-0.6#555484.04#305614.87999#-0.6#555438.08#305614.87999#-0.6#555438.08#305587.27999#-0.6"));

            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(envelope).thenReturn(new JSONArray());

            List<Object> testList = new ArrayList<>();

            assertFalse((Boolean) getWeather.invoke(agent, "", "", "", testCRS, testList));
            assertEquals(testList.size(), 0);

            // test when there are retrievable weather data
            JSONArray station = new JSONArray();
            station.put(new JSONObject().put("station", "testStation"));
            JSONArray weatherIRIs = new JSONArray();
            weatherIRIs.put(new JSONObject().put("weatherParameter", ontoemsUri.get(agent) + "testWeather").put("measure", "testMeasure").put("rdb", "testRDB"));
            JSONArray coordinate = new JSONArray();
            coordinate.put(new JSONObject().put("coordinate", "1.0#1.0"));
            JSONArray elevation = new JSONArray();
            elevation.put(new JSONObject().put("elevation", "1.0"));

            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(envelope).thenReturn(station).thenReturn(weatherIRIs).thenReturn(coordinate).thenReturn(elevation);


            TimeSeries<Instant> mockTS = mock(TimeSeries.class);

            List<Instant> testTimesList = Collections.nCopies(8760, Instant.parse("2023-01-01T00:00:00.00Z"));
            List<Double> testWeatherData = Collections.nCopies(8760, 0.00);

            doReturn(testTimesList).when(mockTS).getTimes();
            doReturn(testWeatherData).when(mockTS).getValuesAsDouble(anyString());

            doReturn(mockTS).when(agent).retrieveData(anyString(), any(), any(), any());
            doReturn(0.00).when(agent).getStationOffset(anyDouble(), anyDouble(), any(), any());

            assertTrue((Boolean) getWeather.invoke(agent, "", "", "", testCRS, testList));

            verify(agent, times(1)).retrieveData(anyString(), any(), any(), any());
            assertEquals(testList.size(), 3);
            assertEquals(((Map<String, List<String>>) testList.get(1)).size(), 1);
            assertTrue(((Map<String, List<String>>) testList.get(1)).containsKey("testWeather"));
            assertEquals(((Map<String, List<String>>) testList.get(1)).get("testWeather").size(), ((List<OffsetDateTime>) testList.get(0)).size());
            assertEquals(((List<OffsetDateTime>) testList.get(0)).size(), 8760);
        }
    }

    @Test
    public void testRunOpenMeteoAgent() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();

        Method runOpenMeteoAgent = agent.getClass().getDeclaredMethod("runOpenMeteoAgent", String.class, String.class);
        assertNotNull(runOpenMeteoAgent);
        runOpenMeteoAgent.setAccessible(true);

        try (MockedStatic<Unirest> unirestMock = mockStatic(Unirest.class, RETURNS_MOCKS)) {

            HttpResponse<String> mockResponse = mock(HttpResponse.class);

            when(mockResponse.getStatus()).thenReturn(HttpURLConnection.HTTP_OK);

            unirestMock.when(() -> Unirest.post(anyString())
                            .header(anyString(), anyString())
                            .body(anyString())
                            .socketTimeout(anyInt())
                            .asString())
                    .thenReturn(mockResponse);

            String result = (String) runOpenMeteoAgent.invoke(agent, "", "");

            assertEquals(result, "");
        }
    }

    @Test
    public void testGetWeatherIRI() throws NoSuchMethodException, NoSuchFieldException, IllegalAccessException, InvocationTargetException {
        CEAAgent agent = new CEAAgent();

        Method getWeatherIRI = agent.getClass().getDeclaredMethod("getWeatherIRI", String.class, String.class);
        assertNotNull(getWeatherIRI);
        getWeatherIRI.setAccessible(true);

        Field ontoemsUri = agent.getClass().getDeclaredField("ontoemsUri");
        assertNotNull(ontoemsUri);
        ontoemsUri.setAccessible(true);

        String testWeatherParamter = ontoemsUri.get(agent) + "TestWeather";
        JSONArray testJSONArray = new JSONArray();
        testJSONArray.put(new JSONObject().put("weatherParameter", testWeatherParamter).put("measure", "testMeasure").put("rdb", "testRDB"));

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(testJSONArray);

            Map<String, List<String>> result = (Map<String, List<String>>) getWeatherIRI.invoke(agent, "", "");

            assertTrue(result.containsKey("TestWeather"));
            assertEquals(result.get("TestWeather").get(0), "testMeasure");
            assertEquals(result.get("TestWeather").get(1), "testRDB");
        }
    }

    @Test
    public void testParseWeather() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        CEAAgent agent = spy(new CEAAgent());

        Method parseWeather = agent.getClass().getDeclaredMethod("parseWeather", Map.class, List.class, Double.class, Double.class);
        assertNotNull(parseWeather);
        parseWeather.setAccessible(true);

        Map<String, List<String>> testMap = new HashMap<>();
        List<Object> testList = new ArrayList<>();

        testMap.put("testWeather", Arrays.asList("testWeatherIRI", "testRDB"));

        // test for when the weather data do not have enough data points (<8760)
        List<Instant> testTimesList = Collections.nCopies(2, Instant.now());
        List<Double> testWeatherData = Collections.nCopies(2, 0.00);

        Field rdb_client = agent.getClass().getDeclaredField("RDB_CLIENT");
        assertNotNull(rdb_client);
        rdb_client.setAccessible(true);

        Field store_client = agent.getClass().getDeclaredField("STORE_CLIENT");
        assertNotNull(store_client);
        store_client.setAccessible(true);

        RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
        RemoteStoreClient mockStoreClient = mock(RemoteStoreClient.class);

        Map<String, Object> mockWeatherClients = new HashMap<>();
        mockWeatherClients.put(rdb_client.get(agent).toString(), mockRDBClient);
        mockWeatherClients.put(store_client.get(agent).toString(), mockStoreClient);

        doReturn(mockWeatherClients).when(agent).getWeatherClients(anyString());

        TimeSeries<Instant> mockTS = mock(TimeSeries.class);
        doReturn(testTimesList).when(mockTS).getTimes();
        doReturn(testWeatherData).when(mockTS).getValuesAsDouble(anyString());

        doReturn(mockTS).when(agent).retrieveData(anyString(), any(), any(), any());

        // check that the data is not parsed to testList if weather data do not meet CEA requirements
        assertFalse((Boolean) parseWeather.invoke(agent, testMap, testList, 0.00, 0.00));
        assertEquals(testList.size(), 0);

        testList = new ArrayList<>();

        testTimesList = Collections.nCopies(8760, Instant.parse("2023-01-01T00:00:00.00Z"));
        testWeatherData = Collections.nCopies(8760, 0.00);

        doReturn(testTimesList).when(mockTS).getTimes();
        doReturn(testWeatherData).when(mockTS).getValuesAsDouble(anyString());

        // test that weather data is correctly parsed, with exactly 8760 data points
        assertTrue((Boolean) parseWeather.invoke(agent, testMap, testList, 0.00, 0.00));

        assertEquals(testList.size(), 2);
        assertEquals(((Map<String, List<String>>) testList.get(1)).size(), 1);
        assertTrue(((Map<String, List<String>>) testList.get(1)).containsKey("testWeather"));
        assertEquals(((Map<String, List<String>>) testList.get(1)).get("testWeather").size(), ((List<OffsetDateTime>) testList.get(0)).size());
        assertEquals(((List<OffsetDateTime>) testList.get(0)).size(), 8760);
    }

    @Test
    public void testValidateWeatherTimes() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();

        Method validateWeatherTimes = agent.getClass().getDeclaredMethod("validateWeatherTimes", List.class, Double.class, Double.class);
        assertNotNull(validateWeatherTimes);
        validateWeatherTimes.setAccessible(true);

        // check validation for requirement of 8760 timestamps
        List<Instant> testTimesList = Collections.nCopies(2, Instant.parse("2023-01-01T00:00:00.00Z"));
        assertFalse((Boolean) validateWeatherTimes.invoke(agent, testTimesList, 0.00, 0.00));

        // check validation for requirement of start date to be first day of the year
        testTimesList = Collections.nCopies(8760, Instant.parse("2023-01-03T00:00:00.00Z"));
        assertFalse((Boolean) validateWeatherTimes.invoke(agent, testTimesList, 0.00, 0.00));

        // check validation for requirements for both start date and number of time stamps
        testTimesList = Collections.nCopies(8760, Instant.parse("2023-01-01T00:00:00.00Z"));
        assertTrue((Boolean) validateWeatherTimes.invoke(agent, testTimesList, 0.00, 0.00));
    }

    @Test
    public void testParseWeatherTimes() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        CEAAgent agent = new CEAAgent();

        Method parseWeatherTimes = agent.getClass().getDeclaredMethod("parseWeatherTimes", List.class, Integer.class);
        assertNotNull(parseWeatherTimes);
        parseWeatherTimes.setAccessible(true);

        List<Instant> testTimesList = Collections.nCopies(3, Instant.parse("2023-01-01T00:00:00.00Z"));

        List<OffsetDateTime> result = (List<OffsetDateTime>) parseWeatherTimes.invoke(agent, testTimesList, 3600);

        assertEquals(3600, result.get(0).getOffset().getTotalSeconds());
        assertEquals(1, result.get(0).getHour());
    }

    @Test
    public void testGetTerrain() throws NoSuchMethodException, SQLException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        CEAAgent agent = spy(new CEAAgent());

        Method getTerrain = agent.getClass().getDeclaredMethod("getTerrain", String.class, String.class, String.class, List.class, RemoteRDBStoreClient.class);
        assertNotNull(getTerrain);
        getTerrain.setAccessible(true);

        byte[] testBytes = new byte[] {1, 2, 3};
        JSONArray sridArray = new JSONArray();

        sridArray.put(new JSONObject().put("srid", 32632));

        RemoteRDBStoreClient mockRDBClient = mock(RemoteRDBStoreClient.class);
        Connection mockConnection = mock(Connection.class);
        Statement  mockStatement = mock(Statement.class);
        ResultSet mockResultSet = mock(ResultSet.class);

        doReturn(sridArray).when(mockRDBClient).executeQuery(anyString());

        doReturn(testBytes).when(mockResultSet).getBytes(anyString());
        when(mockResultSet.next()).thenReturn(true).thenReturn(false);
        doReturn(mockResultSet).when(mockStatement).executeQuery(anyString());
        doReturn(mockStatement).when(mockConnection).createStatement();
        doReturn(mockConnection).when(mockRDBClient).getConnection();

        List<Coordinate> testSurroundingCoordinates = new ArrayList<>();

        testSurroundingCoordinates.add(new Coordinate(-38583.309964376036, 5475530.947358239));
        testSurroundingCoordinates.add(new Coordinate(-38590.053145669284, 5475500.099337375));
        testSurroundingCoordinates.add(new Coordinate(-38570.165186359896, 5475520.409057047));



        byte[] result = (byte[]) getTerrain.invoke(agent, "", "", "32633", testSurroundingCoordinates, mockRDBClient);

        assertEquals(result.length, testBytes.length);
        assertEquals(result[0], testBytes[0]);
        assertEquals(result[1], testBytes[1]);
        assertEquals(result[2], testBytes[2]);
    }

    @AfterEach
    public void cleanup() throws IOException {
        if (postgres.isRunning()) {
            postgres.stop();
        }
    }
}
