package uk.ac.cam.cares.jps.agent.openmeteoagent;

import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

public class OpenMeteoAgentTest {
    @Test
    public void testProcessRequestParameters() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException, SQLException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(new JSONArray());

            // Test processRequestParameters(JSONObject.class, HttpServletRequest.class)
            OpenMeteoAgent agentHTTP = spy(new OpenMeteoAgent());
            Method processRequestParametersHTTP = agentHTTP.getClass().getDeclaredMethod("processRequestParameters", JSONObject.class, HttpServletRequest.class);
            assertNotNull(processRequestParametersHTTP);

            JSONObject mockJSONObject = mock(JSONObject.class);
            HttpServletRequest mockHTTP = mock(HttpServletRequest.class);

            doReturn(new JSONObject()).when(agentHTTP).processRequestParameters(any(JSONObject.class));

            processRequestParametersHTTP.invoke(agentHTTP, mockJSONObject, mockHTTP);

            verify(agentHTTP, times(1)).processRequestParameters(any(JSONObject.class));

            // Test processRequestParameters(JSONObject.class) with URI_RUN endpoint
            OpenMeteoAgent agent = spy(new OpenMeteoAgent());
            Method processRequestParameters = agent.getClass().getDeclaredMethod("processRequestParameters", JSONObject.class);
            assertNotNull(processRequestParameters);

            Field api_hourly = agent.getClass().getDeclaredField("API_HOURLY");
            assertNotNull(api_hourly);
            api_hourly.setAccessible(true);
            Field api_hourly_units = agent.getClass().getDeclaredField("API_HOURLY_UNITS");
            assertNotNull(api_hourly_units);
            api_hourly_units.setAccessible(true);
            Field api_elevation = agent.getClass().getDeclaredField("API_ELEVATION");
            assertNotNull(api_elevation);
            api_elevation.setAccessible(true);
            Field api_offset = agent.getClass().getDeclaredField("API_OFFSET");
            assertNotNull(api_offset);
            api_offset.setAccessible(true);

            Field latitude = agent.getClass().getDeclaredField("latitude");
            latitude.setAccessible(true);
            assertNull(latitude.get(agent));
            Field longitude = agent.getClass().getDeclaredField("longitude");
            longitude.setAccessible(true);
            assertNull(longitude.get(agent));
            Field elevation = agent.getClass().getDeclaredField("elevation");
            elevation.setAccessible(true);
            assertNull(elevation.get(agent));

            Field rdbStoreClient = agent.getClass().getDeclaredField("rdbStoreClient");
            rdbStoreClient.setAccessible(true);

            RemoteRDBStoreClient mockRDBStoreClient = mock(RemoteRDBStoreClient.class);
            rdbStoreClient.set(agent, mockRDBStoreClient);

            Connection mockConn = mock(Connection.class);

            doReturn(mockConn).when(mockRDBStoreClient).getConnection();

            JSONObject testRequestParams = new JSONObject()
                    .put(OpenMeteoAgent.KEY_REQ_URL, OpenMeteoAgent.URI_RUN)
                    .put(OpenMeteoAgent.KEY_REQ_METHOD, HttpMethod.POST)
                    .put(OpenMeteoAgent.KEY_LAT, "1.00")
                    .put(OpenMeteoAgent.KEY_LON, "2.00")
                    .put(OpenMeteoAgent.KEY_START, "2021-01-01")
                    .put(OpenMeteoAgent.KEY_END, "2021-01-02");

            List<Double> testData = new ArrayList<>();
            testData.add(1.0);
            testData.add(2.0);

            Field api_parameters = agent.getClass().getDeclaredField("API_PARAMETERS");
            assertNotNull(api_parameters);
            api_parameters.setAccessible(true);
            Field api_time = agent.getClass().getDeclaredField("API_TIME");
            assertNotNull(api_time);
            api_time.setAccessible(true);

            JSONArray testJSONArray = new JSONArray(testData);

            JSONObject testWeatherData = new JSONObject();
            JSONObject testWeatherUnit = new JSONObject();

            for (String parameter: (List<String>) api_parameters.get(agent)){
                testWeatherData.put(parameter, testJSONArray);
                testWeatherUnit.put(parameter, "");
            }

            List<String> testTimes = new ArrayList<>();
            testTimes.add("2021-01-01T01:00");
            testTimes.add("2021-01-01T02:00");

            String timezone = "Europe/Berlin";

            testWeatherData.put(api_time.get(agent).toString(), new JSONArray(testTimes));

            JSONObject testWeatherResponse = new JSONObject()
                    .put(api_offset.get(agent).toString(), 1)
                    .put(api_hourly.get(agent).toString(), testWeatherData)
                    .put(api_hourly_units.get(agent).toString(), testWeatherUnit)
                    .put(api_elevation.get(agent).toString(), 3.00);

            doReturn(testWeatherResponse).when(agent).getWeatherData(anyDouble(), anyDouble(), anyString(), anyString());

            try (MockedConstruction<TimeSeriesClient> mockTS = mockConstruction(TimeSeriesClient.class)){
                processRequestParameters.invoke(agent, testRequestParams);
                verify(mockTS.constructed().get(0), times(1)).bulkInitTimeSeries(anyList(), anyList(), anyList(), any(Connection.class), anyList(), anyList(), anyList());
                verify(mockTS.constructed().get(0), times(1)).bulkaddTimeSeriesData(anyList(), any(Connection.class));

            }
            assertEquals((Double) latitude.get(agent), 1.00);
            assertEquals((Double) longitude.get(agent), 2.00);
            assertEquals((Double) elevation.get(agent), 3.00);
            verify(agent, times(1)).getWeatherData(anyDouble(), anyDouble(), anyString(), anyString());

            // Test processRequestParameters(JSONObject.class) with URI_DELETE endpoint

            testRequestParams.put(OpenMeteoAgent.KEY_REQ_URL, OpenMeteoAgent.URI_DELETE);

            JSONArray testQueryResults = new JSONArray()
                    .put(new JSONObject().put("timeseries", "ts").put("measure", "ms").put("quantity", "qt"))
                    .put(new JSONObject().put("timeseries", "ts1").put("measure", "ms1").put("quantity", "qt1"));

            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(new JSONArray().put(new JSONObject().put("station", "testStation"))).thenReturn(testQueryResults);

            try (MockedConstruction<TimeSeriesClient> mockTS = mockConstruction(TimeSeriesClient.class)) {
                processRequestParameters.invoke(agent, testRequestParams);
                verify(mockTS.constructed().get(0), times(testQueryResults.length())).deleteTimeSeries(anyString(), any(Connection.class));
            }
        }
    }

    @Test
    public void testValidateInput() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));

            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method validateInput = agent.getClass().getMethod("validateInput", JSONObject.class);
            assertNotNull(validateInput);

            JSONObject testJSON = new JSONObject();

            try {
                validateInput.invoke(agent, testJSON);
            } catch (Exception e) {
                assert e instanceof InvocationTargetException;
                assertEquals(((InvocationTargetException) e).getTargetException().getClass(), BadRequestException.class);
            }

            testJSON.put(OpenMeteoAgent.KEY_REQ_URL, OpenMeteoAgent.URI_RUN);
            testJSON.put(OpenMeteoAgent.KEY_LAT, "1.0");
            testJSON.put(OpenMeteoAgent.KEY_LON, "1.0");
            testJSON.put(OpenMeteoAgent.KEY_START, "2021-01-02");
            testJSON.put(OpenMeteoAgent.KEY_END, "2021-01-01");

            try {
                validateInput.invoke(agent, testJSON);
            } catch (Exception e) {
                assert e instanceof InvocationTargetException;
                assertEquals(((InvocationTargetException) e).getTargetException().getClass(), BadRequestException.class);
            }

            testJSON.put(OpenMeteoAgent.KEY_END, "2021-01-03");
            testJSON.put(OpenMeteoAgent.KEY_REQ_METHOD, HttpMethod.POST);

            assertTrue((Boolean) validateInput.invoke(agent, testJSON));
        }
    }

    @Test
    public void testValidateDate() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));

            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method validateDate = agent.getClass().getDeclaredMethod("validateDate", String.class);
            assertNotNull(validateDate);

            String testString = "2021-13-02";

            assertFalse((Boolean) validateDate.invoke(agent, testString));

            testString = "2021-01-01";

            assertTrue((Boolean) validateDate.invoke(agent, testString));
        }
    }

    @Test
    public void testParseWeatherData() throws NoSuchMethodException, NoSuchFieldException, IllegalAccessException, InvocationTargetException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));

            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method parseWeatherData = agent.getClass().getDeclaredMethod("parseWeatherData", JSONObject.class, JSONObject.class);
            assertNotNull(parseWeatherData);
            parseWeatherData.setAccessible(true);

            Field api_parameters = agent.getClass().getDeclaredField("API_PARAMETERS");
            assertNotNull(api_parameters);
            api_parameters.setAccessible(true);

            List<Double> testData = new ArrayList<>();
            testData.add(1.0);
            testData.add(2.0);

            JSONArray testJSONArray = new JSONArray(testData);

            JSONObject testWeatherData = new JSONObject();
            JSONObject testWeatherUnit = new JSONObject();

            for (String parameter: (List<String>) api_parameters.get(agent)){
                testWeatherData.put(parameter, testJSONArray);
                testWeatherUnit.put(parameter, "");
            }

            Map<String, List<Object>> result = (Map<String, List<Object>>) parseWeatherData.invoke(agent, testWeatherData, testWeatherUnit);

            for (String parameter: (List<String>) api_parameters.get(agent)){
                assertTrue(result.containsKey(parameter));
                assertEquals(result.get(parameter).get(1), testData);
            }
        }
    }

    @Test
    public void testCreateStation() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));

            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method createStation = agent.getClass().getDeclaredMethod("createStation", Double.class, Double.class, Double.class);
            assertNotNull(createStation);
            createStation.setAccessible(true);


            Field ontoemsURI = agent.getClass().getDeclaredField("ontoemsURI");
            ontoemsURI.setAccessible(true);
            Field station = agent.getClass().getDeclaredField("STATION");
            station.setAccessible(true);

            String result = (String) createStation.invoke(agent, 1.0, 1.0, 1.0);

            assertTrue(result.contains(ontoemsURI.get(agent).toString()));
            assertTrue(result.contains(station.get(agent).toString()));

            accessAgentCallerMock.verify(() -> AccessAgentCaller.updateStore(anyString(), anyString()), times(1));
        }
    }

    @Test
    public void testCreateUpdate() throws NoSuchMethodException, NoSuchFieldException, InvocationTargetException, IllegalAccessException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));

            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method createUpdate = agent.getClass().getDeclaredMethod("createUpdate", WhereBuilder.class, String.class, String.class, String.class, String.class, String.class);
            assertNotNull(createUpdate);
            createUpdate.setAccessible(true);

            Field ontoemsURI = agent.getClass().getDeclaredField("ontoemsURI");
            ontoemsURI.setAccessible(true);
            Field omURI = agent.getClass().getDeclaredField("omURI");
            omURI.setAccessible(true);
            Field rdfURI = agent.getClass().getDeclaredField("rdfURI");
            rdfURI.setAccessible(true);

            WhereBuilder test = new WhereBuilder()
                    .addPrefix("ontoems", ontoemsURI.get(agent).toString())
                    .addPrefix("om", omURI.get(agent).toString())
                    .addPrefix("rdf", rdfURI.get(agent).toString());

            createUpdate.invoke(agent, test, "testStation", "testQuantity", "testType", "testMeasure", "testUnit");

            String result = test.build().toString();

            assertTrue(result.contains("testStation"));
            assertTrue(result.contains("testQuantity"));
            assertTrue(result.contains("testType"));
            assertTrue(result.contains("testMeasure"));
            assertTrue(result.contains("testUnit"));
            assertTrue(result.contains("reports"));
        }
    }

    @Test
    public void testAddTimeSeriesWhere() throws NoSuchMethodException, NoSuchFieldException, IllegalAccessException, InvocationTargetException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));


            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method addTimeSeriesWhere = agent.getClass().getDeclaredMethod("addWeatherWhere", WhereBuilder.class, String.class);
            assertNotNull(addTimeSeriesWhere);
            addTimeSeriesWhere.setAccessible(true);

            Field ontoemsURI = agent.getClass().getDeclaredField("ontoemsURI");
            ontoemsURI.setAccessible(true);
            Field omURI = agent.getClass().getDeclaredField("omURI");
            omURI.setAccessible(true);
            Field ontotimeseriesURI = agent.getClass().getDeclaredField("ontotimeseriesURI");
            ontotimeseriesURI.setAccessible(true);
            Field rdfURI = agent.getClass().getDeclaredField("rdfURI");
            rdfURI.setAccessible(true);

            WhereBuilder test = new WhereBuilder()
                    .addPrefix("ontoems", ontoemsURI.get(agent).toString())
                    .addPrefix("om", omURI.get(agent).toString())
                    .addPrefix("ontotimeseries", ontotimeseriesURI.get(agent).toString())
                    .addPrefix("rdf", rdfURI.get(agent).toString());

            addTimeSeriesWhere.invoke(agent, test, "test");

            String result = test.build().toString();

            assertTrue(result.contains("test"));
            assertTrue(result.contains("reports"));
            assertTrue(result.contains("hasValue"));
            assertTrue(result.contains("hasTimeSeries"));
        }
    }

    @Test
    public void testGetStation() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));

            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(new JSONArray()).thenReturn(new JSONArray().put(new JSONObject().put("station", "testStation")));

            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method getStation = agent.getClass().getDeclaredMethod("getStation", Double.class, Double.class);
            assertNotNull(getStation);
            getStation.setAccessible(true);

            try {
                getStation.invoke(agent, 1.0, 1.0);
            } catch (Exception e) {
                assert e instanceof InvocationTargetException;
                assertTrue(((InvocationTargetException) e).getTargetException().getMessage().contains("No reporting station found at the given coordinate."));
            }

            String result = (String) getStation.invoke(agent, 1.0, 1.0);

            assertTrue(result.equals("testStation"));

            accessAgentCallerMock.verify(() -> AccessAgentCaller.queryStore(anyString(), anyString()), times(2));
        }
    }

    @Test
    public void testDeleteIRI() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));

            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method deleteIRI = agent.getClass().getDeclaredMethod("deleteIRI", String.class);
            assertNotNull(deleteIRI);
            deleteIRI.setAccessible(true);

            deleteIRI.invoke(agent, "test");

            accessAgentCallerMock.verify(() -> AccessAgentCaller.updateStore(anyString(), anyString()), times(2));
        }
    }

    @Test
    public void testSetTimeSeriesTypes() throws NoSuchMethodException, NoSuchFieldException, InvocationTargetException, IllegalAccessException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));

            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method setTimeSeriesTypes = agent.getClass().getDeclaredMethod("setTimeSeriesTypes");
            assertNotNull(setTimeSeriesTypes);
            setTimeSeriesTypes.setAccessible(true);

            Field api_timeseries = agent.getClass().getDeclaredField("api_timeseries");
            assertNotNull(api_timeseries);
            api_timeseries.setAccessible(true);

            Field api_parameters = agent.getClass().getDeclaredField("API_PARAMETERS");
            assertNotNull(api_parameters);
            api_parameters.setAccessible(true);

            setTimeSeriesTypes.invoke(agent);

            assertFalse(((Map) api_timeseries.get(agent)).isEmpty());

            for (String parameter: (List<String>) api_parameters.get(agent)){
                assertTrue(((Map) api_timeseries.get(agent)).containsKey(parameter));
                assertTrue(((Map) api_timeseries.get(agent)).get(parameter) instanceof TimeSeriesClient.Type);
            }

        }
    }

    @Test
    public void testGetTimesList() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {

            accessAgentCallerMock.when(() -> AccessAgentCaller.getEndpoints(anyString()))
                    .thenReturn(new JSONObject().put(JPSConstants.QUERY_ENDPOINT, "").put(JPSConstants.UPDATE_ENDPOINT, ""));

            OpenMeteoAgent agent = new OpenMeteoAgent();
            Method getTimesList = agent.getClass().getDeclaredMethod("getTimesList", JSONObject.class, String.class, Integer.class);
            assertNotNull(getTimesList);
            getTimesList.setAccessible(true);

            JSONObject testJSONObject = new JSONObject();
            List<String> test = new ArrayList<>();
            String key = "time";
            Integer offset = 3600;
            ZoneOffset zoneOffset = ZoneOffset.ofTotalSeconds(offset);

            test.add("2021-01-01T01:00");
            test.add("2021-01-01T02:00");

            testJSONObject.put(key, new JSONArray(test));

            List<OffsetDateTime> result = (List<OffsetDateTime>) getTimesList.invoke(agent, testJSONObject, key, offset);

            assertEquals(test.size(), result.size());

            for (int i = 0; i < test.size(); i++) {
                assertEquals(LocalDateTime.parse(test.get(i)).atOffset(zoneOffset), result.get(i));
            }
        }
    }
}