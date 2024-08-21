package uk.ac.cam.cares.jps.sensor;

import android.content.Context;
import android.util.Log;

import com.android.volley.AuthFailureError;
import com.android.volley.RequestQueue;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.StringRequest;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.io.UnsupportedEncodingException;
import java.util.Map;

import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class SensorNetworkSourceTest {

    @Mock
    private Context mockContext;

    @Mock
    private RequestQueue mockRequestQueue;

    @Mock
    private SensorLocalSource mockSensorLocalSource;

    private SensorNetworkSource sensorNetworkSource;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        sensorNetworkSource = new SensorNetworkSource(mockContext, mockRequestQueue);
        mockSensorLocalSource = new SensorLocalSource(mockContext);
    }

    @Test
    public void testSendPostRequest_SuccessfulRequest() throws JSONException, UnsupportedEncodingException, AuthFailureError {

        String deviceId = "device123";
        JSONArray sensorData = new JSONArray();
        JSONObject sensorObject = new JSONObject();
        sensorObject.put("type", "location");
        sensorObject.put("value", 42);
        sensorData.put(sensorObject);

        sensorNetworkSource.sendPostRequest(deviceId, sensorData);

        ArgumentCaptor<StringRequest> requestCaptor = ArgumentCaptor.forClass(StringRequest.class);
        verify(mockRequestQueue).add(requestCaptor.capture());


        StringRequest capturedRequest = requestCaptor.getValue();
        assertNotNull(capturedRequest);
        assertEquals("application/json; charset=utf-8", capturedRequest.getBodyContentType());

        String requestBody = new String(capturedRequest.getBody(), "UTF-8");
        JSONObject requestBodyJson = new JSONObject(requestBody);

        assertEquals(deviceId, requestBodyJson.getString("deviceId"));
        assertEquals("location", requestBodyJson.getJSONArray("payload").getJSONObject(0).getString("type"));
    }

    @Test
    public void testSendPostRequest_FailedRequest() throws JSONException {
       // shld prob delcare this outside so that it's less repetition
        String deviceId = "device123";
        JSONArray sensorData = new JSONArray();
        JSONObject sensorObject = new JSONObject();
        sensorObject.put("type", "location");
        sensorObject.put("value", 42);
        sensorData.put(sensorObject);

        doAnswer(invocation -> {
            StringRequest request = invocation.getArgument(0);
            request.getErrorListener().onErrorResponse((VolleyError) new Exception("Network error"));
            return null;
        }).when(mockRequestQueue).add(any(StringRequest.class));


        sensorNetworkSource.sendPostRequest(deviceId, sensorData);

        verify(mockSensorLocalSource, times(1)).writeToDatabase(any(Map.class));
    }

    @Test
    public void testConvertSensorDataToMap() throws JSONException {
        JSONArray sensorData = new JSONArray();
        JSONObject sensorObject1 = new JSONObject();
        sensorObject1.put("type", "location");
        sensorObject1.put("value", 42);
        sensorData.put(sensorObject1);

        JSONObject sensorObject2 = new JSONObject();
        sensorObject2.put("type", "location");
        sensorObject2.put("value", 43);
        sensorData.put(sensorObject2);

        Map<String, JSONArray> resultMap = sensorNetworkSource.convertSensorDataToMap(sensorData);

        assertEquals(1, resultMap.size());
        assertTrue(resultMap.containsKey("location"));
        assertEquals(2, resultMap.get("location").length());
    }
}
