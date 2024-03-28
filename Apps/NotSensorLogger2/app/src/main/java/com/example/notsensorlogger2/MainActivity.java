package com.example.notsensorlogger2;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.hardware.SensorManager;
import android.util.Log;
import android.view.View;
import android.widget.ImageView;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.StringRequest;
import com.android.volley.toolbox.Volley;
import com.example.notsensorlogger2.gpsfunctionality.MainActivity2;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.nio.charset.StandardCharsets;
import java.util.Random;
import java.util.UUID;

public class MainActivity extends Activity {
    private SensorManager sensorManager;
    private SensorHandler accelerometerHandler;
    private SensorHandler gyroscopeHandler;
    private SensorHandler magnetometerHandler;
    private SensorHandler lightSensorHandler;
    private SensorHandler humiditySensorHandler;
    private SensorHandler pressureSensorHandler;
    private SensorHandler gravitySensorHandler;
    private LocationHandler locationTracker; // Make sure this is the updated LocationHandler
    private String deviceId;

    @Override
    public final void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        deviceId = DeviceIdManager.getDeviceId(this);
        Log.d("DeviceId", "Device ID: " + deviceId);

        sensorManager = (SensorManager) getSystemService(SENSOR_SERVICE);
        accelerometerHandler = new AccelerometerHandler(sensorManager);
        gyroscopeHandler = new GyroscopeHandler(sensorManager);
        magnetometerHandler = new MagnetometerHandler(sensorManager);
        lightSensorHandler = new LightSensorHandler(sensorManager);
        humiditySensorHandler = new RelativeHumiditySensorHandler(sensorManager);
        pressureSensorHandler = new PressureSensorHandler(sensorManager);
        gravitySensorHandler = new GravitySensorHandler(sensorManager);
        locationTracker = new LocationHandler(this); // Initialize the LocationHandler

        ImageView imageView = (ImageView) findViewById(R.id.locationButton); // Replace with your ImageView ID
        imageView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent intent = new Intent(MainActivity.this, MainActivity2.class);
                startActivity(intent);
            }
        });
    }

    @Override
    protected void onResume() {
        super.onResume();
        accelerometerHandler.start();
        gyroscopeHandler.start();
        magnetometerHandler.start();
        lightSensorHandler.start();
        humiditySensorHandler.start();
        pressureSensorHandler.start();
        gravitySensorHandler.start();
        locationTracker.start(); // Start the LocationHandler
    }

    @Override
    protected void onPause() {
        super.onPause();
        accelerometerHandler.stop();
        gyroscopeHandler.stop();
        magnetometerHandler.stop();
        lightSensorHandler.stop();
        humiditySensorHandler.stop();
        pressureSensorHandler.stop();
        gravitySensorHandler.stop();
        locationTracker.stop(); // Stop the LocationHandler

        // Collect sensor data
        JSONArray allSensorData = new JSONArray();
        allSensorData.put(((AbstractSensorHandler) accelerometerHandler).getSensorData());
        allSensorData.put(((AbstractSensorHandler) gyroscopeHandler).getSensorData());
        allSensorData.put(((AbstractSensorHandler) magnetometerHandler).getSensorData());
        allSensorData.put(((AbstractSensorHandler) lightSensorHandler).getSensorData());
        allSensorData.put(((AbstractSensorHandler) humiditySensorHandler).getSensorData());
        allSensorData.put(((AbstractSensorHandler) pressureSensorHandler).getSensorData());
        allSensorData.put(((AbstractSensorHandler) gravitySensorHandler).getSensorData());
        allSensorData.put(locationTracker.getLocationData());

        // Send the sensor data
        sendPostRequest(allSensorData);
    }

    private void sendPostRequest(JSONArray sensorData) {
        String url = "http://192.168.1.60:3838/sensorloggermobileappagent/update"; // http://192.168.1.60:3838/sensorloggermobileappagent/update          //  https://eou1bwdjb3p7r6h.m.pipedream.net  //https://eo1kqlcacw69s9d.m.pipedream.net/
        RequestQueue queue = Volley.newRequestQueue(this);

        // Generate a session ID and message ID as needed
        String sessionId = UUID.randomUUID().toString(); // Example, use an actual session management
        int messageId = generateMessageId(); // Implement this method to generate a message ID

        try {
            // Wrap your sensor data inside a JSONObject with the expected structure
            JSONObject payloadWrapper = new JSONObject();
            payloadWrapper.put("deviceId", deviceId);
            payloadWrapper.put("messageId", messageId);
            payloadWrapper.put("payload", sensorData); // Your existing sensor data JSONArray
            payloadWrapper.put("sessionI", sessionId);

            // Convert the whole payload to a JSON string
            final String requestBody = payloadWrapper.toString();

            // Create the request with the proper body and headers
            StringRequest postRequest = new StringRequest(Request.Method.POST, url,
                    new Response.Listener<String>() {
                        @Override
                        public void onResponse(String response) {
                            Log.d("Response", response);
                        }
                    },
                    new Response.ErrorListener() {
                        @Override
                        public void onErrorResponse(VolleyError error) {
                            Log.d("Error.Response", error.getMessage() == null? "null error mesage" : error.getMessage());
                        }
                    }
            ) {
                @Override
                public byte[] getBody() {
                    return requestBody.getBytes(StandardCharsets.UTF_8);
                }

                @Override
                public String getBodyContentType() {
                    return "application/json; charset=utf-8";
                }
            };

            queue.add(postRequest);
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }


    private int generateMessageId() {
        // Replace this with your own logic to generate a message ID
        return new Random().nextInt(1000);
    }

}
