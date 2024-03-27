package com.example.notsensorlogger2;

import android.app.Activity;
import android.os.Bundle;
import android.hardware.SensorManager;
import android.util.Log;
import android.view.View;
import android.widget.Button;

import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.StringRequest;
import com.android.volley.toolbox.Volley;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.UUID;

public class MainActivity extends Activity {
    private SensorManager sensorManager;
    private SensorViewModel sensorViewModel;
    private SensorHandler accelerometerHandler;
    private SensorHandler gyroscopeHandler;
    private SensorHandler magnetometerHandler;
    private SensorHandler lightSensorHandler;
    private SensorHandler humiditySensorHandler;
    private SensorHandler pressureSensorHandler;
    private SensorHandler gravitySensorHandler;
    private LocationHandler locationTracker; // Make sure this is the updated LocationHandler
    private String deviceId;
    private RecyclerView sensorsRecyclerView;
    private SensorAdapter sensorAdapter;
    private List<SensorItem> sensorItems;
    private boolean isRecording = false;
    private Button startRecordingButton; // For the recording button


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

        // Set up the RecyclerView with the SensorAdapter
        sensorsRecyclerView = findViewById(R.id.sensors_recycler_view);
        sensorsRecyclerView.setLayoutManager(new LinearLayoutManager(this));

        // Create a list of SensorItems (you'll need to define appropriate drawable resources for your icons)
        List<SensorItem> sensorItems = new ArrayList<>();
        sensorItems.add(new SensorItem("Accelerometer", R.drawable.ic_accelerometer, false, (AbstractSensorHandler) accelerometerHandler));
        sensorItems.add(new SensorItem("Gravity", R.drawable.ic_gravity, false, (AbstractSensorHandler) gravitySensorHandler));
        sensorItems.add(new SensorItem("Gyroscope", R.drawable.ic_gyroscope, false, (AbstractSensorHandler) gyroscopeHandler));
        sensorItems.add(new SensorItem("light", R.drawable.ic_light, false, (AbstractSensorHandler) lightSensorHandler));
//        sensorItems.add(new SensorItem("Location", R.drawable.ic_location, false, locationTracker));
        sensorItems.add(new SensorItem("Magnetometer", R.drawable.ic_magnetometer, false, (AbstractSensorHandler) magnetometerHandler));
        sensorItems.add(new SensorItem("Barometer", R.drawable.ic_barometer, false, (AbstractSensorHandler) pressureSensorHandler));
        sensorItems.add(new SensorItem("Relative Humidity", R.drawable.ic_relative_humidity, false, (AbstractSensorHandler) humiditySensorHandler));
        //  sensorItems.add(new SensorItem("Sound Pressure", R.drawable.ic_sound_pressure, false));

        // Initialize the adapter and attach it to the RecyclerView
        this.sensorItems = sensorItems; // Assign to the member variable for later access
        sensorAdapter = new SensorAdapter(sensorItems, new SensorAdapter.OnSensorToggleListener() {
            @Override
            public void onSensorToggled(SensorItem sensor, boolean isChecked) {
                // Handle sensor toggle here if needed, such as saving the state
                sensor.setToggled(isChecked);
            }
        }, new SensorAdapter.OnSensorDetailsListener() {
            @Override
            public void onSensorDetailsRequested(SensorItem sensor) {
                // Handle sensor detail request here
            }
        });

        sensorsRecyclerView.setAdapter(sensorAdapter);

        // Initialize the start/stop recording button
        startRecordingButton = findViewById(R.id.start_recording_button);
        startRecordingButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                handleRecordingToggle();
            }
        });
    }

//        ImageView imageView = (ImageView) findViewById(R.id.locationButton); // Replace with your ImageView ID
//        imageView.setOnClickListener(new View.OnClickListener() {
//            @Override
//            public void onClick(View v) {
//                Intent intent = new Intent(MainActivity.this, MainActivity2.class);
//                startActivity(intent);
//            }
//        });

    private void handleRecordingToggle() {
        if (!isRecording) {
            // Start recording from sensors that are toggled on
            for (SensorItem sensorItem : sensorItems) {
                if (sensorItem.isToggled()) {
                    sensorItem.getHandler().start();
                }
            }
            isRecording = true;
            startRecordingButton.setText("Stop Recording");
        } else {
            // Stop all sensors and collect their data
            JSONArray allSensorData = new JSONArray();
            for (SensorItem sensorItem : sensorItems) {
                if (sensorItem.isToggled()) {
                    sensorItem.getHandler().stop();
                    allSensorData.put(sensorItem.getHandler().getSensorData());
                }
            }
            sendPostRequest(allSensorData);
            isRecording = false;
            startRecordingButton.setText("Start Recording");
        }
    }


    @Override
    protected void onResume() {
        super.onResume();
    }

    @Override
    protected void onPause() {
        super.onPause();

        // Collect sensor data into a JSONArray
        JSONArray allSensorData = new JSONArray();
        allSensorData.put(accelerometerHandler.getSensorData());
        allSensorData.put(gyroscopeHandler.getSensorData());
        allSensorData.put(magnetometerHandler.getSensorData());
        allSensorData.put(lightSensorHandler.getSensorData());
        allSensorData.put(humiditySensorHandler.getSensorData());
        allSensorData.put(pressureSensorHandler.getSensorData());
        allSensorData.put(gravitySensorHandler.getSensorData());
        allSensorData.put(locationTracker.getSensorData()); // Assuming this also follows the new pattern

        // Send the collected sensor data
        sendPostRequest(allSensorData);
    }


    private void sendPostRequest(JSONArray sensorData) {
        String url = "abc"; // http://192.168.1.60:3838/sensorloggermobileappagent/update          //  https://eou1bwdjb3p7r6h.m.pipedream.net
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
            payloadWrapper.put("sessionId", sessionId);

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
                            Log.d("Error.Response", error.toString());
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

//    private void sendPostRequest(JSONArray sensorData) {
//        String url = "http://192.168.1.60:3838/sensorloggermobileappagent/update"; // Replace with your RequestBin URL
//        RequestQueue queue = Volley.newRequestQueue(this);
//
//        StringRequest postRequest = new StringRequest(Request.Method.POST, url,
//                new Response.Listener<String>() {
//                    @Override
//                    public void onResponse(String response) {
//                        Log.d("Response", response);
//                    }
//                },
//                new Response.ErrorListener() {
//                    @Override
//                    public void onErrorResponse(VolleyError error) {
//                        Log.d("Error.Response", error.toString());
//                    }
//                }
//        ) {
//            @Override
//            public byte[] getBody() {
//                return sensorData.toString().getBytes(StandardCharsets.UTF_8);
//            }
//
//            @Override
//            public String getBodyContentType() {
//                return "application/json; charset=utf-8";
//            }
//        };
//
//        queue.add(postRequest);
//    }

//}