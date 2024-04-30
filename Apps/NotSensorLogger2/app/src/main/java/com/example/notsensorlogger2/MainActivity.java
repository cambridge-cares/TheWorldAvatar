package com.example.notsensorlogger2;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.hardware.SensorManager;
import android.os.Handler;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

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
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import android.Manifest;


public class MainActivity extends Activity {
    private SensorManager sensorManager;
    private SensorHandler accelerometerHandler;
    private SensorHandler gyroscopeHandler;
    private SensorHandler magnetometerHandler;
    private SensorHandler lightSensorHandler;
    private SensorHandler humiditySensorHandler;
    private SensorHandler pressureSensorHandler;
    private SensorHandler gravitySensorHandler;
    private LocationHandler locationTracker;
    private SoundLevelHandler soundLevelHandler;
    private String deviceId;
    private Handler handler = new Handler();
    private Runnable sendDataRunnable;
    private RecyclerView sensorsRecyclerView;
    private SensorAdapter sensorAdapter;
    private List<SensorItem> sensorItems;
    private static final long SEND_INTERVAL = 2000; // Interval in milliseconds
    private boolean isDataCollectionActive = true;
    private static final int PERMISSION_REQUEST_RECORD_AUDIO = 101;
    private int messageId = 1; // Start messageId from 1


    // This method will initialize your sensor list and RecyclerView
    private void setupSensorList() {
        // Initialize your sensor handler list here
        sensorItems = new ArrayList<>();
        sensorItems.add(new SensorItem("Accelerometer", R.drawable.ic_accelerometer, false, accelerometerHandler));
        sensorItems.add(new SensorItem("Gravity", R.drawable.ic_gravity, false, gravitySensorHandler));
        sensorItems.add(new SensorItem("Gyroscope", R.drawable.ic_gyroscope, false, gyroscopeHandler));
        sensorItems.add(new SensorItem("Light", R.drawable.ic_light, false, lightSensorHandler));
        sensorItems.add(new SensorItem("Location", R.drawable.ic_location, false, locationTracker));  // Now directly using LocationHandler
        sensorItems.add(new SensorItem("Magnetometer", R.drawable.ic_magnetometer, false, magnetometerHandler));
        sensorItems.add(new SensorItem("Barometer", R.drawable.ic_barometer, false, pressureSensorHandler));
        sensorItems.add(new SensorItem("Relative Humidity", R.drawable.ic_relative_humidity, false, humiditySensorHandler));
        sensorItems.add(new SensorItem("Sound Pressure", R.drawable.ic_sound_level, false, soundLevelHandler));

        // Initialize RecyclerView
        sensorsRecyclerView = findViewById(R.id.sensors_recycler_view);
        sensorsRecyclerView.setLayoutManager(new LinearLayoutManager(this));

        // Initialize the adapter
        sensorAdapter = new SensorAdapter(sensorItems, new SensorAdapter.OnSensorToggleListener() {
            @Override
            public void onSensorToggled(SensorItem sensor, boolean isChecked) {
                if (isChecked) {
                    sensor.getHandler().start(); // Start recording for this sensor
                } else {
                    sensor.getHandler().stop();  // Stop recording for this sensor
                }
            }
        }, new SensorAdapter.OnSensorDetailsListener() {
            @Override
            public void onSensorDetailsRequested(SensorItem sensor) {
                // Your logic to show sensor details
            }
        });

        // Set the adapter to the RecyclerView
        sensorsRecyclerView.setAdapter(sensorAdapter);
    }


    private void checkAndRequestAudioPermission() {
        if (ActivityCompat.checkSelfPermission(this, Manifest.permission.RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.RECORD_AUDIO}, PERMISSION_REQUEST_RECORD_AUDIO);
        }
        // The initialization of SoundLevelHandler has been moved to onCreate, so we don't initialize it here anymore.
    }


//    @Override
//    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
//        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
//        if (requestCode == PERMISSION_REQUEST_RECORD_AUDIO) {
//            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
//                // Permission was granted, start the audio recording if the handler has been initialized
//                if (soundLevelHandler != null) {
//                    soundLevelHandler.start();
//                }
//            } else {
//                Log.e("MainActivity", "Permission Denied to record audio.");
//                // Handle the case where permission is not granted.
//                // You could disable the SoundLevelHandler or inform the user that they cannot record audio without permission.
//            }
//        }
//    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == PERMISSION_REQUEST_RECORD_AUDIO) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                // Permission was granted
                if (soundLevelHandler != null) {
                    soundLevelHandler.initAudioRecord(); // Make sure this method is public in SoundLevelHandler
                    //  soundLevelHandler.start();
                }
            } else {
                Log.e("MainActivity", "Permission Denied to record audio.");
                // Optionally update the UI or disable related functionality
            }
        }
    }


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
        locationTracker = new LocationHandler((Context) this, (PressureSensorHandler) pressureSensorHandler);
        soundLevelHandler = new SoundLevelHandler(this, sensorManager);

        // After initializing all sensors and handlers, call the setup method for RecyclerView
        setupSensorList();
//        ImageView imageView = (ImageView) findViewById(R.id.locationButton);
//        imageView.setOnClickListener(v -> {
//            Intent intent = new Intent(MainActivity.this, MainActivity2.class);
//            startActivity(intent);
//        });


        Button stopDataButton = findViewById(R.id.start_recording_button);

        Button btnOpenActivity2 = findViewById(R.id.open_activity2_button);

        stopDataButton.setText(R.string.start_button_text); // Initialize with "Start" text
        isDataCollectionActive = false; // Ensure collection is inactive at start

        stopDataButton.setOnClickListener(v -> {
            if (!isDataCollectionActive) {
                startDataCollection();
                stopDataButton.setText(R.string.stop_button_text);
                isDataCollectionActive = true;
            } else {
                stopDataCollection();
                stopDataButton.setText(R.string.start_button_text);
                isDataCollectionActive = false;
            }
        });

        btnOpenActivity2.setOnClickListener(v -> {
            Intent intent = new Intent(this, MainActivity2.class);
            startActivity(intent);
        });


//        Button stopDataButton = findViewById(R.id.start_recording_button);
//        stopDataButton.setOnClickListener(v -> {
//            if (!isDataCollectionActive) {
//                startDataCollection();
//                stopDataButton.setText(R.string.stop_button_text);
//                isDataCollectionActive = true;
//            } else {
//                stopDataCollection();
//                stopDataButton.setText(R.string.start_button_text);
//                isDataCollectionActive = false;
//            }
//        });


//        Button stopDataButton = findViewById(R.id.start_recording_button);
//        stopDataButton.setOnClickListener(v -> {
//            if (isDataCollectionActive) {
//                stopDataCollection();
//                stopDataButton.setText(R.string.start_button_text);
//            } else {
//                startDataCollection();
//                stopDataButton.setText(R.string.stop_button_text);
//            }
//            isDataCollectionActive = !isDataCollectionActive;
//        });

        sendDataRunnable = new Runnable() {
            @Override
            public void run() {
                sendSensorData();
                handler.postDelayed(this, SEND_INTERVAL);
            }
        };
    }


    @Override
    protected void onResume() {
        super.onResume();
        // No auto-start of data collection
        checkAndRequestAudioPermission();
        if (isDataCollectionActive) {
            startDataCollection();
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        if (isDataCollectionActive) {
            stopDataCollection();
        }
    }


//    @Override
//    protected void onResume() {
//        super.onResume();
//        startSensors();
//        handler.post(sendDataRunnable);              WORKING ONES. UNCOMMENT LATER
//    }
//
//    @Override
//    protected void onPause() {
//        super.onPause();
//        stopSensors();
//        handler.removeCallbacks(sendDataRunnable);
//    }


    private void startDataCollection() {
        messageId = 1; // Reset messageId to 1 each time data collection starts
        for (SensorItem item : sensorItems) {
            if (item.isToggled() && item.getHandler() != null) {
                item.getHandler().start();
            }
        }
        // Start sending data only if it's not already being sent
        if (!isDataCollectionActive) {
            handler.postDelayed(sendDataRunnable, SEND_INTERVAL);
            isDataCollectionActive = true;
        }
    }

    private void stopDataCollection() {
        for (SensorItem item : sensorItems) {
            if (item.isToggled() && item.getHandler() != null) {
                item.getHandler().stop();
            }
        }
        // Stop sending data
        if (isDataCollectionActive) {
            handler.removeCallbacks(sendDataRunnable);
            isDataCollectionActive = false;
        }
    }

//    private void startDataCollection() {
//        startSensors();
//        handler.post(sendDataRunnable);
//    }                                                     //WORKING ONES
//
//    private void stopDataCollection() {
//        stopSensors();
//        handler.removeCallbacks(sendDataRunnable);
//    }


//    private void sendSensorData() {
//        JSONArray allSensorData = collectSensorData();
//        sendPostRequest(allSensorData);
//
//        // Clear sensor data after sending
//        ((AbstractSensorHandler) accelerometerHandler).clearSensorData();
//        ((AbstractSensorHandler) gyroscopeHandler).clearSensorData();
//        ((AbstractSensorHandler) magnetometerHandler).clearSensorData();
//        ((AbstractSensorHandler) lightSensorHandler).clearSensorData();
//        ((AbstractSensorHandler) humiditySensorHandler).clearSensorData();
//        ((AbstractSensorHandler) pressureSensorHandler).clearSensorData();
//        ((AbstractSensorHandler) gravitySensorHandler).clearSensorData();
//        locationTracker.clearLocationData();
//    }

//    private void sendSensorData() {
//        JSONObject allSensorData = collectSensorData();
//        sendPostRequest(allSensorData);
//
//        // Clear sensor data after sending
//        ((AbstractSensorHandler) accelerometerHandler).clearSensorData();
//        ((AbstractSensorHandler) gyroscopeHandler).clearSensorData();
//        ((AbstractSensorHandler) magnetometerHandler).clearSensorData();
//        ((AbstractSensorHandler) lightSensorHandler).clearSensorData();
//        ((AbstractSensorHandler) humiditySensorHandler).clearSensorData();
//        ((AbstractSensorHandler) pressureSensorHandler).clearSensorData();
//        ((AbstractSensorHandler) gravitySensorHandler).clearSensorData();
//        locationTracker.clearLocationData(); // Ensure locationTracker has a method to clear data
//    }

    private void sendSensorData() {
        JSONArray allSensorData = collectSensorData();
        sendPostRequest(allSensorData);
        clearAllSensorData();
    }

    private void clearAllSensorData() {
        accelerometerHandler.clearSensorData();
        gyroscopeHandler.clearSensorData();
        magnetometerHandler.clearSensorData();
        lightSensorHandler.clearSensorData();
        humiditySensorHandler.clearSensorData();
        pressureSensorHandler.clearSensorData();
        gravitySensorHandler.clearSensorData();
        locationTracker.clearSensorData();
        if (soundLevelHandler != null) {
            soundLevelHandler.clearSensorData();
        }
    }

    private JSONArray collectSensorData() {
        JSONArray allSensorData = new JSONArray();
        try {
            // Directly add all elements of each sensor's data to the single array
            addAllSensorData(allSensorData, accelerometerHandler.getSensorData());
            addAllSensorData(allSensorData, gyroscopeHandler.getSensorData());
            addAllSensorData(allSensorData, lightSensorHandler.getSensorData());
            addAllSensorData(allSensorData, humiditySensorHandler.getSensorData());
            addAllSensorData(allSensorData, pressureSensorHandler.getSensorData());
            addAllSensorData(allSensorData, gravitySensorHandler.getSensorData());
            addAllSensorData(allSensorData, magnetometerHandler.getSensorData());
            addAllSensorData(allSensorData, locationTracker.getLocationData());
            if (soundLevelHandler != null) {
                addAllSensorData(allSensorData, soundLevelHandler.getSensorData());
            }
            // add other sensors similarly
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return allSensorData;
    }

    private void addAllSensorData(JSONArray allSensorData, JSONArray sensorData) throws JSONException {
        for (int i = 0; i < sensorData.length(); i++) {
            allSensorData.put(sensorData.get(i));
        }
    }


//    private JSONArray collectSensorData() {
//        JSONArray allSensorData = new JSONArray();
//        try {
//            // Concatenate all sensor data arrays into one array
//            allSensorData.put(new JSONArray(accelerometerHandler.getSensorData().toString()));
//            allSensorData.put(new JSONArray(gyroscopeHandler.getSensorData().toString()));
//            allSensorData.put(new JSONArray(lightSensorHandler.getSensorData().toString()));
//            allSensorData.put(new JSONArray(humiditySensorHandler.getSensorData().toString()));
//            allSensorData.put(new JSONArray(pressureSensorHandler.getSensorData().toString()));
//            allSensorData.put(new JSONArray(gravitySensorHandler.getSensorData().toString()));
//            allSensorData.put(new JSONArray(magnetometerHandler.getSensorData().toString()));
//            allSensorData.put(new JSONArray(locationTracker.getLocationData().toString()));
//            // add other sensors similarly
//        } catch (JSONException e) {
//            e.printStackTrace();
//        }
//        return allSensorData;
//    }

    // Code with second working version, where the expandable buttons have sensor names:
//    private JSONObject collectSensorData() {
//        JSONObject allSensorData = new JSONObject();
//        try {
//            allSensorData.put("accelerometer", ((AbstractSensorHandler) accelerometerHandler).getSensorData());
//            allSensorData.put("gyroscope", ((AbstractSensorHandler) gyroscopeHandler).getSensorData());
//            allSensorData.put("light", ((AbstractSensorHandler) lightSensorHandler).getSensorData());
//            allSensorData.put("humidity", ((AbstractSensorHandler) humiditySensorHandler).getSensorData());
//            allSensorData.put("pressure", ((AbstractSensorHandler) pressureSensorHandler).getSensorData());
//            allSensorData.put("gravity", ((AbstractSensorHandler) gravitySensorHandler).getSensorData());
//            allSensorData.put("magnetometer", ((AbstractSensorHandler) magnetometerHandler).getSensorData());
//            allSensorData.put("location", locationTracker.getLocationData());
//        } catch (JSONException e) {
//            e.printStackTrace();
//        }
//        return allSensorData;
//    }


    // The code with first working version:
//    private JSONArray collectSensorData() {
//        JSONArray allSensorData = new JSONArray();
//        allSensorData.put(((AbstractSensorHandler) accelerometerHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) gyroscopeHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) magnetometerHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) lightSensorHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) humiditySensorHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) pressureSensorHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) gravitySensorHandler).getSensorData());
//        allSensorData.put(locationTracker.getLocationData());
//        return allSensorData;
//    }

    private void startSensors() {
        accelerometerHandler.start();
        gyroscopeHandler.start();
        magnetometerHandler.start();
        lightSensorHandler.start();
        humiditySensorHandler.start();
        pressureSensorHandler.start();
        gravitySensorHandler.start();
        locationTracker.start();
        if (soundLevelHandler != null) { // Check if initialized
            soundLevelHandler.start();
        }
    }

    private void stopSensors() {
        accelerometerHandler.stop();
        gyroscopeHandler.stop();
        magnetometerHandler.stop();
        lightSensorHandler.stop();
        humiditySensorHandler.stop();
        pressureSensorHandler.stop();
        gravitySensorHandler.stop();
        locationTracker.stop();
        if (soundLevelHandler != null) { // Check if initialized
            soundLevelHandler.stop();
        }
    }


    private void sendPostRequest(JSONArray sensorData) {
        String url = "http://139.59.110.28:3838/sensorloggermobileappagent/update";    //https://eo5qowv4nlk2w03.m.pipedream.net   // http://139.59.110.28:3838/    //http://10.0.2.2:3838/sensorloggermobileappagent/update      //https://eou1bwdjb3p7r6h.m.pipedream.net   //http://192.168.1.60:3838/sensorloggermobileappagent/update
        RequestQueue queue = Volley.newRequestQueue(this);
        String sessionId = UUID.randomUUID().toString();
        String deviceId = DeviceIdManager.getDeviceId(this); // Assuming DeviceIdManager is accessible here

        // Create a JSONObject to structure the data according to the required format
        JSONObject postData = new JSONObject();
        try {
            postData.put("deviceId", deviceId);
            postData.put("messageId", messageId++);
            postData.put("payload", sensorData);
            postData.put("sessionId", sessionId);
        } catch (JSONException e) {
            e.printStackTrace();
            // Handle the error appropriately
        }

        // Convert the structured JSONObject to String
        final String requestBody = postData.toString();

        StringRequest postRequest = new StringRequest(Request.Method.POST, url,
                response -> Log.d("Response", response),
                error -> Log.d("Error.Response", error.toString())
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
    }
}

//    private int generateMessageId() {
//        return new Random().nextInt(1000);
//    }
//}






















//package com.example.notsensorlogger2;
//
//import android.app.Activity;
//import android.content.Intent;
//import android.os.Bundle;
//import android.hardware.SensorManager;
//import android.util.Log;
//import android.view.View;
//import android.widget.ImageView;
//
//import com.android.volley.Request;
//import com.android.volley.RequestQueue;
//import com.android.volley.Response;
//import com.android.volley.VolleyError;
//import com.android.volley.toolbox.StringRequest;
//import com.android.volley.toolbox.Volley;
//import com.example.notsensorlogger2.gpsfunctionality.MainActivity2;
//
//import org.json.JSONArray;
//import org.json.JSONException;
//import org.json.JSONObject;
//
//import java.nio.charset.StandardCharsets;
//import java.util.Random;
//import java.util.UUID;
//
//public class MainActivity extends Activity {
//    private SensorManager sensorManager;
//    private SensorHandler accelerometerHandler;
//    private SensorHandler gyroscopeHandler;
//    private SensorHandler magnetometerHandler;
//    private SensorHandler lightSensorHandler;
//    private SensorHandler humiditySensorHandler;
//    private SensorHandler pressureSensorHandler;
//    private SensorHandler gravitySensorHandler;
//    private LocationHandler locationTracker; // Make sure this is the updated LocationHandler
//    private String deviceId;
//
//    @Override
//    public final void onCreate(Bundle savedInstanceState) {
//        super.onCreate(savedInstanceState);
//        setContentView(R.layout.activity_main);
//
//        deviceId = DeviceIdManager.getDeviceId(this);
//        Log.d("DeviceId", "Device ID: " + deviceId);
//
//        sensorManager = (SensorManager) getSystemService(SENSOR_SERVICE);
//        accelerometerHandler = new AccelerometerHandler(sensorManager);
//        gyroscopeHandler = new GyroscopeHandler(sensorManager);
//        magnetometerHandler = new MagnetometerHandler(sensorManager);
//        lightSensorHandler = new LightSensorHandler(sensorManager);
//        humiditySensorHandler = new RelativeHumiditySensorHandler(sensorManager);
//        pressureSensorHandler = new PressureSensorHandler(sensorManager);
//        gravitySensorHandler = new GravitySensorHandler(sensorManager);
//        locationTracker = new LocationHandler(this); // Initialize the LocationHandler
//
//        ImageView imageView = (ImageView) findViewById(R.id.locationButton); // Replace with your ImageView ID
//        imageView.setOnClickListener(new View.OnClickListener() {
//            @Override
//            public void onClick(View v) {
//                Intent intent = new Intent(MainActivity.this, MainActivity2.class);
//                startActivity(intent);
//            }
//        });
//    }
//
//    @Override
//    protected void onResume() {
//        super.onResume();
//        accelerometerHandler.start();
//        gyroscopeHandler.start();
//        magnetometerHandler.start();
//        lightSensorHandler.start();
//        humiditySensorHandler.start();
//        pressureSensorHandler.start();
//        gravitySensorHandler.start();
//        locationTracker.start(); // Start the LocationHandler
//    }
//
//    @Override
//    protected void onPause() {
//        super.onPause();
//        accelerometerHandler.stop();
//        gyroscopeHandler.stop();
//        magnetometerHandler.stop();
//        lightSensorHandler.stop();
//        humiditySensorHandler.stop();
//        pressureSensorHandler.stop();
//        gravitySensorHandler.stop();
//        locationTracker.stop(); // Stop the LocationHandler
//
//        // Collect sensor data
//        JSONArray allSensorData = new JSONArray();
//        allSensorData.put(((AbstractSensorHandler) accelerometerHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) gyroscopeHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) magnetometerHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) lightSensorHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) humiditySensorHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) pressureSensorHandler).getSensorData());
//        allSensorData.put(((AbstractSensorHandler) gravitySensorHandler).getSensorData());
//        allSensorData.put(locationTracker.getLocationData());
//
//        // Send the sensor data
//        sendPostRequest(allSensorData);
//    }
//
//    private void sendPostRequest(JSONArray sensorData) {
//        String url = "https://eo5qowv4nlk2w03.m.pipedream.net"; // http://192.168.1.60:3838/sensorloggermobileappagent/update          //  https://eou1bwdjb3p7r6h.m.pipedream.net  //https://eo1kqlcacw69s9d.m.pipedream.net/
//        RequestQueue queue = Volley.newRequestQueue(this);
//
//        // Generate a session ID and message ID as needed
//        String sessionId = UUID.randomUUID().toString(); // Example, use an actual session management
//        int messageId = generateMessageId(); // Implement this method to generate a message ID
//
//        try {
//            // Wrap your sensor data inside a JSONObject with the expected structure
//            JSONObject payloadWrapper = new JSONObject();
//            payloadWrapper.put("deviceId", deviceId);
//            payloadWrapper.put("messageId", messageId);
//            payloadWrapper.put("payload", sensorData); // Your existing sensor data JSONArray
//            payloadWrapper.put("sessionId", sessionId);
//
//            // Convert the whole payload to a JSON string
//            final String requestBody = payloadWrapper.toString();
//
//            // Create the request with the proper body and headers
//            StringRequest postRequest = new StringRequest(Request.Method.POST, url,
//                    new Response.Listener<String>() {
//                        @Override
//                        public void onResponse(String response) {
//                            Log.d("Response", response);
//                        }
//                    },
//                    new Response.ErrorListener() {
//                        @Override
//                        public void onErrorResponse(VolleyError error) {
//                            Log.d("Error.Response", error.toString());
//                        }
//                    }
//            ) {
//                @Override
//                public byte[] getBody() {
//                    return requestBody.getBytes(StandardCharsets.UTF_8);
//                }
//
//                @Override
//                public String getBodyContentType() {
//                    return "application/json; charset=utf-8";
//                }
//            };
//
//            queue.add(postRequest);
//        } catch (JSONException e) {
//            e.printStackTrace();
//        }
//    }
//
//    private int generateMessageId() {
//        // Replace this with your own logic to generate a message ID
//        return new Random().nextInt(1000);
//    }
//
//}
