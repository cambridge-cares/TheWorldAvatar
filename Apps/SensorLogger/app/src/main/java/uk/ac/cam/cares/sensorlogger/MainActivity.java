package uk.ac.cam.cares.sensorlogger;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.hardware.SensorManager;
import android.os.Handler;
import android.util.Log;
import android.widget.Button;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.toolbox.StringRequest;
import com.android.volley.toolbox.Volley;
import uk.ac.cam.cares.sensorlogger.gpsfunctionality.MainActivityMap;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import android.Manifest;

/**
 * The MainActivity is the central hub for managing various sensor handlers and initiating sensor data collection.
 * It initializes and manages accelerometer, gyroscope, magnetometer, light, humidity, pressure, gravity,
 * sound level sensors, and location tracking. This activity provides functionality to start and stop the collection
 * of data from these sensors and send this data to a specified server endpoint periodically.
 *
 * This activity also handles runtime permissions necessary for accessing sensors and location data.
 * It sets up a user interface with a RecyclerView to toggle sensors on and off and a button to start and stop data collection.
 * Additionally, there's functionality to navigate to a secondary activity that handles specific GPS-based functionalities.
 */
public class MainActivity extends Activity {
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
    private int messageId = 1;
    private static final int PERMISSION_REQUEST_RECORD_AUDIO = 101;


    /**
     * Initializes the list of sensors and their respective handlers and sets up the RecyclerView for sensor control.
     * This method populates the sensorItems list with all available sensors and assigns each a corresponding handler.
     * It also configures the adapter for the RecyclerView, setting toggle listeners for each sensor to start or stop data collection.
     */
    private void setupSensorList() {
        // Initialize sensor handler list
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

        // Initialize RecyclerView and Adapter
        sensorsRecyclerView = findViewById(R.id.sensors_recycler_view);
        sensorsRecyclerView.setLayoutManager(new LinearLayoutManager(this));
        sensorAdapter = new SensorAdapter(sensorItems, (sensor, isChecked) -> {
            sensor.setToggled(isChecked);
            if (isChecked) {
                sensor.getHandler().start();
            } else {
                sensor.getHandler().stop();
            }
        }, sensor -> {
            // Placeholder for future implementation if more details are needed
        });
        sensorsRecyclerView.setAdapter(sensorAdapter);
    }

    /**
     * Checks if the audio recording permission is granted and requests it if not.
     * Necessary for initializing the SoundLevelHandler.
     */
    private void checkAndRequestAudioPermission() {
        if (ActivityCompat.checkSelfPermission(this, Manifest.permission.RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.RECORD_AUDIO}, PERMISSION_REQUEST_RECORD_AUDIO);
        }
    }

    /**
     * Callback for the result from requesting permissions. This method is invoked for every call on requestPermissions.
     *
     * @param requestCode  The request code passed in requestPermissions.
     * @param permissions  The requested permissions. Never null.
     * @param grantResults The grant results for the corresponding permissions which is either PERMISSION_GRANTED or PERMISSION_DENIED. Never null.
     */
    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == PERMISSION_REQUEST_RECORD_AUDIO) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                soundLevelHandler.initAudioRecord();
            }
            }
        }

    /**
     * Sets up sensor list and RecyclerView for sensor toggling, initializes sensor handlers, and checks permissions.
     * Initializes and configures buttons for controlling data collection and navigating to other activities.
     *
     * @param savedInstanceState Contains data supplied in onSaveInstanceState(Bundle) or null if no data was supplied.
     */
    public final void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        deviceId = DeviceIdManager.getDeviceId(this);
        Log.d("DeviceId", "Device ID: " + deviceId);

        SensorManager sensorManager = (SensorManager) getSystemService(SENSOR_SERVICE);
        accelerometerHandler = new AccelerometerHandler(sensorManager);
        gyroscopeHandler = new GyroscopeHandler(sensorManager);
        magnetometerHandler = new MagnetometerHandler(sensorManager);
        lightSensorHandler = new LightSensorHandler(sensorManager);
        humiditySensorHandler = new RelativeHumiditySensorHandler(sensorManager);
        pressureSensorHandler = new PressureSensorHandler(sensorManager);
        gravitySensorHandler = new GravitySensorHandler(sensorManager);
        locationTracker = new LocationHandler((Context) this);
        soundLevelHandler = new SoundLevelHandler(this, sensorManager);

        // After initializing all sensors and handlers, call the setup method for RecyclerView
        setupSensorList();

        Button stopDataButton = findViewById(R.id.start_recording_button);
        stopDataButton.setText(R.string.start_button_text);

        isDataCollectionActive = false;

        stopDataButton.setOnClickListener(v -> {
            if (!isDataCollectionActive) {
                startDataCollection();
                stopDataButton.setText(R.string.stop_button_text);
            } else {
                stopDataCollection();
                stopDataButton.setText(R.string.start_button_text);
            }
        });


        Button btnOpenMap = findViewById(R.id.open_map_button);
        btnOpenMap.setOnClickListener(v -> {
            Intent intent = new Intent(this, MainActivityMap.class);
            startActivity(intent);
        });

        sendDataRunnable = new Runnable() {
            @Override
            public void run() {
                sendSensorData();
                handler.postDelayed(this, SEND_INTERVAL);
            }
        };
    }

    /**
     * Resumes the activity, checks necessary permissions, and resumes data collection if it was active before pausing.
     */
    @Override
    protected void onResume() {
        super.onResume();
        checkAndRequestAudioPermission();
        // Always attempt to restart data collection if it was active.
        if (isDataCollectionActive) {
            startDataCollection();
        }
    }

    /**
     * Pauses the activity and stops data collection if it was active.
     */
    @Override
    protected void onPause() {
        super.onPause();
        if (isDataCollectionActive) {
            stopDataCollection();
        }
    }

    /**
     * Starts the data collection from all active sensors and schedules periodic data sending to the server.
     */
    private void startDataCollection() {
        messageId = 1;  // Reset messageId for a new collection session.
        for (SensorItem item : sensorItems) {
            if (item.isToggled() && item.getHandler() != null) {
                item.getHandler().start();
            }
        }
        // Always post the runnable if this method is called.
        handler.postDelayed(sendDataRunnable, SEND_INTERVAL);
        isDataCollectionActive = true; // Indicate that data collection is active.
    }

    /**
     * Stops the data collection from all sensors and cancels the periodic sending of data to the server.
     */
    private void stopDataCollection() {
        if (isDataCollectionActive) {
            for (SensorItem item : sensorItems) {
                if (item.isToggled() && item.getHandler() != null) {
                    item.getHandler().stop();
                }
            }
            handler.removeCallbacks(sendDataRunnable);
            isDataCollectionActive = false;
        }
    }

    /**
     * Sends the collected sensor data to the server.
     */
    private void sendSensorData() {
        JSONArray allSensorData = collectSensorData();
        sendPostRequest(allSensorData);
        clearAllSensorData();
    }

    /**
     * Clears all sensor data from the memory.
     */
    private void clearAllSensorData() {
        accelerometerHandler.clearSensorData();
        gyroscopeHandler.clearSensorData();
        magnetometerHandler.clearSensorData();
        lightSensorHandler.clearSensorData();
        humiditySensorHandler.clearSensorData();
        pressureSensorHandler.clearSensorData();
        gravitySensorHandler.clearSensorData();
        locationTracker.clearSensorData();
        soundLevelHandler.clearSensorData();

    }

    /**
     * Collects data from all initialized sensors and returns it in a JSONArray.
     *
     * @return JSONArray containing data from all sensors.
     */
    private JSONArray collectSensorData() {
        JSONArray allSensorData = new JSONArray();
        try {
            addAllSensorData(allSensorData, accelerometerHandler.getSensorData());
            addAllSensorData(allSensorData, gyroscopeHandler.getSensorData());
            addAllSensorData(allSensorData, lightSensorHandler.getSensorData());
            addAllSensorData(allSensorData, humiditySensorHandler.getSensorData());
            addAllSensorData(allSensorData, pressureSensorHandler.getSensorData());
            addAllSensorData(allSensorData, gravitySensorHandler.getSensorData());
            addAllSensorData(allSensorData, magnetometerHandler.getSensorData());
            addAllSensorData(allSensorData, locationTracker.getSensorData());
            addAllSensorData(allSensorData, soundLevelHandler.getSensorData());
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return allSensorData;
    }

    /**
     * Adds sensor data from a specific sensor into a collective JSONArray.
     *
     * @param allSensorData JSONArray containing all collected sensor data.
     * @param sensorData    JSONArray containing data from a specific sensor.
     * @throws JSONException If there is a problem parsing the data.
     */
    private void addAllSensorData(JSONArray allSensorData, JSONArray sensorData) throws JSONException {
        for (int i = 0; i < sensorData.length(); i++) {
            allSensorData.put(sensorData.get(i));
        }
    }

    /**
     * Configures and sends a POST request with sensor data to the server.
     *
     * @param sensorData JSONArray containing the sensor data to send.
     */
    private void sendPostRequest(JSONArray sensorData) {
        String url = getString(R.string.data_post_url);  // Use actual url and fill it in, in strings.xml
        RequestQueue queue = Volley.newRequestQueue(this);
        String sessionId = UUID.randomUUID().toString();
        String deviceId = DeviceIdManager.getDeviceId(this);

        JSONObject postData = new JSONObject();
        try {
            postData.put("deviceId", deviceId);
            postData.put("messageId", messageId++);
            postData.put("payload", sensorData);
            postData.put("sessionId", sessionId);
        } catch (JSONException e) {
            e.printStackTrace();
        }

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
