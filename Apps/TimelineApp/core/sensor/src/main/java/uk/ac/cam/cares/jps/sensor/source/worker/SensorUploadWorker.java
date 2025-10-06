package uk.ac.cam.cares.jps.sensor.source.worker;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.hilt.work.HiltWorker;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static uk.ac.cam.cares.jps.utils.Utils.compressData;

import dagger.assisted.Assisted;
import dagger.assisted.AssistedInject;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;

@HiltWorker
public class SensorUploadWorker extends Worker {

    private final SensorNetworkSource sensorNetworkSource;
    private final SensorLocalSource sensorLocalSource;
    private final Logger LOGGER = Logger.getLogger(SensorUploadWorker.class);
    private String deviceId;
    private String sessionId;
    private List<SensorType> selectedSensors;

    @AssistedInject
    public SensorUploadWorker(@NonNull @Assisted Context context, @NonNull @Assisted WorkerParameters workerParams,
                              SensorNetworkSource sensorNetworkSource,
                              SensorLocalSource sensorLocalSource) {
        super(context, workerParams);
        this.sensorNetworkSource = sensorNetworkSource;
        this.sensorLocalSource = sensorLocalSource;
        deviceId = workerParams.getInputData().getString("deviceId");
        sessionId = workerParams.getInputData().getString("sessionId");
        String selectedSensorsJson = workerParams.getInputData().getString("selectedSensors");
        selectedSensors = new ArrayList<>();

        try {
            JSONArray jsonArray = new JSONArray(selectedSensorsJson);
            for (int i = 0; i < jsonArray.length(); i++) {
                String sensorName = jsonArray.getString(i).toUpperCase();
                selectedSensors.add(SensorType.valueOf(sensorName));
                LOGGER.info(SensorType.valueOf(sensorName));
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    @NonNull
    @Override
    public Result doWork() {
        // Retrieve data and upload
        try {
            uploadSensorData();

            return Result.success();
        } catch (Exception e) {
            Log.e("DataUploadWorker", "Error uploading sensor data", e);
            return Result.failure(); // data should be sent to local alr
        }
    }

    private void uploadSensorData() throws JSONException, IOException {
        int PAGE_SIZE = 800;
        int offset = 0;
        boolean hasMoreData = true;

        while (hasMoreData) {
            JSONArray allSensorData = sensorLocalSource.retrieveUnUploadedSensorData(selectedSensors, PAGE_SIZE, offset);

            if (allSensorData.length() < PAGE_SIZE) {
                hasMoreData = false;
            }

            String jsonString = allSensorData.toString();
            byte[] compressedData = compressData(jsonString);

            // Send to the network
            if (allSensorData.length() > 0) {
                LOGGER.info("Attempting to send " + allSensorData.length() + " items to the network.");
                sensorNetworkSource.sendPostRequest(deviceId, sessionId, compressedData, allSensorData);
                LOGGER.info("Accumulated data sent to network.");
            } else {
                LOGGER.info("No accumulated data to send to the network.");
            }
            offset += PAGE_SIZE;
        }

    }


}
