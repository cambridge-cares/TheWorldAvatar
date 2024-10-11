package uk.ac.cam.cares.jps.sensor.source.database;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.hilt.work.HiltWorker;
import androidx.work.ListenableWorker;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.GZIPOutputStream;

import javax.inject.Inject;

import dagger.assisted.Assisted;
import dagger.assisted.AssistedFactory;
import dagger.assisted.AssistedInject;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;

@HiltWorker
public class SensorUploadWorker extends Worker {

    private final SensorNetworkSource sensorNetworkSource;
    private final SensorLocalSource sensorLocalSource;
    private final Logger LOGGER = Logger.getLogger(SensorUploadWorker.class);
    private String deviceId;
    private List<SensorType> selectedSensors;

    @AssistedInject
    public SensorUploadWorker(@NonNull @Assisted Context context, @NonNull @Assisted WorkerParameters workerParams,
                              SensorNetworkSource sensorNetworkSource,
                              SensorLocalSource sensorLocalSource) {
        super(context, workerParams);
        this.sensorNetworkSource = sensorNetworkSource;
        this.sensorLocalSource = sensorLocalSource;
        deviceId = workerParams.getInputData().getString("deviceId");
        String selectedSensorsJson = workerParams.getInputData().getString("selectedSensors");
        selectedSensors = new ArrayList<>();

        try {
            JSONArray jsonArray = new JSONArray(selectedSensorsJson);
            for (int i = 0; i < jsonArray.length(); i++) {
                String sensorName = jsonArray.getString(i);
                selectedSensors.add(SensorType.valueOf(sensorName)); // Assuming SensorType is an Enum
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
            return Result.retry(); // Retry the work if there is a failure
        }
    }

    private void uploadSensorData() throws JSONException, IOException {
        int PAGE_SIZE = 1500;
        int offset = 0;
        boolean hasMoreData = true;

        while (hasMoreData) {
            JSONArray allSensorData = sensorLocalSource.retrieveUnUploadedSensorData(selectedSensors, PAGE_SIZE, offset);
            Log.i("DataUploadWorker", "Retrieved " + allSensorData.length() + " items from local storage.");

            if (allSensorData.length() < PAGE_SIZE) {
                hasMoreData = false;
            }

            String jsonString = allSensorData.toString();
            byte[] compressedData = compressData(jsonString);

            // Send to the network
            if (allSensorData.length() > 0) {
                LOGGER.info("Attempting to send " + allSensorData.length() + " items to the network.");
                LOGGER.info("All sensor data " + allSensorData);
                sensorNetworkSource.sendPostRequest(deviceId, compressedData, allSensorData);
                LOGGER.info("Accumulated data sent to network.");
            } else {
                LOGGER.info("No accumulated data to send to the network.");
            }
            offset += PAGE_SIZE;
        }
    }

    /**
     * Compresses a given string into a GZIP-compressed byte array.
     * This method uses the GZIP compression algorithm to reduce the size of the input string.
     *
     * @param data The input string to be compressed.
     * @return A byte array containing the GZIP-compressed data.
     * @throws IOException If an I/O error occurs during the compression process.
     */
    public static byte[] compressData(String data) throws IOException {
        try {
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            GZIPOutputStream gzipOutputStream = new GZIPOutputStream(byteArrayOutputStream);
            gzipOutputStream.write(data.getBytes("UTF-8"));
            gzipOutputStream.close();
            return byteArrayOutputStream.toByteArray();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

//    @AssistedFactory
//    public interface SensorUploadWorkerFactory {
//        SensorUploadWorker create(@NonNull Context appContext, @NonNull WorkerParameters workerParameters);
//    }

}
