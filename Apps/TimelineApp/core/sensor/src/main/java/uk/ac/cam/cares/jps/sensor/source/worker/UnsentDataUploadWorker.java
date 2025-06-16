package uk.ac.cam.cares.jps.sensor.source.worker;

import static uk.ac.cam.cares.jps.utils.di.UtilsModule.compressData;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.hilt.work.HiltWorker;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import dagger.assisted.Assisted;
import dagger.assisted.AssistedInject;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.UnsentData;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;

@HiltWorker
public class UnsentDataUploadWorker extends Worker {
    private final SensorLocalSource sensorLocalSource;
    private final SensorNetworkSource sensorNetworkSource;
    private final Logger LOGGER = Logger.getLogger(UnsentDataUploadWorker.class);

    // TODO: taskId should be in the stored data
    private final String taskId;

    @AssistedInject
    public UnsentDataUploadWorker(@Assisted @NonNull Context context, @Assisted @NonNull WorkerParameters workerParams,
                                  SensorLocalSource sensorLocalSource,
                                  SensorNetworkSource sensorNetworkSource) {
        super(context, workerParams);
        this.sensorLocalSource = sensorLocalSource;
        this.sensorNetworkSource = sensorNetworkSource;
        this.taskId = workerParams.getInputData().getString("taskId");
    }

    @NonNull
    @Override
    public Result doWork() {
        try {
            uploadUnsentData();

            return Result.success();
        } catch (Exception e) {
            Log.e("DataUploadWorker", "Error uploading sensor data", e);
            return Result.failure(); // data should be sent to local alr
        }
    }

    private void uploadUnsentData() {
        int limit = 100;
        int offset = 0;
        List<UnsentData> unsentDataList;

        do {
            LOGGER.info("offset: " + offset);
            unsentDataList = sensorLocalSource.retrieveUnsentData(limit, offset);
            offset += unsentDataList.size();

            Map<String, String> deviceIdToPayloads = combinePayload(unsentDataList);
            for (Map.Entry<String, String> entry : deviceIdToPayloads.entrySet()) {
                String deviceId = entry.getKey();
                String payload = entry.getValue();

                try {
                    byte[] compressedData = compressData(payload);
                    sensorNetworkSource.sendPostRequest(deviceId, taskId, compressedData, payload);
                    sensorLocalSource.deleteUnsentData(unsentDataList);
                    LOGGER.info("unsent data operations completed");
                } catch (Exception e) {
                    LOGGER.error("Error processing unsent data", e);
                }
            }
        } while (!unsentDataList.isEmpty());
    }

    private Map<String, String> combinePayload(List<UnsentData> unsentDataList) {
        Map<String, String> deviceIdToPayload = new HashMap<>();
        for (UnsentData unsentData : unsentDataList) {
            String combinedPayload = deviceIdToPayload.getOrDefault(unsentData.deviceId, "") + unsentData.data.substring(1, unsentData.data.length() -1);
            deviceIdToPayload.put(unsentData.deviceId, combinedPayload);
        }
        for (String deviceId : deviceIdToPayload.keySet()) {
            deviceIdToPayload.put(deviceId, String.format("[%s]", deviceIdToPayload.get(deviceId)));
        }
        return deviceIdToPayload;
    }
}
