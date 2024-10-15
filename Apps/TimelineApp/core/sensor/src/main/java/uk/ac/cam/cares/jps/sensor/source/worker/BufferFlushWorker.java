package uk.ac.cam.cares.jps.sensor.source.worker;

import android.content.Context;
import androidx.annotation.NonNull;
import androidx.hilt.work.HiltWorker;
import androidx.work.OneTimeWorkRequest;
import androidx.work.WorkManager;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import org.apache.log4j.Logger;
import org.json.JSONArray;

import dagger.assisted.Assisted;
import dagger.assisted.AssistedInject;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorHandlerManager;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;


@HiltWorker
public class BufferFlushWorker extends Worker {

    private final SensorHandlerManager sensorHandlerManager;
    private final SensorLocalSource sensorLocalSource;

    private final Logger LOGGER = Logger.getLogger(BufferFlushWorker.class);
    private final Map<String, JSONArray> memoryBuffer = new HashMap<>();
    private static final long THIRTY_DAYS_IN_MILLIS = 30L * 24 * 60 * 60 * 1000;

    @AssistedInject
    public BufferFlushWorker(@Assisted @NonNull Context context, @Assisted @NonNull WorkerParameters workerParams,
                             SensorHandlerManager sensorHandlerManager,
                             SensorLocalSource sensorLocalSource) {
        super(context, workerParams);
        this.sensorHandlerManager = sensorHandlerManager;
        this.sensorLocalSource = sensorLocalSource;
    }

    @NonNull
    @Override
    public Result doWork() {
        try {
            Map<String, JSONArray> localData = sensorHandlerManager.collectSensorData();
            for (Map.Entry<String, JSONArray> entry : localData.entrySet()) {
                String sensorName = entry.getKey();
                JSONArray newData = entry.getValue();

                JSONArray bufferedData = memoryBuffer.computeIfAbsent(sensorName, k -> new JSONArray());

                for (int i = 0; i < newData.length(); i++) {
                    try {
                        bufferedData.put(newData.get(i));
                    } catch (Exception e) {
                        LOGGER.error("Error adding data to buffer", e);
                    }
                }
            }

            // Write buffered data to local storage
            sensorLocalSource.writeToDatabase(memoryBuffer);
            // Clear the buffer after flushing to local storage
            memoryBuffer.clear();

            LOGGER.info("Buffer flushed to local storage.");

            // Handle deleting old data
            long cutoffTime = System.currentTimeMillis() - THIRTY_DAYS_IN_MILLIS;
            sensorLocalSource.deleteHistoricalData(cutoffTime);


            return Result.success();
        } catch (Exception e) {
            LOGGER.error("Error flushing buffer", e);
            return Result.retry();
        }
    }

}
