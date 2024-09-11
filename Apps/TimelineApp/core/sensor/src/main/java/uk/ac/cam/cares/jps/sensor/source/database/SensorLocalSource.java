package uk.ac.cam.cares.jps.sensor.source.database;

import android.content.Context;
import android.util.Log;

import androidx.lifecycle.LiveDataKt;
import androidx.room.Room;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import uk.ac.cam.cares.jps.sensor.source.database.model.dao.GravityDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.GyroDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.LightDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.MagnetFieldStrengthDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.PressureDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.RelativeHumidityDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.SensorDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.SoundLevelDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.UnsentDataDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Acceleration;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.AccelerationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.AppDatabase;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.LocationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Gravity;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.GyroData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.LightData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.LocationData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.MagnetFieldStrength;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Pressure;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.RelativeHumidity;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.SensorData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.SoundLevel;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.UnsentData;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;


/**
 * A class that commits data to the local database.
 */
public class SensorLocalSource {
    Context context;
    public LocationDao locationDao;
    public AccelerationDao accelerationDao;
    public GravityDao gravityDao;
    public GyroDao gyroDao;
    public LightDao lightDao;
    public MagnetFieldStrengthDao magnetFieldStrengthDao;
    public PressureDao pressureDao;
    public UnsentDataDao unsentDataDao;
    public RelativeHumidityDao relativeHumidityDao;
    public SoundLevelDao soundLevelDao;
    Logger LOGGER = Logger.getLogger(SensorLocalSource.class);
    Map<String, JSONArray> unsentData;


    public SensorLocalSource(Context context) {
        this.context = context;
        AppDatabase appDatabase = Room.databaseBuilder(context, AppDatabase.class, "timeline-database").build();
        locationDao = appDatabase.locationDao();
        accelerationDao = appDatabase.accelerationDao();
        gravityDao = appDatabase.gravityDao();
        gyroDao = appDatabase.gyroDao();
        lightDao = appDatabase.lightDao();
        magnetFieldStrengthDao = appDatabase.magnetFieldStrengthDao();
        pressureDao = appDatabase.pressureDao();
        soundLevelDao = appDatabase.soundLevelDao();
        relativeHumidityDao = appDatabase.relativeHumidityDao();
        this.unsentDataDao = appDatabase.unsentDataDao();
        this.unsentData = new HashMap<>();

    }


    /**
     * Commits sensor data to the local database.
     * The data is provided as a map, where the key is the sensor type
     * and the value is an array of data points.
     *
     * @param allSensorData map containing sensor type as key and corresponding data values as JSON arrays.
     */
    public void writeToDatabase(Map<String, JSONArray> allSensorData) {
        LOGGER.info("Start to write to database");

        writeToDatabaseHelper(allSensorData, "location", locationDao, LocationData.class);
        writeToDatabaseHelper(allSensorData, "accelerometer", accelerationDao, Acceleration.class);
        writeToDatabaseHelper(allSensorData, "gravity", gravityDao, Gravity.class);
        writeToDatabaseHelper(allSensorData, "gyroscope", gyroDao, GyroData.class);
        writeToDatabaseHelper(allSensorData, "light", lightDao, LightData.class);
        writeToDatabaseHelper(allSensorData, "magnetometer", magnetFieldStrengthDao, MagnetFieldStrength.class);
        writeToDatabaseHelper(allSensorData, "pressure", pressureDao, Pressure.class);
        writeToDatabaseHelper(allSensorData, "microphone", soundLevelDao, SoundLevel.class);
        writeToDatabaseHelper(allSensorData, "humidity", relativeHumidityDao, RelativeHumidity.class);

    }

    /**
     * A helper method that inserts data from a map into the local database.
     *
     * @param allSensorData the data needing to be commited to the local database
     * @param sensorType    the type of sensor the data corresponds to
     * @param sensorDao     the dao that corresponds to the data
     * @param sensorClass   the sensor class of the data
     * @param <T>           generic type
     */
    private <T extends SensorData> void writeToDatabaseHelper(Map<String, JSONArray> allSensorData, String sensorType,
                                                              SensorDao<T> sensorDao, Class<T> sensorClass) {
        JSONArray sensorArray = allSensorData.get(sensorType);

        if (sensorArray != null) {
            try {
                Map<Long, T> sensorDataList = new HashMap<>();
                for (int i = 0; i < sensorArray.length(); i++) {
                    JSONObject jsonObject = sensorArray.getJSONObject(i);
                    T sensorData = sensorClass.getConstructor(JSONObject.class).newInstance(jsonObject);
                    sensorDataList.put(sensorData.time, sensorData);
                }

                T[] sensorDataArray = sensorDataList.values().toArray((T[]) java.lang.reflect.Array.newInstance(sensorClass, sensorDataList.size()));
                sensorDao.insertAll(sensorDataArray); // maybe better to collect into a list and insert all at once
                LOGGER.info("Finished writing " + sensorType + ": " + sensorArray.length());
            } catch (JSONException | NoSuchMethodException e) {
                LOGGER.error("Error processing sensor data for type " + sensorType + ": " + e.getMessage());
                throw new RuntimeException("Could not write data to database", e);
            } catch (InvocationTargetException e) {
                throw new RuntimeException(e);
            } catch (IllegalAccessException e) {
                throw new RuntimeException(e);
            } catch (InstantiationException e) {
                throw new RuntimeException(e);
            }
        }
    }


    /**
     * Inserts an unsent data record into the local database.
     * This data will be stored until the network is available for transmission.
     *
     * @param unsentData the unsent data record to be inserted.
     */
    public void insertUnsentData(UnsentData unsentData) {
        try {
            ExecutorService executor = Executors.newSingleThreadExecutor();
            executor.execute(() -> {
                this.unsentDataDao.insert(unsentData);
            });

            Log.e("local source", "unsent data inserted");
        } catch (Exception e){
            Log.e("local source", "error inserting unsent data", e);
        }
    }

    /**
     * Retrieves unsent data from the local database.
     *
     * @return a list of {@link UnsentData} objects
     */
    public List<UnsentData> retrieveUnsentData() {
        return unsentDataDao.getAllUnsentData();
    }

    public void deleteUnsentData(UnsentData unsentData) {
        try {
            ExecutorService executor = Executors.newSingleThreadExecutor();
            executor.execute(() -> {
        unsentDataDao.deleteById(unsentData.id);
            });
        Log.e("local source", "unsent data deleted");
            } catch (Exception e){
                Log.e("local source", "error deleting unsent data", e);
            }
    }

    /**
     * Deletes data stored longer than a determined cutoff time. Initialized in onStartCommand in
     * {@link uk.ac.cam.cares.jps.sensor.SensorService}
     *
     * @param cutoffTime predetermined time that establishes what data should be deleted
     */
    public void deleteHistoricalData(long cutoffTime) {
        locationDao.delete(cutoffTime);
        accelerationDao.delete(cutoffTime);
        gravityDao.delete(cutoffTime);
        gyroDao.delete(cutoffTime);
        lightDao.delete(cutoffTime);
        magnetFieldStrengthDao.delete(cutoffTime);
        pressureDao.delete(cutoffTime);
        soundLevelDao.delete(cutoffTime);
        relativeHumidityDao.delete(cutoffTime);
    }


    /**
     * Retrieves unsent sensor data from the local database for the specified sensor types,
     * paginates the results using limit and offset, and prepares the data for upload.
     * Also, marks the retrieved data's timestamps for later marking as uploaded.
     *
     * @param selectedSensors A list of {@link SensorType} representing the sensors for which to retrieve data.
     * @param limit The maximum number of records to retrieve for each sensor.
     * @param offset The starting point for retrieving records (for pagination).
     * @return A {@link JSONArray} containing the unsent sensor data for all specified sensors.
     */
    public JSONArray retrieveUnsentSensorData(List<SensorType> selectedSensors, int limit, int offset) {
        List<SensorData> allSensorData = new ArrayList<>();
        Map<String, List<Long>> timesToMarkAsUploaded = new HashMap<>();

        // Check each selected sensor and retrieve data accordingly
        for (SensorType sensor : selectedSensors) {
            switch(sensor) {
                case LOCATION:
                    List<LocationData> locationDataList = Arrays.asList(locationDao.getAllUnsent(limit, offset));
                    allSensorData.addAll(locationDataList);
                    timesToMarkAsUploaded.put(String.valueOf(sensor), extractTimes(locationDataList));
                    break;
                case ACCELEROMETER:
                    List<Acceleration> accelerationDataList = Arrays.asList(accelerationDao.getAllUnsent(limit, offset));
                    allSensorData.addAll(accelerationDataList);
                    timesToMarkAsUploaded.put(String.valueOf(sensor), extractTimes(accelerationDataList));
                    break;
                case GRAVITY:
                    List<Gravity> gravityDataList = Arrays.asList(gravityDao.getAllUnsent(limit, offset));
                    allSensorData.addAll(gravityDataList);
                    timesToMarkAsUploaded.put(String.valueOf(sensor), extractTimes(gravityDataList));
                    break;
                case GYROSCOPE:
                    List<GyroData> gyroDataList = Arrays.asList(gyroDao.getAllUnsent(limit, offset));
                    allSensorData.addAll(gyroDataList);
                    timesToMarkAsUploaded.put(String.valueOf(sensor), extractTimes(gyroDataList));
                    break;
                case LIGHT:
                    List<LightData> lightDataList = Arrays.asList(lightDao.getAllUnsent(limit, offset));
                    allSensorData.addAll(lightDataList);
                    timesToMarkAsUploaded.put(String.valueOf(sensor), extractTimes(lightDataList));
                    break;
                case MAGNETOMETER:
                    List<MagnetFieldStrength> magnetDataList = Arrays.asList(magnetFieldStrengthDao.getAllUnsent(limit, offset));
                    allSensorData.addAll(magnetDataList);
                    timesToMarkAsUploaded.put(String.valueOf(sensor), extractTimes(magnetDataList));
                    break;
                case PRESSURE:
                    List<Pressure> pressureDataList = Arrays.asList(pressureDao.getAllUnsent(limit, offset));
                    allSensorData.addAll(pressureDataList);
                    timesToMarkAsUploaded.put(String.valueOf(sensor), extractTimes(pressureDataList));
                    break;
                case SOUND:
                    List<SoundLevel> soundDataList = Arrays.asList(soundLevelDao.getAllUnsent(limit, offset));
                    allSensorData.addAll(soundDataList);
                    timesToMarkAsUploaded.put(String.valueOf(sensor), extractTimes(soundDataList));
                    break;
                case HUMIDITY:
                    List<RelativeHumidity> humidityDataList = Arrays.asList(relativeHumidityDao.getAllUnsent(limit, offset));
                    allSensorData.addAll(humidityDataList);
                    timesToMarkAsUploaded.put(String.valueOf(sensor), extractTimes(humidityDataList));
                    break;
            }
        }
        JSONArray allSensorDataArray = new JSONArray();

        for(SensorData sensorData : allSensorData) {
            allSensorDataArray.put(sensorData.toJson());
        }

        // Mark each type of data as being sent to get uploaded
        for (Map.Entry<String, List<Long>> entry : timesToMarkAsUploaded.entrySet()) {
            markDataAsBeingUploaded(SensorType.valueOf(entry.getKey()), entry.getValue());
        }



        return allSensorDataArray;
    }

    /**
     * Marks sensor data as uploaded for a specific sensor type by updating the 'uploaded' field
     * for the provided timestamps.
     *
     * @param sensorType The {@link SensorType} for which the data is being marked as uploaded.
     * @param times A list of timestamps representing the data that should be marked as uploaded.
     */
    public void markDataAsBeingUploaded(SensorType sensorType, List<Long> times) {
        switch(sensorType) {
            case LOCATION:
                locationDao.markAsUploaded(times);
                break;
            case ACCELEROMETER:
                accelerationDao.markAsUploaded(times);
                break;
            case GRAVITY:
                gravityDao.markAsUploaded(times);
                break;
            case GYROSCOPE:
                gyroDao.markAsUploaded(times);
                break;
            case LIGHT:
                lightDao.markAsUploaded(times);
                break;
            case MAGNETOMETER:
                magnetFieldStrengthDao.markAsUploaded(times);
                break;
            case PRESSURE:
                pressureDao.markAsUploaded(times);
                break;
            case SOUND:
                soundLevelDao.markAsUploaded(times);
                break;
            case HUMIDITY:
                relativeHumidityDao.markAsUploaded(times);
                break;
        }
    }


    /**
     * Extracts the timestamps from a list of sensor data objects.
     *
     * @param sensorDataList The list of sensor data from which to extract timestamps.
     * @return A list of timestamps extracted from the sensor data.
     */
    private List<Long> extractTimes(List<? extends SensorData> sensorDataList) {
        List<Long> times = new ArrayList<>();
        for (SensorData data : sensorDataList) {
            times.add(data.time);
        }
        return times;
    }



}



