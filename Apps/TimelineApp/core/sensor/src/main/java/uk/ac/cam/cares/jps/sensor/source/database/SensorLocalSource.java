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
}



