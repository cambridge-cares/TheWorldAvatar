package uk.ac.cam.cares.jps.sensor.database;

import android.content.Context;

import androidx.room.Room;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;

import java.util.HashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.sensor.database.model.entity.Acceleration;
import uk.ac.cam.cares.jps.sensor.database.model.dao.AccelerationDao;
import uk.ac.cam.cares.jps.sensor.database.model.AppDatabase;
import uk.ac.cam.cares.jps.sensor.database.model.dao.LocationDao;
import uk.ac.cam.cares.jps.sensor.database.model.entity.LocationData;

public class SensorLocalSource {
    Context context;
    LocationDao locationDao;
    AccelerationDao accelerationDao;
    Logger LOGGER = Logger.getLogger(SensorLocalSource.class);

    public SensorLocalSource(Context context) {
        this.context = context;
        AppDatabase appDatabase = Room.databaseBuilder(context, AppDatabase.class, "timeline-database").build();
        locationDao = appDatabase.locationDao();
        accelerationDao = appDatabase.accelerationDao();
    }

    public void writeToDatabase(Map<String, JSONArray> allSensorData) {
        LOGGER.info("Start to write to database");
        JSONArray locations = allSensorData.get("location");
        Map<Long, LocationData> locationDataList = new HashMap<>();
        for (int i = 0; i < locations.length(); i ++) {
            try {
                LocationData locationData = new LocationData(locations.getJSONObject(i));
                locationDataList.put(locationData.time, locationData);
            } catch (JSONException e) {
                LOGGER.error(e.getMessage());
                throw new RuntimeException(e);
            }
        }
        LocationData[] locationDataArray = new LocationData[locationDataList.size()];
        locationDataArray = locationDataList.values().toArray(locationDataArray);
        locationDao.insertAll(locationDataArray);

        LOGGER.info("finish writing location: " + locations.length());

        JSONArray accelerations = allSensorData.get("accelerometer");
        Map<Long, Acceleration> accelerationList = new HashMap<>();
        for (int i = 0; i < accelerations.length(); i ++) {
            try {
                Acceleration acceleration = new Acceleration(accelerations.getJSONObject(i));
                accelerationList.put(acceleration.time, acceleration);
            } catch (JSONException e) {
                LOGGER.error(e.getMessage());
                throw new RuntimeException(e);
            }
        }
        Acceleration[] accelerationArray = new Acceleration[accelerationList.size()];
        accelerationArray = accelerationList.values().toArray(accelerationArray);
        accelerationDao.insertAll(accelerationArray);

        LOGGER.info("finish writing acceleration: " + accelerations.length());
    }
}
