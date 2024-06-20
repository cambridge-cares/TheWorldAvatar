package uk.ac.cam.cares.jps.sensor.database;

import android.content.Context;

import androidx.room.Room;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.sensor.database.model.AppDatabase;
import uk.ac.cam.cares.jps.sensor.database.model.LocationDao;
import uk.ac.cam.cares.jps.sensor.database.model.LocationData;

public class SensorLocalSource {
    Context context;
    LocationDao locationDao;
    Logger LOGGER = Logger.getLogger(SensorLocalSource.class);

    public SensorLocalSource(Context context) {
        this.context = context;
        AppDatabase appDatabase = Room.databaseBuilder(context, AppDatabase.class, "timeline-database").build();
        locationDao = appDatabase.locationDao();
    }

    public void writeToDatabase(Map<String, JSONArray> allSensorData) {
        JSONArray locations = allSensorData.get("location");
        List<LocationData> locationDataList = new ArrayList<>();
        for (int i = 0; i < locations.length(); i ++) {
//            locationDataList.add(new LocationData())
        }
//        locationDao.insertAll(locations);
    }
}
