package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.PrimaryKey;

import org.json.JSONObject;
import org.json.JSONException;


/**
 * An abstract base class representing generic sensor data.
 * This class is extended by specific sensor data entities.
 * It provides common properties and methods for handling sensor data.
 */
public abstract class SensorData {

    /**
     * The timestamp for the sensor data record.
     * Marked as the primary key for the database entity.
     */
    @PrimaryKey
    public long time;

    /**
     * Default constructor
     */
    public SensorData() {

    }

    /**
     * Constructs a sensor data object using a JSONObject.
     * @param jo JSONObject which contains time data.
     */
    public SensorData(JSONObject jo) {
        try {
            this.time = jo.getLong("time");
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }

    public abstract String toJSONString();
}
