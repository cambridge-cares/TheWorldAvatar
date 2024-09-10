package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a Sound sensor data entity in the database.
 * This class extends {@link SensorData} and stores sound-specific attributes.
 * The data is stored in the "sound" table in the database.
 */
@Entity(tableName = "sound")
public class SoundLevel extends SensorData {

    /**
     * decibels relative to full scale
     */
    public double dBFS;

    /**
     * Default constructor
     */
    public SoundLevel() {}

    /**
     * Constructs a sound object using a JSONObject
     * @param jo JSONObject which contains sound data.
     */
    public SoundLevel(JSONObject jo) {

        // get time from JSONObject from abstract class
        super(jo);

        try {
            // Extract the nested values JSONObject
            JSONObject values = jo.getJSONObject("values");

            // Extract lux values from the nested JSONObject
            this.dBFS = values.getDouble("dBFS");
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String toJSONString() {
        return "";
    }
}
