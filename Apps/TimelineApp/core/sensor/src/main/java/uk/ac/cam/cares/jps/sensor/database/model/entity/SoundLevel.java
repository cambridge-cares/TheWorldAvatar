package uk.ac.cam.cares.jps.sensor.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

@Entity(tableName = "sound")
public class SoundLevel {
    @PrimaryKey
    public long time;

    public double dBFS;

    public SoundLevel() {}

    public SoundLevel(JSONObject jo) {
        try {
            // Extract time from the JSONObject
            this.time = jo.getLong("time");

            // Extract the nested values JSONObject
            JSONObject values = jo.getJSONObject("values");

            // Extract lux values from the nested JSONObject
            this.dBFS = values.getDouble("dBFS");
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }
}
