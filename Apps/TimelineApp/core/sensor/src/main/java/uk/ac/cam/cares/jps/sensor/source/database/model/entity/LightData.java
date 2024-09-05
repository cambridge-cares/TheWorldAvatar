package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

@Entity(tableName = "light")
public class LightData {
    @PrimaryKey
    public long time;

    public double lux;

    public LightData() {}

    public LightData(JSONObject jo) {
        try {
            // Extract time from the JSONObject
            this.time = jo.getLong("time");

            // Extract the nested values JSONObject
            JSONObject values = jo.getJSONObject("values");

            // Extract lux values from the nested JSONObject
            this.lux = values.getDouble("lux");
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }
}
