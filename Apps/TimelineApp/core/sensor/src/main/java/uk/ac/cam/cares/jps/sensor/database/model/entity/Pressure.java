package uk.ac.cam.cares.jps.sensor.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

@Entity(tableName = "pressure")
public class Pressure {
    @PrimaryKey
    public long time;

    public double pressure;

    public Pressure() {}

    public Pressure(JSONObject jo) {
        try {
            // Extract time from the JSONObject
            this.time = jo.getLong("time");

            // Extract the nested values JSONObject
            JSONObject values = jo.getJSONObject("values");

            // Extract lux values from the nested JSONObject
            this.pressure = values.getDouble("pressure");
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }
}
