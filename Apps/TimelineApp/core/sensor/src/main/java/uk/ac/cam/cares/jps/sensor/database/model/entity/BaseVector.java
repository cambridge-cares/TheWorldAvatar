package uk.ac.cam.cares.jps.sensor.database.model.entity;

import androidx.room.Ignore;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

public class BaseVector {
    @PrimaryKey
    public long time;

    public double x;

    public double y;

    public double z;

    @Ignore
    public String sensorName;

    public BaseVector() {}

    public BaseVector(long time, double x, double y, double z) {
        this.time = time;
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public BaseVector(JSONObject jo) {
        try {
            // Extract time from the JSONObject
            this.time = jo.getLong("time");

            // Extract the nested values JSONObject
            JSONObject values = jo.getJSONObject("values");

            // Extract x, y, z values from the nested JSONObject
            this.x = values.getDouble("x");
            this.y = values.getDouble("y");
            this.z = values.getDouble("z");
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }

    public String toJSONString() {
        try {
            JSONObject values = new JSONObject();
            values.put("x", x);
            values.put("y", y);
            values.put("z", z);

            JSONObject dataPoint = new JSONObject();
            dataPoint.put("name", this.sensorName);
            dataPoint.put("time", time);
            dataPoint.put("values", values);
            return values.toString();
        } catch (JSONException je) {
            je.printStackTrace();
        }
        return "";
    }
}
