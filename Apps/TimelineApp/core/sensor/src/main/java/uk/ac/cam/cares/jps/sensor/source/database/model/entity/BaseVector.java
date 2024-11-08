package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Ignore;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;
/**
 * A base class representing vector sensor data with x, y, and z components.
 * This class is extended by specific sensor data entities like {@link Acceleration}.
 * It provides common properties and methods for handling sensor data.
 */
public abstract class BaseVector extends SensorData {

    public double x;

    public double y;

    public double z;

    @Ignore
    public String sensorName;

    public BaseVector() {
    }

    /**
     * Constructs a BaseVector object with the specified time, x, y, and z values.
     *
     * @param time the timestamp for the sensor data record.
     * @param x the x-component of the sensor data.
     * @param y the y-component of the sensor data.
     * @param z the z-component of the sensor data.
     */
    public BaseVector(long time, double x, double y, double z) {
        this.time = time * 1_000_000L;
        this.x = x;
        this.y = y;
        this.z = z;
    }


    /**
     * Constructs a BaseVector object from a JSON object.
     * The JSON object should contain time and a nested object with x, y, and z values.
     *
     * @param jo the JSON object containing the vector data.
     * @throws RuntimeException if there is an error parsing the JSON object.
     */
    public BaseVector(JSONObject jo) {
        super(jo);
        try {
            // Extract time from the JSONObject
            this.time = jo.getLong("time") * 1_000_000L;

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

    /**
     * Converts json object into a string
     * @return a string representation of a BaseVector object
     */
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

    /**
     * Abstract method to be implemented by subclasses to provide the specific sensor name.
     * @return The sensor name for the specific type of sensor.
     */
    protected abstract String getSensorName();

    /**
     * Converts the current sensor data object into a {@link JSONObject} representation.
     * The resulting JSON object contains the sensor's name, its x, y, z values, and the timestamp.
     * @return A {@link JSONObject} representation of the sensor data, with sensor name, values, and time.
     */
    @Override
    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        try {
            json.put("name", getSensorName());

            JSONObject values = new JSONObject();
            values.put("x", this.x);
            values.put("y", this.y);
            values.put("z", this.z);

            json.put("values", values);
            json.put("time", this.time);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return json;
    }

}
