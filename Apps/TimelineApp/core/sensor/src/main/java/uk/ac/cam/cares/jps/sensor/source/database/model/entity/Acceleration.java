package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;

import org.json.JSONException;
import org.json.JSONObject;
/**
 * Represents an Acceleration sensor data entity in the database.
 * This class extends {@link BaseVector} and inherits its properties.
 * The data is stored in the "acceleration" table in the database.
 * Measures the acceleration force in m/s2 that is applied to a device on all three physical axes
 * (x, y, and z), including the force of gravity.
 */
@Entity(tableName = "acceleration")
public class Acceleration extends BaseVector{

    /**
     * Default constructor
     */
    public Acceleration() {}

    /**
     * Constructs an Acceleration object from a JSON object.
     * The JSON object should contain time, x, y, and z values.
     *
     * @param jo the JSON object containing the acceleration data.
     */
    public Acceleration(JSONObject jo) {
        super(jo);
    }

    @Override
    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        try {
            json.put("name", "accelerometer");

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
