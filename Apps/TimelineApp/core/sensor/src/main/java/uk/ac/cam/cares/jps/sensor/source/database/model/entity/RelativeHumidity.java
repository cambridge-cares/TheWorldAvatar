package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a Humidity sensor data entity in the database.
 * This class extends {@link SensorData} and stores humidity-specific attributes.
 * The data is stored in the "humidity" table in the database.
 */
@Entity(tableName = "humidity")
public class RelativeHumidity extends SensorData {
    /**
     * Measures the relative ambient humidity in percent (%).
     */
    public double humidity;

    /**
     * Default constructor
     */
    public RelativeHumidity() {}

    /**
     * Constructs a humidity object using a JSONObject
     * @param jo object which includes data relating to humidity.
     */
    public RelativeHumidity(JSONObject jo) {
        super(jo);

        try {

            // Extract the nested values JSONObject
            JSONObject values = jo.getJSONObject("values");

            // Extract lux values from the nested JSONObject
            this.humidity = values.getDouble("humidity");
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String toJSONString() {
        return "";
    }

    @Override
    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        try {
            json.put("name", "humidity");

            JSONObject values = new JSONObject();
            values.put("humidity", this.humidity);

            json.put("values", values);
            json.put("time", this.time);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return json;
    }

}
