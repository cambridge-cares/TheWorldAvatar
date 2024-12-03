package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a Light sensor data entity in the database.
 * This class extends {@link SensorData} and stores light-specific attributes.
 * The data is stored in the "light" table in the database.
 */
@Entity(tableName = "light")
public class LightData extends SensorData {

    /**
     * The SI unit of illuminance, equal to one lumen per square metre.
     */
    public double lux;

    /**
     * Default constructor
     */
    public LightData() {}

    /**
     * Constructs a light object which should have values for lux and tiem.
     * @param jo the JSON object which has light data
     */
    public LightData(JSONObject jo) {
        // get time from JSONObject from abstract class
        super(jo);
        try {
            // Extract the nested values JSONObject
            JSONObject values = jo.getJSONObject("values");

            // Extract lux values from the nested JSONObject
            this.lux = values.getDouble("lux");
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
            json.put("name", "light");


            JSONObject values = new JSONObject();
            values.put("lux", this.lux);

            json.put("values", values);
            json.put("time", this.time);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return json;
    }

}
