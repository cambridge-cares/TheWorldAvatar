package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a Pressure sensor data entity in the database.
 * This class extends {@link SensorData} and stores pressure-specific attributes.
 * The data is stored in the "pressure" table in the database.
 */
@Entity(tableName = "pressure")
public class Pressure extends SensorData {

    /**
     * represents the ambient air pressure
     */
    public double pressure;

    /**
     * Default constructor
     */
    public Pressure() {}

    /**
     * Constructs a pressure object using a JSONObject.
     * @param jo the JSONObject which stores data related to pressure.
     */
    public Pressure(JSONObject jo) {

        // get time from JSONObject from abstract class
        super(jo);

        try {

            // Extract the nested values JSONObject
            JSONObject values = jo.getJSONObject("values");

            // Extract lux values from the nested JSONObject
            this.pressure = values.getDouble("pressure");
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
            json.put("name", "barometer");

            JSONObject values = new JSONObject();
            values.put("pressure", this.pressure);


            json.put("values", values);
            json.put("time", this.time);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return json;
    }

}
