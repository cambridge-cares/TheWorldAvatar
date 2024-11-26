package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents an Gravity sensor data entity in the database.
 * This class extends {@link BaseVector} and inherits its properties.
 * The data is stored in the "gravity" table in the database.
 * Measures the force of gravity in m/s2 that is applied to a device on all three physical axes
 * (x, y, z).
 */
@Entity(tableName = "gravity")
public class Gravity extends BaseVector{

    /**
     * Default constructor
     */
    public Gravity() {
        super();
    }

    /**
     * Constructs a Gravity object from a JSON object.
     * The JSON object should contain time, x, y, and z values.
     *
     * @param jo the JSON object containing the acceleration data.
     */
    public Gravity(JSONObject jo) {
        super(jo);
    }

    @Override
    protected String getSensorName() {
        return "gravity";
    }


}
