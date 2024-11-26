package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents an MagnetFieldStrength sensor data entity in the database.
 * This class extends {@link BaseVector} and inherits its properties.
 * The data is stored in the "magnet" table in the database.
 * Measures the ambient geomagnetic field for all three physical axes (x, y, z) in μT
 */
@Entity(tableName = "magnet")
public class MagnetFieldStrength extends BaseVector {
    /**
     * Constructs a Magnet object from a JSON object.
     * The JSON object should contain time, x, y, and z values.
     *
     * @param jo the JSON object containing the acceleration data.
     */
    public MagnetFieldStrength(JSONObject jo) {
        super(jo);
    }

    /**
     * Default constructor
     */
    public MagnetFieldStrength() {
        super();
    }

    @Override
    protected String getSensorName() {
        return "magnetometer";
    }

    }
