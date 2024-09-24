package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents an Gyroscope sensor data entity in the database.
 * This class extends {@link BaseVector} and inherits its properties.
 * The data is stored in the "gyro" table in the database.
 * Measures a device's rate of rotation in rad/s around each of the three physical axes (x, y, and z).
 */
@Entity(tableName = "gyro")
public class GyroData extends BaseVector{

    /**
     * Constructs a Gyro object from a JSON object.
     * The JSON object should contain time, x, y, and z values.
     *
     * @param jo the JSON object containing the acceleration data.
     */
    public GyroData(JSONObject jo) {
        super(jo);
    }


    /**
     * Default constructor
     */
    public GyroData() {
        super();
    }

    @Override
    protected String getSensorName() {
        return "gyroscope";
    }


}
