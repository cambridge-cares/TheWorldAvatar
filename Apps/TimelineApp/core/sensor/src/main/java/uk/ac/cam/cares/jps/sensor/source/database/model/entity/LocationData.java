package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents location sensor data in the database.
 * This class extends {@link SensorData} and stores location-specific attributes.
 * The data is stored in the "location" table in the database.
 */
@Entity(tableName = "location")
public class LocationData extends SensorData {
    @PrimaryKey
    public long time;

    public double latitude;

    public double longitude;

    public double altitude;

    public float speed;

    public float bearing;

    public Float horizontalAccuracy;

    public Float bearingAccuracy;

    public Float speedAccuracy;

    public Float verticalAccuracy;

    /**
     * Constructs a LocationData object with the specified attributes.
     *
     * @param time the timestamp for the location data.
     * @param latitude the latitude of the location.
     * @param longitude the longitude of the location.
     * @param altitude the altitude of the location.
     * @param speed the speed at the location.
     * @param bearing the bearing at the location.
     * @param horizontalAccuracy the horizontal accuracy of the location data.
     * @param bearingAccuracy the bearing accuracy of the location data.
     * @param speedAccuracy the speed accuracy of the location data.
     * @param verticalAccuracy the vertical accuracy of the location data.
     */
    public LocationData(long time, double latitude, double longitude, double altitude, float speed, float bearing, Float horizontalAccuracy, Float bearingAccuracy, Float speedAccuracy, Float verticalAccuracy) {
        this.time = time;
        this.latitude = latitude;
        this.longitude = longitude;
        this.altitude = altitude;
        this.speed = speed;
        this.bearing = bearing;
        this.horizontalAccuracy = horizontalAccuracy;
        this.bearingAccuracy = bearingAccuracy;
        this.speedAccuracy = speedAccuracy;
        this.verticalAccuracy = verticalAccuracy;
    }

    /**
     * Constructs a LocationData object from a JSON object.
     * The JSON object should contain time and nested values for latitude, longitude, altitude, speed, etc.
     *
     * @param jo the JSON object containing the location data.
     * @throws RuntimeException if there is an error parsing the JSON object.
     */
    public LocationData(JSONObject jo) {
        try {
            this.time = jo.getLong("time");

            JSONObject values = jo.getJSONObject("values");

            this.latitude = values.getDouble("latitude");
            this.longitude = values.getDouble("longitude");
            this.altitude = values.getDouble("altitude");
            this.speed = (float) values.getDouble("speed");
            this.bearing = (float) values.getDouble("bearing");
            this.horizontalAccuracy = values.has("horizontalAccuracy") && !values.isNull("horizontalAccuracy") ?
                    (float) values.getDouble("horizontalAccuracy") : -1;
            this.bearingAccuracy = values.has("bearingAccuracy") && !values.isNull("bearingAccuracy") ?
                    (float) values.getDouble("bearingAccuracy") : -1;
            this.speedAccuracy = values.has("speedAccuracy") && !values.isNull("speedAccuracy") ?
                    (float) values.getDouble("speedAccuracy") : -1;
            this.verticalAccuracy = values.has("verticalAccuracy") && !values.isNull("verticalAccuracy") ?
                    (float) values.getDouble("verticalAccuracy") : -1;

        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String toJSONString() {
        return "";
    }
}
