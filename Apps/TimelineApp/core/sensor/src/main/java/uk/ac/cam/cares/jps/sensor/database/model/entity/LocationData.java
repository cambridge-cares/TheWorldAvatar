package uk.ac.cam.cares.jps.sensor.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

@Entity(tableName = "location")
public class LocationData {
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
}
