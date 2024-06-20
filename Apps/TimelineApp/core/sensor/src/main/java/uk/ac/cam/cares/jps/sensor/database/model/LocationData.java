package uk.ac.cam.cares.jps.sensor.database.model;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

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
}
