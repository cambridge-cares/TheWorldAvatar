package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;

import org.json.JSONObject;

@Entity(tableName = "gyro")
public class GyroData extends BaseVector{
    public GyroData(JSONObject jo) {
        super(jo);
    }
}
