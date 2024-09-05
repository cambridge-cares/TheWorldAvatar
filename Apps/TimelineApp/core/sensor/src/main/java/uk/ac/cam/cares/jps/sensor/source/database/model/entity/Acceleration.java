package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;

import org.json.JSONObject;

@Entity(tableName = "acceleration")
public class Acceleration extends BaseVector{
    public Acceleration() {}
    public Acceleration(JSONObject jo) {
        super(jo);
    }
}
