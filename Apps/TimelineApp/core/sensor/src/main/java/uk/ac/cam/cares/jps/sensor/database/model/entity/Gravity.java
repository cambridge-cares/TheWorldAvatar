package uk.ac.cam.cares.jps.sensor.database.model.entity;

import androidx.room.Entity;

import org.json.JSONObject;

@Entity(tableName = "gravity")
public class Gravity extends BaseVector{
    public Gravity(JSONObject jo) {
        super(jo);
    }
}
