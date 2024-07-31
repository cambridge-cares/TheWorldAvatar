package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;

import org.json.JSONObject;

@Entity(tableName = "magnet")
public class MagnetFieldStrength extends BaseVector {
    public MagnetFieldStrength(JSONObject jo) {
        super(jo);
    }
}
