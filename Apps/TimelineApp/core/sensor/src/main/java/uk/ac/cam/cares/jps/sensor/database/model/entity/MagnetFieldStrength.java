package uk.ac.cam.cares.jps.sensor.database.model.entity;

import androidx.room.Entity;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.sensor.database.model.entity.BaseVector;

@Entity(tableName = "magnet")
public class MagnetFieldStrength extends BaseVector {
    public MagnetFieldStrength(JSONObject jo) {
        super(jo);
    }
}
