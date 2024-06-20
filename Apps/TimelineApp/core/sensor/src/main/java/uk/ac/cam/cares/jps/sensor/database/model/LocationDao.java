package uk.ac.cam.cares.jps.sensor.database.model;

import androidx.room.Dao;
import androidx.room.Insert;

@Dao
public interface LocationDao {
    @Insert
    void insertAll(LocationData... locationData);
}
