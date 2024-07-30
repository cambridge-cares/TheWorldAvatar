package uk.ac.cam.cares.jps.sensor.database.model;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;

@Dao
public interface LocationDao {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(LocationData... locationData);
}
