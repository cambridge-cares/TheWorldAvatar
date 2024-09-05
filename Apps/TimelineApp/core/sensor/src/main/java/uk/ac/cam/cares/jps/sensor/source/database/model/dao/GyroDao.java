package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;

import uk.ac.cam.cares.jps.sensor.source.database.model.entity.GyroData;

@Dao
public interface GyroDao {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(GyroData... gyroData);
}
