package uk.ac.cam.cares.jps.sensor.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;

import uk.ac.cam.cares.jps.sensor.database.model.entity.Pressure;

@Dao
public interface PressureDao {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(Pressure... pressures);
}
