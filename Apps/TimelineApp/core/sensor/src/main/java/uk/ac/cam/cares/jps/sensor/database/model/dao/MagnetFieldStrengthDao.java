package uk.ac.cam.cares.jps.sensor.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;

import uk.ac.cam.cares.jps.sensor.database.model.entity.MagnetFieldStrength;

@Dao
public interface MagnetFieldStrengthDao {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(MagnetFieldStrength... magnetFieldStrengths);
}
