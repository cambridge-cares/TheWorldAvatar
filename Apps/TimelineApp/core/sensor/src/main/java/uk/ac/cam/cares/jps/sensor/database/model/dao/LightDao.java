package uk.ac.cam.cares.jps.sensor.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;

import uk.ac.cam.cares.jps.sensor.database.model.entity.LightData;

@Dao
public interface LightDao {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(LightData... lightData);
}
