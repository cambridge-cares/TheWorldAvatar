package uk.ac.cam.cares.jps.sensor.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;

import uk.ac.cam.cares.jps.sensor.database.model.entity.RelativeHumidity;

@Dao
public interface RelativeHumidityDao {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(RelativeHumidity... humidities);
}
