package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import uk.ac.cam.cares.jps.sensor.source.database.model.entity.LocationData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.SensorData;

/**
 * Data Access Object (DAO) interface for generic sensor data operations.
 * Provides methods for inserting sensor data into the database.
 *
 * @param <T> a type that extends {@link SensorData}, representing the sensor data entity.
 */
@Dao
public interface SensorDao<T extends SensorData> {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(T... sensorData);


}