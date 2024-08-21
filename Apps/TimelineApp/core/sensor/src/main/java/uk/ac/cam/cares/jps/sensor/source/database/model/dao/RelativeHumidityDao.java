package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Acceleration;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.RelativeHumidity;

/**
 * Data Access Object (DAO) for the Humidity sensor data.
 * Provides methods for accessing and modifying the 'acceleration' table in the database.
 */
@Dao
public interface RelativeHumidityDao extends SensorDao<RelativeHumidity> {

    /**
     * Retrieves all Acceleration records from the database.
     *
     * @return an array of {@link RelativeHumidity} objects representing all records in the 'acceleration' table.
     */
    @Query("SELECT * FROM humidity")
    RelativeHumidity[] getAll();

    /**
     * Deletes Humidity records from the database where the timestamp is less than the specified time.
     *
     * @param time the cutoff time; records with a timestamp older than this value will be deleted.
     */
    @Query("DELETE FROM humidity WHERE time < :time")
    void delete(long time);
}
