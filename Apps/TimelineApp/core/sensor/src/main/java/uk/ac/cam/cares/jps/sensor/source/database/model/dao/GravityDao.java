package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Acceleration;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Gravity;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.RelativeHumidity;

/**
 * Data Access Object (DAO) for the Gravity sensor data.
 * Provides methods for accessing and modifying the 'acceleration' table in the database.
 */
@Dao
public interface GravityDao extends SensorDao<Gravity> {

    /**
     * Retrieves all gravity records from the local database.
     *
     * @return an array of {@link Gravity} objects representing all records in the 'gravity' table.
     */
    @Query("SELECT * FROM gravity")
    Gravity[] getAll();

    /**
     * Deletes Gravity records from the database where the timestamp is less than the specified time.
     *
     * @param time the cutoff time; records with a timestamp older than this value will be deleted.
     */
    @Query("DELETE FROM gravity WHERE time < :time")
    void delete(long time);
}
