package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Acceleration;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.RelativeHumidity;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.SoundLevel;

/**
 * Data Access Object (DAO) for the Sound sensor data.
 * Provides methods for accessing and modifying the 'acceleration' table in the database.
 */
@Dao
public interface SoundLevelDao extends SensorDao<SoundLevel> {

    /**
     * Retrieves all Acceleration records from the database.
     *
     * @return an array of {@link SoundLevel} objects representing all records in the 'acceleration' table.
     */
    @Query("SELECT * FROM sound")
    SoundLevel[] getAll();

    /**
     * Deletes Sound records from the database where the timestamp is less than the specified time.
     *
     * @param time the cutoff time; records with a timestamp older than this value will be deleted.
     */
    @Query("DELETE FROM sound WHERE time < :time")
    void delete(long time);
}
