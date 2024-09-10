package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Acceleration;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.LocationData;

/**
 * Data Access Object (DAO) for the Location sensor data.
 * Provides methods for accessing and modifying the 'acceleration' table in the database.
 */
@Dao
public interface LocationDao extends SensorDao<LocationData>  {

    /**
     * Retrieves all Acceleration records from the database.
     *
     * @return an array of {@link LocationData} objects representing all records in the 'acceleration' table.
     */
    @Query("SELECT * FROM location")
    LocationData[] getAll();

    /**
     * Deletes Location records from the database where the timestamp is less than the specified time.
     *
     * @param time the cutoff time; records with a timestamp older than this value will be deleted.
     */
    @Query("DELETE FROM location WHERE time < :time")
    void delete(long time);

}
