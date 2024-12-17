package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Query;

import java.util.List;

import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Gravity;

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

    /**
     * Retrieves all unsent gravity data from the database that has not been marked as uploaded.
     * The query limits the number of rows returned and allows pagination using the offset parameter.
     *
     * @param limit  The maximum number of records to return.
     * @param offset The offset from which to start retrieving records (useful for pagination).
     * @return An array of {@link Gravity} objects that have not been uploaded.
     */
    @Query("SELECT * FROM gravity WHERE uploaded = 0 LIMIT :limit OFFSET :offset")
    Gravity[] getAllUnUploadedData(int limit, int offset);

    /**
     * Marks the gravity records as uploaded in the database by updating the 'uploaded' field.
     * This method takes a list of timestamps (represented as long values) and sets the 'uploaded' field to 1
     * for the corresponding records in the database.
     *
     * @param times A list of timestamps for the gravity data that has been successfully uploaded.
     *              Each entry in the list corresponds to the 'time' field in the database.
     */
    @Query("UPDATE gravity SET uploaded = 1 WHERE time IN (:times)")
    void markAsUploaded(List<Long> times);
}
