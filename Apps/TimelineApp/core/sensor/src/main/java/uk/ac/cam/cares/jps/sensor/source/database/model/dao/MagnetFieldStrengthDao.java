package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Query;

import java.util.List;

import uk.ac.cam.cares.jps.sensor.source.database.model.entity.MagnetFieldStrength;

/**
 * Data Access Object (DAO) for the Magnet sensor data.
 * Provides methods for accessing and modifying the 'acceleration' table in the database.
 */
@Dao
public interface MagnetFieldStrengthDao extends SensorDao<MagnetFieldStrength> {

    /**
     * Retrieves all Acceleration records from the database.
     *
     * @return an array of {@link MagnetFieldStrength} objects representing all records in the 'acceleration' table.
     */
    @Query("SELECT * FROM magnet")
    MagnetFieldStrength[] getAll();

    /**
     * Deletes Magnet records from the database where the timestamp is less than the specified time.
     *
     * @param time the cutoff time; records with a timestamp older than this value will be deleted.
     */
    @Query("DELETE FROM magnet WHERE time < :time")
    void delete(long time);

    /**
     * Retrieves all unsent magnet data from the database that has not been marked as uploaded.
     * The query limits the number of rows returned and allows pagination using the offset parameter.
     *
     * @param limit  The maximum number of records to return.
     * @param offset The offset from which to start retrieving records (useful for pagination).
     * @return An array of {@link MagnetFieldStrength} objects that have not been uploaded.
     */
    @Query("SELECT * FROM magnet WHERE uploaded = 0 LIMIT :limit OFFSET :offset")
    MagnetFieldStrength[] getAllUnUploadedData(int limit, int offset);

    /**
     * Marks the magnet records as uploaded in the database by updating the 'uploaded' field.
     * This method takes a list of timestamps (represented as long values) and sets the 'uploaded' field to 1
     * for the corresponding records in the database.
     *
     * @param times A list of timestamps for the magnet data that has been successfully uploaded.
     *              Each entry in the list corresponds to the 'time' field in the database.
     */
    @Query("UPDATE magnet SET uploaded = 1 WHERE time IN (:times)")
    void markAsUploaded(List<Long> times);
}
