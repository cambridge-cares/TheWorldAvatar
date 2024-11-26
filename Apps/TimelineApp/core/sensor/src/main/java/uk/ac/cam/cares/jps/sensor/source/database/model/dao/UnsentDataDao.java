package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import org.json.JSONArray;

import java.util.List;

import uk.ac.cam.cares.jps.sensor.source.database.model.entity.UnsentData;

/**
 * Data Access Object (DAO) interface for managing unsent sensor data in the database.
 * Provides methods to insert, retrieve, and delete unsent data records.
 */
@Dao
public interface UnsentDataDao {
    /**
     * Inserts a new unsent data record into the database.
     * If a record with the same primary key already exists, it will be replaced.
     *
     * @param unsentData the unsent data record to be inserted.
     */
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(UnsentData unsentData);

    /**
     * Retrieves all unsent data records from the database.
     *
     * @return a list of {@link UnsentData} objects representing all records in the 'unsent_data' table.
     */
    @Query("SELECT * FROM unsent_data")
    List<UnsentData> getAllUnsentData();

    /**
     * Deletes all unsent data records from the database that matches the given id
     *
     * @param id of a unsent data record.
     */
    @Query("DELETE FROM unsent_data WHERE id = :id")
    void deleteById(int id);

    /**
     * Deletes all unsent data records from the database.
     */
    @Query("DELETE FROM unsent_data")
    void deleteAll();

    @Query("SELECT * FROM unsent_data WHERE dataHash = :dataHash LIMIT 1")
    UnsentData getUnsentDataByHash(String dataHash);

}