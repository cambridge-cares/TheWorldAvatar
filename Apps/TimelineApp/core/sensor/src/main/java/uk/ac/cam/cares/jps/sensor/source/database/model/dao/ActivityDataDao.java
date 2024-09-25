package uk.ac.cam.cares.jps.sensor.source.database.model.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.Query;

import java.util.List;

import uk.ac.cam.cares.jps.sensor.source.database.model.entity.ActivityData;

@Dao
public interface ActivityDataDao {

    @Insert
    void insert(ActivityData activityData);

    @Query("SELECT * FROM activity_data ORDER BY timestamp DESC")
    List<ActivityData> getAllActivityData();
}
