package uk.ac.cam.cares.jps.sensor.database.model;

import androidx.room.Database;
import androidx.room.RoomDatabase;

@Database(entities = {LocationData.class}, version = 1)
public abstract class AppDatabase extends RoomDatabase {
    public abstract LocationDao locationDao();
}
