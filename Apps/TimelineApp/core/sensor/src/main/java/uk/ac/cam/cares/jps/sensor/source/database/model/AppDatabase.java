package uk.ac.cam.cares.jps.sensor.source.database.model;

import androidx.room.Database;
import androidx.room.RoomDatabase;

import uk.ac.cam.cares.jps.sensor.source.database.model.dao.AccelerationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.LocationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Acceleration;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.LocationData;

@Database(entities = {LocationData.class, Acceleration.class}, version = 1)
public abstract class AppDatabase extends RoomDatabase {
    public abstract LocationDao locationDao();
    public abstract AccelerationDao accelerationDao();
}
