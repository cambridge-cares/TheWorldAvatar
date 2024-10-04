package uk.ac.cam.cares.jps.sensor.source.database.model;

import androidx.room.Database;
import androidx.room.RoomDatabase;

import uk.ac.cam.cares.jps.sensor.source.database.model.dao.AccelerationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.GravityDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.GyroDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.LightDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.LocationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.MagnetFieldStrengthDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.PressureDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.RelativeHumidityDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.SoundLevelDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.UnsentDataDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.*;

/**
 * Testing database class for in memory testing.
 */
@Database(entities = {
        LocationData.class, Acceleration.class, Gravity.class,
        GyroData.class, LightData.class, MagnetFieldStrength.class,
        Pressure.class, SoundLevel.class, RelativeHumidity.class, UnsentData.class
}, version = 1, exportSchema = false)
public abstract class TestDatabase extends RoomDatabase {
    public abstract LocationDao locationDao();
    public abstract AccelerationDao accelerationDao();
    public abstract GravityDao gravityDao();
    public abstract GyroDao gyroDao();
    public abstract LightDao lightDao();
    public abstract MagnetFieldStrengthDao magnetFieldStrengthDao();
    public abstract PressureDao pressureDao();
    public abstract SoundLevelDao soundLevelDao();
    public abstract RelativeHumidityDao relativeHumidityDao();
    public abstract UnsentDataDao unsentDataDao();
}
