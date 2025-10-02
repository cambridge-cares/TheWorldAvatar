package uk.ac.cam.cares.jps.sensor.source.database.model;

import androidx.room.Database;
import androidx.room.RoomDatabase;

import uk.ac.cam.cares.jps.sensor.source.database.model.dao.AccelerationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.ActivityDataDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.GravityDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.GyroDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.LightDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.LocationDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.MagnetFieldStrengthDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.PressureDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.RelativeHumidityDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.SoundLevelDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.dao.UnsentDataDao;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Acceleration;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.ActivityData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Gravity;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.GyroData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.LightData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.LocationData;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.MagnetFieldStrength;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.Pressure;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.RelativeHumidity;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.SoundLevel;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.UnsentData;

@Database(entities = {LocationData.class, Acceleration.class, LightData.class, MagnetFieldStrength.class, Gravity.class, GyroData.class, Pressure.class, RelativeHumidity.class, SoundLevel.class, UnsentData.class, ActivityData.class}, version = 4)
public abstract class AppDatabase extends RoomDatabase {
    public abstract LocationDao locationDao();
    public abstract AccelerationDao accelerationDao();
    public abstract GravityDao gravityDao();
    public abstract GyroDao gyroDao();
    public abstract LightDao lightDao();
    public abstract MagnetFieldStrengthDao magnetFieldStrengthDao();
    public abstract PressureDao pressureDao();
    public abstract RelativeHumidityDao relativeHumidityDao();
    public abstract SoundLevelDao soundLevelDao();
    public abstract UnsentDataDao unsentDataDao();
    public abstract ActivityDataDao activityDataDao();

}
