package uk.ac.cam.cares.jps.sensor.di;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.toolbox.Volley;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.sensor.SensorManager;
import uk.ac.cam.cares.jps.sensor.SensorNetworkSource;
import uk.ac.cam.cares.jps.sensor.SensorRepository;

@Module
@InstallIn(SingletonComponent.class)
public class SensorModule {

    @Provides
    @Singleton
    public SensorNetworkSource provideSensorNetworkSource(@ApplicationContext Context applicationContext, RequestQueue requestQueue, SensorManager sensorManager) {
        return new SensorNetworkSource(applicationContext, requestQueue, sensorManager);
    }

    @Provides
    @Singleton
    public SensorRepository provideSensorRepository(SensorNetworkSource networkSource, LoginRepository loginRepository) {
        return new SensorRepository(networkSource, loginRepository);
    }

    @Provides
    @Singleton
    public SensorManager provideSensorManager(@ApplicationContext Context applicationContext) {
        return new SensorManager(applicationContext);
    }
}
