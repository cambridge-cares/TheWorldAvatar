package uk.ac.cam.cares.jps.sensor.di;

import android.content.Context;

import com.android.volley.RequestQueue;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.sensor.source.state.SensorCollectionStateManager;
import uk.ac.cam.cares.jps.sensor.data.SensorCollectionStateManagerRepository;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorManager;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;
import uk.ac.cam.cares.jps.sensor.data.SensorRepository;
import uk.ac.cam.cares.jps.sensor.source.network.UserPhoneNetworkSource;
import uk.ac.cam.cares.jps.sensor.data.UserPhoneRepository;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;

@Module
@InstallIn(SingletonComponent.class)
public class SensorModule {

    @Provides
    @Singleton
    public SensorNetworkSource provideSensorNetworkSource(@ApplicationContext Context applicationContext,
                                                          RequestQueue requestQueue) {
        return new SensorNetworkSource(applicationContext, requestQueue);
    }

    @Provides
    @Singleton
    public SensorLocalSource provideSensorLocalSource(@ApplicationContext Context applicationContext) {
        return new SensorLocalSource(applicationContext);
    }

    @Provides
    @Singleton
    public SensorRepository provideSensorRepository(@ApplicationContext Context applicationContext,
                                                    UserPhoneNetworkSource userPhoneNetworkSource,
                                                    SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository) {
        return new SensorRepository(applicationContext, userPhoneNetworkSource, sensorCollectionStateManagerRepository);
    }

    @Provides
    @Singleton
    public SensorManager provideSensorManager(@ApplicationContext Context applicationContext) {
        return new SensorManager(applicationContext);
    }

    @Provides
    @Singleton
    public SensorCollectionStateManager provideSensorCollectionStateManager(@ApplicationContext Context applicationContext) {
        return new SensorCollectionStateManager(applicationContext);
    }

    @Provides
    @Singleton
    public UserPhoneNetworkSource provideUserPhoneNetworkSource(@ApplicationContext Context applicationContext, RequestQueue requestQueue) {
        return new UserPhoneNetworkSource(applicationContext, requestQueue);
    }

    @Provides
    @Singleton
    public UserPhoneRepository provideUserPhoneRepository(
            UserPhoneNetworkSource userPhoneNetworkSource,
            SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository) {
        return new UserPhoneRepository(userPhoneNetworkSource, sensorCollectionStateManagerRepository);
    }

    @Provides
    @Singleton
    public SensorCollectionStateManagerRepository provideSensorCollectionStateManagerRepository(
            SensorCollectionStateManager sensorCollectionStateManager,
            LoginRepository loginRepository) {
        return new SensorCollectionStateManagerRepository(sensorCollectionStateManager, loginRepository);
    }
}
