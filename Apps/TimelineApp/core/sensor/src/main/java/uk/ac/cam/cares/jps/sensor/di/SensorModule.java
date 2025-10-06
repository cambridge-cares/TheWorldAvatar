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
import uk.ac.cam.cares.jps.sensor.source.handler.SensorHandlerManager;
import uk.ac.cam.cares.jps.sensor.source.network.NetworkChangeReceiver;
import uk.ac.cam.cares.jps.sensor.source.state.SensorCollectionStateManager;
import uk.ac.cam.cares.jps.sensor.data.SensorCollectionStateManagerRepository;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;
import uk.ac.cam.cares.jps.sensor.data.SensorRepository;
import uk.ac.cam.cares.jps.sensor.source.network.UserPhoneNetworkSource;
import uk.ac.cam.cares.jps.sensor.data.UserPhoneRepository;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.ui.RecordingState;

/**
 * Dependency injection specification for Sensor module
 */
@Module
@InstallIn(SingletonComponent.class)
public class SensorModule {

    @Provides
    @Singleton
    public SensorNetworkSource provideSensorNetworkSource(@ApplicationContext Context applicationContext,
                                                          RequestQueue requestQueue,
                                                          SensorLocalSource sensorLocalSource) {
        return new SensorNetworkSource(applicationContext, requestQueue, sensorLocalSource);
    }

    @Provides
    @Singleton
    public SensorLocalSource provideSensorLocalSource(@ApplicationContext Context applicationContext) {
        return new SensorLocalSource(applicationContext);
    }

    @Provides
    @Singleton
    public SensorRepository provideSensorRepository(@ApplicationContext Context applicationContext,
                                                    SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository,
                                                    SensorLocalSource sensorLocalSource) {
        return new SensorRepository(applicationContext, sensorCollectionStateManagerRepository, sensorLocalSource);
    }

    @Provides
    @Singleton
    public SensorHandlerManager provideSensorManager(@ApplicationContext Context applicationContext) {
        return new SensorHandlerManager(applicationContext);
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

    @Provides
    @Singleton
    public NetworkChangeReceiver provideNetworkChangeReceiver(
            SensorLocalSource localSource,
            SensorNetworkSource networkSource) {
        return new NetworkChangeReceiver(localSource, networkSource);
    }

    @Provides
    @Singleton
    public RecordingState provideRecordingState() {
        return new RecordingState();
    }
}
