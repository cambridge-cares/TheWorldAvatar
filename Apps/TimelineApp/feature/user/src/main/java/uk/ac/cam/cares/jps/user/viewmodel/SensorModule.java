package uk.ac.cam.cares.jps.user.viewmodel;

import android.content.Context;
import android.content.SharedPreferences;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import dagger.hilt.android.qualifiers.ApplicationContext;

@Module
@InstallIn(SingletonComponent.class)
public class SensorModule {

    @Provides
    public SharedPreferences provideSharedPreferences(@ApplicationContext Context context) {
        return context.getSharedPreferences("sensor_prefs", Context.MODE_PRIVATE);
    }
}
