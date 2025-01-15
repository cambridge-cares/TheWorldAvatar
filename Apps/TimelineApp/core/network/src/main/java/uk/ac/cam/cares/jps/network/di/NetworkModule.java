package uk.ac.cam.cares.jps.network.di;

import android.content.Context;

import com.android.volley.RequestQueue;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.network.DatesWithTrajectoryNetworkSource;
import uk.ac.cam.cares.jps.network.TrajectoryNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class NetworkModule {
    @Provides
    @Singleton
    public TrajectoryNetworkSource provideTrajectoryNetworkSource(RequestQueue requestQueue, @ApplicationContext Context context) {
        return new TrajectoryNetworkSource(requestQueue, context);
    }

    @Provides
    @Singleton
    public DatesWithTrajectoryNetworkSource provideDatesWithTrajectoryNetworkSource(RequestQueue requestQueue, @ApplicationContext Context context) {
        return new DatesWithTrajectoryNetworkSource(requestQueue, context);
    }
}
