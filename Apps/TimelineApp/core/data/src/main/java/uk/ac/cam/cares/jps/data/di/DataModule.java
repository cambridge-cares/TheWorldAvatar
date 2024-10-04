package uk.ac.cam.cares.jps.data.di;

import android.content.Context;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.DatesWithTrajectoryRepository;
import uk.ac.cam.cares.jps.data.TrajectoryRepository;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.network.DatesWithTrajectoryNetworkSource;
import uk.ac.cam.cares.jps.network.TrajectoryNetworkSource;

/**
 * Dependency injection specification for data module
 */
@Module
@InstallIn(SingletonComponent.class)
public class DataModule {
    @Provides
    @Singleton
    public TrajectoryRepository provideTrajectoryRepository(TrajectoryNetworkSource trajectoryNetworkSource,
                                                            LoginRepository loginRepository,
                                                            @ApplicationContext Context context) {
        return new TrajectoryRepository(trajectoryNetworkSource, loginRepository, context);
    }

    @Provides
    @Singleton
    public DatesWithTrajectoryRepository provideDatesWithTrajectoryRepository(DatesWithTrajectoryNetworkSource datesWithTrajectoryNetworkSource,
                                                                              LoginRepository loginRepository,
                                                                              @ApplicationContext Context context) {
        return new DatesWithTrajectoryRepository(datesWithTrajectoryNetworkSource, loginRepository, context);
    }
}
