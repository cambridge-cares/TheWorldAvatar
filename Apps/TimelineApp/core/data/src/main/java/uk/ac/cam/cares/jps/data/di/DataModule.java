package uk.ac.cam.cares.jps.data.di;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.TrajectoryRepository;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.network.TrajectoryNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class DataModule {
    @Provides
    @Singleton
    public TrajectoryRepository provideTrajectoryRepository(TrajectoryNetworkSource trajectoryNetworkSource,
                                                            LoginRepository loginRepository) {
        return new TrajectoryRepository(trajectoryNetworkSource, loginRepository);
    }
}
