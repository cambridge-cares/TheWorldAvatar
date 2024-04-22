package uk.ac.cam.cares.jps.data.di;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.TodoRepository;
import uk.ac.cam.cares.jps.network.TodoNetworkSource;
import uk.ac.cam.cares.jps.network.UserNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class DataModule {
    @Provides
    @Singleton
    public TodoRepository provideTodoRepository(TodoNetworkSource trajectoryNetworkSource, UserNetworkSource userNetworkSource) {
        return new TodoRepository(trajectoryNetworkSource, userNetworkSource);
    }
}
