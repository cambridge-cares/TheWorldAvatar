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

    /**
     * Provides a singleton instance of TodoRepository.
     *
     * @param todoNetworkSource The source for todo-related data, obtained through network calls.
     * @param userNetworkSource The source for user-related data, obtained through network calls.
     * @return A singleton instance of TodoRepository.
     */
    @Provides
    @Singleton
    public TodoRepository provideTodoRepository(TodoNetworkSource todoNetworkSource, UserNetworkSource userNetworkSource) {
        return new TodoRepository(todoNetworkSource, userNetworkSource);
    }
}
