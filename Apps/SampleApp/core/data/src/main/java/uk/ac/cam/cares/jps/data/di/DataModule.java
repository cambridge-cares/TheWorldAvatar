package uk.ac.cam.cares.jps.data.di;

import android.util.Pair;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.GenericRepository;
import uk.ac.cam.cares.jps.data.TodoRepository;
import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.TodoWithUser;
import uk.ac.cam.cares.jps.model.User;
import uk.ac.cam.cares.jps.network.NetworkSource;

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
    public GenericRepository<TodoWithUser> provideTodoRepository(NetworkSource<Todo> todoNetworkSource, NetworkSource<User> userNetworkSource) {
        return new TodoRepository(todoNetworkSource, userNetworkSource);
    }
}
