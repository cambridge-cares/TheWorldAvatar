package uk.ac.cam.cares.jps.network.di;

import android.content.Context;

import com.android.volley.RequestQueue;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.model.Todo;
import uk.ac.cam.cares.jps.model.User;
import uk.ac.cam.cares.jps.network.NetworkSource;
import uk.ac.cam.cares.jps.network.TodoNetworkSource;
import uk.ac.cam.cares.jps.network.UserNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class NetworkModule {

    /**
     * Provides a singleton instance of TodoNetworkSource.
     *
     * @param requestQueue The RequestQueue instance shared across the app.
     * @param context      The application context.
     * @return A singleton instance of TodoNetworkSource.
     */
    @Provides
    @Singleton
    public NetworkSource<Todo> provideTodoNetworkSource(RequestQueue requestQueue, @ApplicationContext Context context) {
        return new TodoNetworkSource(requestQueue, context);
    }

    /**
     * Provides a singleton instance of UserNetworkSource.
     *
     * @param requestQueue The RequestQueue instance shared across the app.
     * @param context      The application context.
     * @return A singleton instance of UserNetworkSource.
     */
    @Provides
    @Singleton
    public NetworkSource<User> provideUserNetworkSource(RequestQueue requestQueue, @ApplicationContext Context context) {
        return new UserNetworkSource(requestQueue, context);
    }
}
