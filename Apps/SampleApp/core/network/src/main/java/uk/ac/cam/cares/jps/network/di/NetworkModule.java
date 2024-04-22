package uk.ac.cam.cares.jps.network.di;

import android.content.Context;

import com.android.volley.RequestQueue;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.network.TodoNetworkSource;
import uk.ac.cam.cares.jps.network.UserNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class NetworkModule {

    // RequestQueue is provided in utils. The whole app will share the single instance of RequestQueue.
    // It is placed in the utils is because some other modules, which are not included in this project, will require the RequestQueue
    @Provides
    @Singleton
    public TodoNetworkSource provideTodoNetworkSource(RequestQueue requestQueue, @ApplicationContext Context context) {
        return new TodoNetworkSource(requestQueue, context);
    }

    @Provides
    @Singleton
    public UserNetworkSource provideUserNetworkSource(RequestQueue requestQueue, @ApplicationContext Context context) {
        return new UserNetworkSource(requestQueue, context);
    }
}
