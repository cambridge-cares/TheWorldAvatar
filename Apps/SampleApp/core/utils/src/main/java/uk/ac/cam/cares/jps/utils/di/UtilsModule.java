package uk.ac.cam.cares.jps.utils.di;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.toolbox.Volley;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;


@Module
@InstallIn(SingletonComponent.class)
public class UtilsModule {

    /**
     * Provides a singleton instance of RequestQueue for making network requests.
     *
     * @param applicationContext The application context.
     * @return A singleton instance of RequestQueue.
     */
    @Provides
    @Singleton
    public RequestQueue provideRequestQueue(@ApplicationContext Context applicationContext) {
        return Volley.newRequestQueue(applicationContext);
    }
}
