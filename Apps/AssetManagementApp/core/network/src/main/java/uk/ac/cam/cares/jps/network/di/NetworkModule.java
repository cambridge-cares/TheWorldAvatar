package uk.ac.cam.cares.jps.network.di;

import android.app.Application;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.network.AssetNetworkSource;
import uk.ac.cam.cares.jps.network.Connection;

@Module
@InstallIn(SingletonComponent.class)
public class NetworkModule {
    @Provides
    public Connection provideConnection(@ApplicationContext Application application) {
        return Connection.getInstance(application.getApplicationContext());
    }

    @Provides
    public AssetNetworkSource provideAssetNetworkSource() {
        return new AssetNetworkSource();
    }
}
