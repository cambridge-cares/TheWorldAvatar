package uk.ac.cam.cares.jps.network.di;

import android.content.Context;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.toilet.ToiletBuildingInfoNetworkSource;
import uk.ac.cam.cares.jps.network.toilet.ToiletInfoNetworkSource;
import uk.ac.cam.cares.jps.network.toilet.ToiletNetworkSource;
import uk.ac.cam.cares.jps.network.route.RouteNetworkSource;
import uk.ac.cam.cares.jps.network.route.VertexNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class NetworkModule {
    @Provides
    public Connection provideConnection(@ApplicationContext Context applicationContext) {
        return Connection.getInstance(applicationContext);
    }

    @Provides
    @Singleton
    public VertexNetworkSource provideVertexNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new VertexNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public RouteNetworkSource provideRouteNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new RouteNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public ToiletNetworkSource provideToiletNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new ToiletNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public ToiletInfoNetworkSource provideToiletInfoNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new ToiletInfoNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public ToiletBuildingInfoNetworkSource provideToiletBuildingInfoNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new ToiletBuildingInfoNetworkSource(connection, applicationContext);
    }
}
