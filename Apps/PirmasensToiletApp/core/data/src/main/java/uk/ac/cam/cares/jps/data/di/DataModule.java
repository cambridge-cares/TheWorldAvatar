package uk.ac.cam.cares.jps.data.di;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.RouteRepository;
import uk.ac.cam.cares.jps.data.ToiletRepository;
import uk.ac.cam.cares.jps.network.poi.ToiletNetworkSource;
import uk.ac.cam.cares.jps.network.route.RouteNetworkSource;
import uk.ac.cam.cares.jps.network.route.VertexNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class DataModule {

    @Provides
    @Singleton
    public RouteRepository provideRouteRepository(VertexNetworkSource vertexNetworkSource, RouteNetworkSource routeNetworkSource) {
        return new RouteRepository(vertexNetworkSource, routeNetworkSource);
    }

    @Provides
    @Singleton
    public ToiletRepository provideToiletRepository(ToiletNetworkSource toiletNetworkSource) {
        return new ToiletRepository(toiletNetworkSource);
    }
}
