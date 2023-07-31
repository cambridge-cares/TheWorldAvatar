package uk.ac.cam.cares.jps.data.di;

import android.app.Application;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.AssetInfoRepository;
import uk.ac.cam.cares.jps.network.AssetNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class RepositoryModule {
    @Provides
    public AssetInfoRepository provideAssetInfoRepository(AssetNetworkSource networkSource) {
        return new AssetInfoRepository(networkSource);
    }
}
