package uk.ac.cam.cares.jps.data.di;

import android.app.Application;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.AssetInfoRepository;

@Module
@InstallIn(SingletonComponent.class)
public class RepositoryModule {
    @Provides
    public AssetInfoRepository provideAssetInfoRepository() {
        return new AssetInfoRepository();
    }
}
