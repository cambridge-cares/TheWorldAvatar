package uk.ac.cam.cares.jps.data.di;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.AssetInfoRepository;
import uk.ac.cam.cares.jps.data.MailRepository;
import uk.ac.cam.cares.jps.data.OtherInfoRepository;
import uk.ac.cam.cares.jps.datastore.OtherInfoLocalSource;
import uk.ac.cam.cares.jps.network.assetinfo.AssetNetworkSource;
import uk.ac.cam.cares.jps.network.mail.MailNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.OtherInfoNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class RepositoryModule {
    @Provides
    public AssetInfoRepository provideAssetInfoRepository(AssetNetworkSource networkSource) {
        return new AssetInfoRepository(networkSource);
    }

    @Provides
    public MailRepository provideMailRepository(MailNetworkSource networkSource) {
        return new MailRepository(networkSource);
    }

    @Provides
    public OtherInfoRepository provideOtherInfoRepository(OtherInfoLocalSource otherInfoLocalSource, OtherInfoNetworkSource otherInfoNetworkSource) {
        return new OtherInfoRepository(otherInfoLocalSource, otherInfoNetworkSource);
    }
}
