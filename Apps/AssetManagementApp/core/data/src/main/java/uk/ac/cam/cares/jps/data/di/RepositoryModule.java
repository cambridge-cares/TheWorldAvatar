package uk.ac.cam.cares.jps.data.di;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.AssetInfoRepository;
import uk.ac.cam.cares.jps.data.MailRepository;
import uk.ac.cam.cares.jps.data.OtherInfoRepository;
import uk.ac.cam.cares.jps.data.SettingRepository;
import uk.ac.cam.cares.jps.datastore.OtherInfoLocalSource;
import uk.ac.cam.cares.jps.datastore.SettingLocalSource;
import uk.ac.cam.cares.jps.network.assetinfo.AssetNetworkSource;
import uk.ac.cam.cares.jps.network.mail.MailNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.OtherInfoNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class RepositoryModule {
    @Provides
    @Singleton
    public AssetInfoRepository provideAssetInfoRepository(AssetNetworkSource networkSource, SettingRepository settingRepository) {
        return new AssetInfoRepository(networkSource, settingRepository);
    }

    @Provides
    @Singleton
    public MailRepository provideMailRepository(MailNetworkSource networkSource) {
        return new MailRepository(networkSource);
    }

    @Provides
    @Singleton
    public OtherInfoRepository provideOtherInfoRepository(OtherInfoLocalSource otherInfoLocalSource, OtherInfoNetworkSource otherInfoNetworkSource) {
        return new OtherInfoRepository(otherInfoLocalSource, otherInfoNetworkSource);
    }

    @Provides
    @Singleton
    public SettingRepository provideSettingRepository(SettingLocalSource settingLocalSource) {
        return new SettingRepository(settingLocalSource);
    }
}
