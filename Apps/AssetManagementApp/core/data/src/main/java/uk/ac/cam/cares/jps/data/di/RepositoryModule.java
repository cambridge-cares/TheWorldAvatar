package uk.ac.cam.cares.jps.data.di;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.data.assetinfo.AssetInfoRepository;
import uk.ac.cam.cares.jps.data.mail.MailRepository;
import uk.ac.cam.cares.jps.data.maintenance.MaintenanceRepository;
import uk.ac.cam.cares.jps.data.otherinfo.OtherInfoRepository;
import uk.ac.cam.cares.jps.data.qrprint.QRPrintRepository;
import uk.ac.cam.cares.jps.data.setting.SettingRepository;
import uk.ac.cam.cares.jps.datastore.OtherInfoLocalSource;
import uk.ac.cam.cares.jps.datastore.QRPrintingLocalSource;
import uk.ac.cam.cares.jps.datastore.SettingLocalSource;
import uk.ac.cam.cares.jps.network.assetinfo.AssetNetworkSource;
import uk.ac.cam.cares.jps.network.datasheet.DataSheetNetworkSource;
import uk.ac.cam.cares.jps.network.mail.MailNetworkSource;
import uk.ac.cam.cares.jps.network.maintenance.MaintenanceNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.BMSNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.OtherInfoNetworkSource;
import uk.ac.cam.cares.jps.network.qrprint.QRPrintingNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class RepositoryModule {
    @Provides
    @Singleton
    public AssetInfoRepository provideAssetInfoRepository(AssetNetworkSource assetNetworkSource, SettingRepository settingRepository, DataSheetNetworkSource dataSheetNetworkSource) {
        return new AssetInfoRepository(assetNetworkSource, settingRepository, dataSheetNetworkSource);
    }

    @Provides
    @Singleton
    public MailRepository provideMailRepository(MailNetworkSource networkSource) {
        return new MailRepository(networkSource);
    }

    @Provides
    @Singleton
    public OtherInfoRepository provideOtherInfoRepository(OtherInfoLocalSource otherInfoLocalSource, OtherInfoNetworkSource otherInfoNetworkSource, BMSNetworkSource bmsNetworkSource) {
        return new OtherInfoRepository(otherInfoLocalSource, otherInfoNetworkSource, bmsNetworkSource);
    }

    @Provides
    @Singleton
    public SettingRepository provideSettingRepository(SettingLocalSource settingLocalSource) {
        return new SettingRepository(settingLocalSource);
    }

    @Provides
    @Singleton
    public QRPrintRepository provideQRPrintingRepository(QRPrintingLocalSource localSource, QRPrintingNetworkSource networkSource) {
        return new QRPrintRepository(localSource, networkSource);
    }

    @Provides
    @Singleton
    public MaintenanceRepository provideMaintenanceRepository(MaintenanceNetworkSource networkSource) {
        return new MaintenanceRepository(networkSource);
    }
}
