package uk.ac.cam.cares.jps.network.di;

import android.content.Context;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.network.assetinfo.AssetNetworkSource;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.datasheet.DataSheetNetworkSource;
import uk.ac.cam.cares.jps.network.mail.MailNetworkSource;
import uk.ac.cam.cares.jps.network.maintenance.MaintenanceNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.BMSNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.OtherInfoNetworkSource;
import uk.ac.cam.cares.jps.network.qrprint.QRPrintingNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class NetworkModule {
    @Provides
    @Singleton
    public Connection provideConnection(@ApplicationContext Context applicationContext) {
        return Connection.getInstance(applicationContext);
    }

    @Provides
    @Singleton
    public AssetNetworkSource provideAssetNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new AssetNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public MailNetworkSource provideMailNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new MailNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public OtherInfoNetworkSource provideOtherInfoNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new OtherInfoNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public BMSNetworkSource provideBMSNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new BMSNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public DataSheetNetworkSource provideDocumentNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new DataSheetNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public QRPrintingNetworkSource provideQRPrintingNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new QRPrintingNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public MaintenanceNetworkSource provideMaintenanceNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new MaintenanceNetworkSource(connection, applicationContext);
    }

}
