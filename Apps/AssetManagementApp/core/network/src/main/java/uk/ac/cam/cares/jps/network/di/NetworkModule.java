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
    public AssetNetworkSource provideAssetNetworkSource(Connection connection) {
        return new AssetNetworkSource(connection);
    }

    @Provides
    @Singleton
    public MailNetworkSource provideMailNetworkSource(Connection connection) {
        return new MailNetworkSource(connection);
    }

    @Provides
    @Singleton
    public OtherInfoNetworkSource provideOtherInfoNetworkSource(Connection connection) {
        return new OtherInfoNetworkSource(connection);
    }

    @Provides
    @Singleton
    public BMSNetworkSource provideBMSNetworkSource(Connection connection) {
        return new BMSNetworkSource(connection);
    }

    @Provides
    @Singleton
    public DataSheetNetworkSource provideDocumentNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new DataSheetNetworkSource(connection, applicationContext);
    }

    @Provides
    @Singleton
    public QRPrintingNetworkSource provideQRPrintingNetworkSource(Connection connection) {
        return new QRPrintingNetworkSource(connection);
    }

    @Provides
    @Singleton
    public MaintenanceNetworkSource provideMaintenanceNetworkSource(Connection connection) {
        return new MaintenanceNetworkSource(connection);
    }

}
