package uk.ac.cam.cares.jps.network.di;

import android.content.Context;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.network.assetinfo.AssetNetworkSource;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.datasheet.DataSheetNetworkSource;
import uk.ac.cam.cares.jps.network.mail.MailNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.BMSNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.OtherInfoNetworkSource;

@Module
@InstallIn(SingletonComponent.class)
public class NetworkModule {
    @Provides
    public Connection provideConnection(@ApplicationContext Context applicationContext) {
        return Connection.getInstance(applicationContext);
    }

    @Provides
    public AssetNetworkSource provideAssetNetworkSource(Connection connection) {
        return new AssetNetworkSource(connection);
    }

    @Provides
    public MailNetworkSource provideMailNetworkSource(Connection connection) {
        return new MailNetworkSource(connection);
    }

    @Provides
    public OtherInfoNetworkSource provideOtherInfoNetworkSource(Connection connection) {
        return new OtherInfoNetworkSource(connection);
    }

    @Provides
    public BMSNetworkSource provideBMSNetworkSource(Connection connection) {
        return new BMSNetworkSource(connection);
    }

    @Provides
    public DataSheetNetworkSource provideDocumentNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        return new DataSheetNetworkSource(connection, applicationContext);
    }
}
