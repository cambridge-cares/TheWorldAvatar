package uk.ac.cam.cares.jps.datastore.di;

import android.content.Context;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.datastore.OtherInfoLocalSource;
import uk.ac.cam.cares.jps.datastore.SettingLocalSource;

@Module
@InstallIn(SingletonComponent.class)
public class DataStoreModule {

    @Provides
    @Singleton
    public OtherInfoLocalSource provideOtherInfoLocalSource(@ApplicationContext Context applicationContext) {
        return new OtherInfoLocalSource(applicationContext);
    }

    @Provides
    @Singleton
    public SettingLocalSource provideSettingLocalSource(@ApplicationContext Context applicationContext) {
        return new SettingLocalSource(applicationContext);
    }
}
