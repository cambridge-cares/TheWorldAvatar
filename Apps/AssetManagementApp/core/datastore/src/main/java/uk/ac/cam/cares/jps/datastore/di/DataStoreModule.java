package uk.ac.cam.cares.jps.datastore.di;

import android.content.Context;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.datastore.OtherInfoLocalSource;

@Module
@InstallIn(SingletonComponent.class)
public class DataStoreModule {

    @Provides
    public OtherInfoLocalSource provideOtherInfoLocalSource(@ApplicationContext Context applicationContext) {
        return new OtherInfoLocalSource(applicationContext);
    }
}
