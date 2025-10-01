package uk.ac.cam.cares.jps.utils.di;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.toolbox.Volley;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.zip.GZIPOutputStream;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;

/**
 * Dependency injection specification for Utils module
 */
@Module
@InstallIn(SingletonComponent.class)
public class UtilsModule {
    @Provides
    @Singleton
    public RequestQueue provideRequestQueue(@ApplicationContext Context applicationContext) {
        return Volley.newRequestQueue(applicationContext);
    }


}
