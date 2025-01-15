package uk.ac.cam.cares.jps.login.di;

import android.content.Context;

import net.openid.appauth.AuthorizationService;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;
import uk.ac.cam.cares.jps.login.AuthServerConfiguration;
import uk.ac.cam.cares.jps.login.AuthStateManager;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.LoginSource;
import uk.ac.cam.cares.jps.network.Connection;

@Module
@InstallIn(SingletonComponent.class)
public class LoginModule {

    @Provides
    @Singleton
    public AuthServerConfiguration provideAuthServerConfiguration(@ApplicationContext Context applicationContext) {
        return new AuthServerConfiguration(applicationContext);
    }

    @Provides
    @Singleton
    public AuthStateManager provideAuthStateManager(@ApplicationContext Context applicationContext) {
        return new AuthStateManager(applicationContext);
    }

    @Provides
    @Singleton
    public LoginSource provideLoginSource(@ApplicationContext Context context, AuthStateManager authStateManager, AuthServerConfiguration configuration, Connection connection) {
        return new LoginSource(context, authStateManager, configuration, connection);
    }

    @Provides
    @Singleton
    public LoginRepository provideLoginRepository(LoginSource loginSource) {
        return new LoginRepository(loginSource);
    }
}
