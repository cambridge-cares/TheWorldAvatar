package uk.ac.cam.cares.jps.assetmanagementapp;

import android.app.Application;

import dagger.Provides;
import dagger.hilt.android.HiltAndroidApp;
import uk.ac.cam.cares.jps.network.Connection;

@HiltAndroidApp
public class AssetManagementApplication extends Application {

    @Override
    public void onCreate() {
        super.onCreate();
    }
}
