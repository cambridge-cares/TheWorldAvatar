package uk.ac.cam.cares.jps.assetmanagementapp;

import android.app.Application;

import dagger.hilt.android.HiltAndroidApp;

@HiltAndroidApp
public class AssetManagementApplication extends Application {

    @Override
    public void onCreate() {
        super.onCreate();
    }
}
