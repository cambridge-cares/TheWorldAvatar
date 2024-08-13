package uk.ac.cam.cares.jps.sampleapp;

import android.app.Application;

import org.apache.log4j.BasicConfigurator;

import dagger.hilt.android.HiltAndroidApp;

@HiltAndroidApp
public class SampleApplication extends Application {
    @Override
    public void onCreate() {
        super.onCreate();
        BasicConfigurator.configure();
    }
}
