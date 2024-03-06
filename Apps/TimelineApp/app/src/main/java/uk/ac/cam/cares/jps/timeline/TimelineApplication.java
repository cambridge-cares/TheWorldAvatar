package uk.ac.cam.cares.jps.timeline;

import android.app.Application;

import org.apache.log4j.BasicConfigurator;

import dagger.hilt.android.HiltAndroidApp;

@HiltAndroidApp
public class TimelineApplication extends Application {
    @Override
    public void onCreate() {
        super.onCreate();
        BasicConfigurator.configure();
    }
}
