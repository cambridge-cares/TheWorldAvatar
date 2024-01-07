package uk.ac.cam.cares.jps.routing;

import android.app.Application;

import org.apache.log4j.BasicConfigurator;

import dagger.hilt.android.HiltAndroidApp;

@HiltAndroidApp
public class RoutingApplication extends Application {
    @Override
    public void onCreate() {
        super.onCreate();
        BasicConfigurator.configure();
    }
}
