package uk.ac.cam.cares.jps.timeline;

import android.app.Application;

import org.apache.log4j.BasicConfigurator;

import androidx.annotation.NonNull;
import androidx.work.Configuration;

import javax.inject.Inject;

import dagger.hilt.android.HiltAndroidApp;
import androidx.hilt.work.HiltWorkerFactory;

@HiltAndroidApp
public class TimelineApplication extends Application implements Configuration.Provider{
    @Inject
    HiltWorkerFactory workerFactory;

    @Override
    public void onCreate() {
        super.onCreate();
        BasicConfigurator.configure();
    }

    @NonNull
    @Override
    public Configuration getWorkManagerConfiguration() {
        return new Configuration.Builder()
                .setWorkerFactory(workerFactory)
                .build();
    }

}
