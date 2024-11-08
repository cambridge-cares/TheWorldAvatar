package uk.ac.cam.cares.jps.timeline;

import android.app.Application;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.FileAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.PropertyConfigurator;

import androidx.annotation.NonNull;
import androidx.work.Configuration;

import javax.inject.Inject;

import dagger.hilt.android.HiltAndroidApp;
import androidx.hilt.work.HiltWorkerFactory;

import java.io.File;
import java.io.IOException;

@HiltAndroidApp
public class TimelineApplication extends Application implements Configuration.Provider{
    private static final Logger log = Logger.getLogger(TimelineApplication.class);
    @Inject
    HiltWorkerFactory workerFactory;

    @Override
    public void onCreate() {
        super.onCreate();
        BasicConfigurator.configure();

        try {
            String filePath = getFilesDir().getAbsolutePath() + "/app_logs/app_log.txt";
            PatternLayout layout = new PatternLayout("%d{yyyy-MM-dd HH:mm:ss} %-5p %c{1}:%L - %m%n");
            FileAppender fileAppender = new FileAppender(layout, filePath, true);
            Logger.getRootLogger().addAppender(fileAppender);
            Logger.getRootLogger().setLevel(Level.INFO);
        } catch (IOException e) {
            e.printStackTrace();
        }

        log.info("Log4j configured programmatically");
    }


    @NonNull
    @Override
    public Configuration getWorkManagerConfiguration() {
        return new Configuration.Builder()
                .setWorkerFactory(workerFactory)
                .build();
    }

}
