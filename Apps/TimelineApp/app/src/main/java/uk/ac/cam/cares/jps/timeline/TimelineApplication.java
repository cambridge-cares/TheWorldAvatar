package uk.ac.cam.cares.jps.timeline;

import android.app.Application;

import androidx.appcompat.app.AppCompatDelegate;
import androidx.emoji2.bundled.BundledEmojiCompatConfig;
import androidx.emoji2.text.EmojiCompat;

import org.apache.log4j.BasicConfigurator;

import androidx.annotation.NonNull;
import androidx.work.Configuration;

import javax.inject.Inject;

import dagger.hilt.android.HiltAndroidApp;
import androidx.hilt.work.HiltWorkerFactory;
import androidx.work.WorkManager;

@HiltAndroidApp
public class TimelineApplication extends Application implements Configuration.Provider{
    @Inject
    HiltWorkerFactory workerFactory;

    @Override
    public void onCreate() {
        super.onCreate();
        BasicConfigurator.configure();

        AppCompatDelegate.setCompatVectorFromResourcesEnabled(false);

        EmojiCompat.Config config = new BundledEmojiCompatConfig(this)
                .setReplaceAll(false);  // Prevents EmojiCompat from replacing all emojis globally
        EmojiCompat.init(config);

    }

    @NonNull
    @Override
    public Configuration getWorkManagerConfiguration() {
        return new Configuration.Builder()
                .setWorkerFactory(workerFactory)
                .build();
    }

}
