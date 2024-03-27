package com.example.notsensorlogger2;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;
import android.media.AudioFormat;
import android.media.AudioRecord;
import android.media.MediaRecorder;
import android.util.Log;

import androidx.core.content.ContextCompat;

public class SoundLevelMeter {
    private static final int SAMPLE_RATE = 44100;
    private AudioRecord audioRecord;
    private int bufferSize;
    private boolean isRecording = false;
    private Context context;

    public SoundLevelMeter(Context context) {
        this.context = context;
        if (ContextCompat.checkSelfPermission(context, Manifest.permission.RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED) {
            bufferSize = AudioRecord.getMinBufferSize(SAMPLE_RATE, AudioFormat.CHANNEL_IN_MONO, AudioFormat.ENCODING_PCM_16BIT);
            audioRecord = new AudioRecord(MediaRecorder.AudioSource.MIC, SAMPLE_RATE, AudioFormat.CHANNEL_IN_MONO, AudioFormat.ENCODING_PCM_16BIT, bufferSize);
        } else {
            Log.e("SoundLevelMeter", "RECORD_AUDIO permission not granted");
        }
    }

    public void start() {
        if (audioRecord.getState() == AudioRecord.STATE_INITIALIZED) {
            audioRecord.startRecording();
            isRecording = true;
            new Thread(new Runnable() {
                @Override
                public void run() {
                    calculateDecibelLevel();
                }
            }).start();
        }
    }

    public void stop() {
        if (audioRecord != null) {
            isRecording = false;
            audioRecord.stop();
            audioRecord.release();
            audioRecord = null;
        }
    }

    private void calculateDecibelLevel() {
        short[] buffer = new short[bufferSize];
        while (isRecording) {
            int readResult = audioRecord.read(buffer, 0, bufferSize);
            if (readResult > 0) {
                double sum = 0;
                for (int i = 0; i < readResult; i++) {
                    sum += buffer[i] * buffer[i];
                }
                double rms = Math.sqrt(sum / readResult);
                double db = 20 * Math.log10(rms);
                Log.d("SoundLevelMeter", "Decibel level: " + db + " dB");
            }
        }
    }
}
