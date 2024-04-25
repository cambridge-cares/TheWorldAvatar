//package com.example.notsensorlogger2;
//
//import android.Manifest;
//import android.app.Activity;
//import android.content.Context;
//import android.content.pm.PackageManager;
//import android.media.AudioFormat;
//import android.media.AudioRecord;
//import android.media.MediaRecorder;
//import android.os.Build;
//import android.util.Log;
//
//import androidx.core.app.ActivityCompat;
//
//import org.json.JSONArray;
//import org.json.JSONException;
//import org.json.JSONObject;
//
//public class SoundLevelHandler implements SensorHandler {
//    private static final int SAMPLE_RATE = 44100; // Common sample rate that works on most devices
//    private static final int CHANNEL_CONFIG = AudioFormat.CHANNEL_IN_MONO;
//    private static final int AUDIO_FORMAT = AudioFormat.ENCODING_PCM_16BIT;
//    private static final int BUFFER_SIZE = AudioRecord.getMinBufferSize(SAMPLE_RATE, CHANNEL_CONFIG, AUDIO_FORMAT);
//    private static final int PERMISSION_REQUEST_RECORD_AUDIO = 200; // Unique request code
//
//    private AudioRecord audioRecord;
//    private Thread recordingThread;
//    private boolean isRecording = false;
//    private JSONArray sensorData;
//    private String sensorName;
//    private Context context;
//
//    public SoundLevelHandler(Context context) {
//        this.context = context;
//        this.sensorName = "Microphone";
//        this.sensorData = new JSONArray();
//        initAudioRecord();
//    }
//
//    private void initAudioRecord() {
//        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
//            ActivityCompat.requestPermissions((Activity) context, new String[]{Manifest.permission.RECORD_AUDIO}, PERMISSION_REQUEST_RECORD_AUDIO);
//        } else {
//            createAudioRecord();
//        }
//    }
//
//    private void createAudioRecord() {
//        audioRecord = new AudioRecord(MediaRecorder.AudioSource.MIC, SAMPLE_RATE, CHANNEL_CONFIG, AUDIO_FORMAT, BUFFER_SIZE);
//        if (audioRecord.getState() != AudioRecord.STATE_INITIALIZED) {
//            Log.e("AudioRecord", "Initialization failed.");
//            audioRecord = null; // Set to null to indicate failure
//        }
//    }
//
//    @Override
//    public void start() {
//        super.start();
//        audioRecord.startRecording();
//        isRecording = true;
//        startRecordingThread();
//    }
//
//    private void startRecordingThread() {
//        recordingThread = new Thread(new Runnable() {
//            @Override
//            public void run() {
//                processAudioStream();
//            }
//        }, "AudioRecorder Thread");
//        recordingThread.start();
//    }
//
//    private void processAudioStream() {
//        short[] buffer = new short[BUFFER_SIZE / 2];
//        while (isRecording) {
//            int readResult = audioRecord.read(buffer, 0, buffer.length);
//            if (readResult > 0) {
//                double dBFS = calculateDBFS(buffer, readResult);
//                logDBFS(dBFS);
//            }
//        }
//    }
//
//    private double calculateDBFS(short[] audioData, int readSize) {
//        double rms = 0.0;
//        for (int i = 0; i < readSize; i++) {
//            rms += audioData[i] * audioData[i];
//        }
//        rms = Math.sqrt(rms / readSize);
//        return 20 * Math.log10(rms / 32768);
//    }
//
//    private void logDBFS(double dBFS) {
//        JSONObject dataPoint = new JSONObject();
//        try {
//            JSONObject values = new JSONObject();
//            values.put("dBFS", dBFS);  // Use the dBFS parameter directly
//
//            dataPoint.put("name", "microphone");  // Use the sensorName set in the constructor
//            dataPoint.put("time", System.nanoTime());  // Record the current system time
//            dataPoint.put("values", values);  // Embed the dBFS value within a "values" JSONObject
//
//            synchronized (this) {
//                sensorData.put(dataPoint);  // Add the constructed JSON object to the sensorData array
//            }
//        } catch (JSONException e) {
//            e.printStackTrace();
//        }
//    }
//
//    @Override
//    public void stop() {
//        super.stop();
//        if (isRecording) {
//            isRecording = false;
//            try {
//                recordingThread.join();
//            } catch (InterruptedException e) {
//                Thread.currentThread().interrupt();
//            }
//            audioRecord.stop();
//            audioRecord.release();
//        }
//    }
//
//    @Override
//    public void onSensorChanged(SensorEvent event) {
//        // Not used for AudioRecord
//    }
//
//    @Override
//    public void onAccuracyChanged(Sensor sensor, int accuracy) {
//        // Not needed for AudioRecord
//    }
//
//    // Rest of the methods such as start, stop, processAudioStream, etc. will remain similar to what you had before,
//    // but now you need to ensure that audioRecord is not null before using it.
//
//    @Override
//    public JSONArray getSensorData() {
//        return sensorData;
//    }
//
//    // ... Include other overridden methods and any additional methods you need
//}
//

















package com.example.notsensorlogger2;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;
import android.media.AudioFormat;
import android.media.AudioRecord;
import android.media.MediaRecorder;
import android.util.Log;

import androidx.core.app.ActivityCompat;

import org.json.JSONException;
import org.json.JSONObject;

public class SoundLevelHandler extends AbstractSensorHandler {
    private static final int SAMPLE_RATE = 44100;  // Common sample rate that works on most devices
    private static final int CHANNEL_CONFIG = AudioFormat.CHANNEL_IN_MONO;
    private static final int AUDIO_FORMAT = AudioFormat.ENCODING_PCM_16BIT;
    private static final int BUFFER_SIZE = AudioRecord.getMinBufferSize(SAMPLE_RATE, CHANNEL_CONFIG, AUDIO_FORMAT);
    private Context context;
    private boolean isInitialized = false;
    private AudioRecord audioRecord;
    private Thread recordingThread;
    private boolean isRecording = false;
    private int frameCount = 0;  // Counter to control logging frequency

    public SoundLevelHandler(Context context, SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_ALL);
        this.context = context;
        this.sensorName = "Microphone";
        initAudioRecord();
    }

    private void initAudioRecord() {
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
            Log.e("AudioRecord", "RECORD_AUDIO permission not granted");
            return;
        }

        audioRecord = new AudioRecord(MediaRecorder.AudioSource.MIC, SAMPLE_RATE, CHANNEL_CONFIG, AUDIO_FORMAT, BUFFER_SIZE);
        if (audioRecord.getState() == AudioRecord.STATE_INITIALIZED) {
            isInitialized = true;
        } else {
            Log.e("AudioRecord", "Initialization failed.");
        }
    }

    @Override
    public void start() {
        if (!isInitialized) {
            Log.e("SoundLevelHandler", "AudioRecord not initialized");
            return;
        }
        super.start();
        audioRecord.startRecording();
        isRecording = true;
        startRecordingThread();
    }

    private void startRecordingThread() {
        recordingThread = new Thread(new Runnable() {
            @Override
            public void run() {
                processAudioStream();
            }
        }, "AudioRecorder Thread");
        recordingThread.start();
    }

    private void processAudioStream() {
        short[] buffer = new short[BUFFER_SIZE / 2];
        while (isRecording) {
            int readResult = audioRecord.read(buffer, 0, buffer.length);
            if (readResult > 0) {
                frameCount++;
                if (frameCount % 1000 == 0) {  // Log every 1000 frames
                    double dBFS = calculateDBFS(buffer, readResult);
                    logDBFS(dBFS);
                }
            }
        }
    }

    private double calculateDBFS(short[] audioData, int readSize) {
        double rms = 0.0;
        for (int i = 0; i < readSize; i++) {
            rms += audioData[i] * audioData[i];
        }
        rms = Math.sqrt(rms / readSize);
        return 20 * Math.log10(rms / 32768);
    }

    private void logDBFS(double dBFS) {
        JSONObject dataPoint = new JSONObject();
        try {
            JSONObject values = new JSONObject();
            values.put("dBFS", dBFS);
            dataPoint.put("name", "microphone");
            dataPoint.put("time", System.nanoTime());
            dataPoint.put("values", values);

            synchronized (this) {
                sensorData.put(dataPoint);
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void stop() {
        super.stop();
        if (isRecording) {
            isRecording = false;
            try {
                recordingThread.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            audioRecord.stop();
            audioRecord.release();
        }
    }


    @Override
    public void onSensorChanged(SensorEvent event) {
        // Not used for AudioRecord
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {
        // Not needed for AudioRecord
    }
}
