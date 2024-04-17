package com.example.notsensorlogger2;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;
import android.media.AudioFormat;
import android.media.AudioRecord;
import android.media.MediaRecorder;
import android.util.Log;

import org.json.JSONException;
import org.json.JSONObject;

public class SoundLevelHandler extends AbstractSensorHandler {
    private static final int SAMPLE_RATE = 44100;  // Common sample rate that works on most devices
    private static final int CHANNEL_CONFIG = AudioFormat.CHANNEL_IN_MONO;
    private static final int AUDIO_FORMAT = AudioFormat.ENCODING_PCM_16BIT;
    private static final int BUFFER_SIZE = AudioRecord.getMinBufferSize(SAMPLE_RATE, CHANNEL_CONFIG, AUDIO_FORMAT);

    private AudioRecord audioRecord;
    private Thread recordingThread;
    private boolean isRecording = false;

    public SoundLevelHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_ALL);  // AudioRecord does not use sensorType
        this.sensorName = "Microphone";
        initAudioRecord();
    }

    private void initAudioRecord() {
        audioRecord = new AudioRecord(MediaRecorder.AudioSource.MIC, SAMPLE_RATE, CHANNEL_CONFIG, AUDIO_FORMAT, BUFFER_SIZE);
        if (audioRecord.getState() != AudioRecord.STATE_INITIALIZED) {
            Log.e("AudioRecord", "Initialization failed.");
        }
    }

    @Override
    public void start() {
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
                double dBFS = calculateDBFS(buffer, readResult);
                logDBFS(dBFS);
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
            values.put("dBFS", dBFS);  // Use the dBFS parameter directly

            dataPoint.put("name", "microphone");  // Use the sensorName set in the constructor
            dataPoint.put("time", System.nanoTime());  // Record the current system time
            dataPoint.put("values", values);  // Embed the dBFS value within a "values" JSONObject

            synchronized (this) {
                sensorData.put(dataPoint);  // Add the constructed JSON object to the sensorData array
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }


//    private void logDBFS(double dBFS) {
//        JSONObject dataPoint = new JSONObject();
//        try {
//            JSONObject values = new JSONObject();
//            values.put("dBFS", event.values[0]);
//
//            dataPoint.put("name", "microphone");
//            dataPoint.put("time", System.nanoTime());
//            dataPoint.put("values", values);
//
//            synchronized (this) {
//                sensorData.put(dataPoint);
//            }
//        } catch (JSONException e) {
//            e.printStackTrace();
//        }
//    }

//    @Override
//    public void onSensorChanged(SensorEvent event) {
//        JSONObject dataPoint = new JSONObject();
//        try {
//            JSONObject values = new JSONObject();
//            values.put("lux", event.values[0]);
//
//            dataPoint.put("name", this.sensorName);
//            dataPoint.put("time", System.nanoTime());
//            dataPoint.put("values", values);
//
//            synchronized (this) {
//                sensorData.put(dataPoint);
//            }
//        } catch (JSONException e) {
//            e.printStackTrace();
//        }
//    }
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
