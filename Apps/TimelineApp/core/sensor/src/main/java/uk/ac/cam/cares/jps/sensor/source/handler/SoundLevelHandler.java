package uk.ac.cam.cares.jps.sensor.source.handler;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;
import android.media.AudioFormat;
import android.media.AudioRecord;
import android.media.MediaRecorder;

import androidx.core.app.ActivityCompat;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Handles the microphone input to measure sound levels in dBFS (decibels relative to full scale).
 * This class initializes and manages an AudioRecord object to capture audio data from the microphone
 * and compute its loudness in real time. It extends {@link AbstractSensorHandler} to integrate seamlessly
 * with sensor handling mechanisms.
 */
public class SoundLevelHandler extends AbstractSensorHandler {
    private static final int SAMPLE_RATE = 160000; // reduced from 44100 for better thread management
    private static final int CHANNEL_CONFIG = AudioFormat.CHANNEL_IN_MONO;
    private static final int AUDIO_FORMAT = AudioFormat.ENCODING_PCM_16BIT;
    private static final int BUFFER_SIZE = AudioRecord.getMinBufferSize(SAMPLE_RATE, CHANNEL_CONFIG, AUDIO_FORMAT);
    private Context context;
    private AudioRecord audioRecord;
    private Thread recordingThread;
    private final Object audioRecordLock = new Object();


    /**
     * Constructs a new SoundLevelHandler.
     *
     * @param context The application context, used for checking permissions.
     * @param sensorManager The sensor manager passed to the superclass constructor.
     */
    public SoundLevelHandler(Context context, SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_ALL);
        this.context = context;
        this.sensorName = "microphone";
    }

    /**
     * Initializes the AudioRecord. If permissions are not granted, this method simply returns.
     * Releases any previously held AudioRecord before initializing a new one.
     */
    public void initAudioRecord() {
        if (audioRecord != null) {
            audioRecord.release();
        }
        // permissions are handled by sensorSettingFragment
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
            return;
        }
        audioRecord = new AudioRecord(
                MediaRecorder.AudioSource.MIC,
                SAMPLE_RATE,
                CHANNEL_CONFIG,
                AUDIO_FORMAT,
                BUFFER_SIZE
        );
    }

    /**
     * Starts recording audio if the AudioRecord is initialized. Initializes and starts a new thread
     * to process the audio stream.
     */
    @Override
    public synchronized void start(Integer integer) {
        super.start(integer);
        initAudioRecord();

        if (audioRecord == null || audioRecord.getState() != AudioRecord.STATE_INITIALIZED) {
            LOGGER.warn("AudioRecord not initialized properly.");
            return;
        }

        try {
            audioRecord.startRecording();
        } catch (IllegalStateException e) {
            LOGGER.error("Failed to start recording.", e);
            e.printStackTrace();
            return;
        }
        startRecordingThread();
    }

    /**
     * Starts a new thread to continuously read audio data from the microphone and process it.
     */
    private void startRecordingThread() {
        recordingThread = new Thread(new Runnable() {
            @Override
            public void run() {
                processAudioStream();
            }
        }, "AudioRecorder Thread");
        recordingThread.start();
    }

    /**
     * Reads audio data from the microphone, calculates the dBFS, and logs it. Continues until recording is stopped.
     */
    private void processAudioStream() {
        short[] buffer = new short[BUFFER_SIZE / 2];
        while (true) {
            int readResult;

            synchronized (audioRecordLock) {
                if (audioRecord == null || audioRecord.getRecordingState() != AudioRecord.RECORDSTATE_RECORDING) {
                    break;
                }
                try {
                    readResult = audioRecord.read(buffer, 0, buffer.length);
                } catch (Exception e) {
                    LOGGER.error("Error reading audio data.", e);
                    break;
                }
            }

            if (readResult > 0) {
                double dBFS = calculateDBFS(buffer, readResult);
                logDBFS(dBFS);
            }
        }
    }



    /**
     * Calculates the dBFS from the audio buffer.
     *
     * @param audioData The audio buffer containing PCM data.
     * @param readSize The number of read bytes.
     * @return The calculated dBFS value.
     */
    private double calculateDBFS(short[] audioData, int readSize) {
        double rms = 0.0;
        for (int i = 0; i < readSize; i++) {
            rms += audioData[i] * audioData[i];
        }
        rms = Math.sqrt(rms / readSize);
        return 20 * Math.log10(rms / 32768);
    }

    /**
     * Logs the dBFS to the sensor data array.
     *
     * @param dBFS The dBFS value to log.
     */
    private void logDBFS(double dBFS) {
        if (Double.isInfinite(dBFS) || Double.isNaN(dBFS)) {
            return;
        }

        JSONObject dataPoint = new JSONObject();
        try {
            JSONObject values = new JSONObject();
            values.put("dBFS", dBFS);

            dataPoint.put("name", "microphone");
            dataPoint.put("time", System.currentTimeMillis());
            dataPoint.put("values", values);

            synchronized (sensorDataLock) {
                sensorData.put(dataPoint);
            }
        } catch (JSONException e) {
            LOGGER.error("JSON exception in logDBFS.", e);
            e.printStackTrace();
        }
    }

    /**
     * Stops the audio recording and releases resources associated with the AudioRecord object.
     */
    @Override
    public synchronized void stop() {
        super.stop();

        synchronized (audioRecordLock) {
            if (audioRecord != null) {
                try {
                    if (audioRecord.getRecordingState() == AudioRecord.RECORDSTATE_RECORDING) {
                        audioRecord.stop();
                    }
                } catch (IllegalStateException e) {
                    e.printStackTrace();
                }
                audioRecord.release();
                audioRecord = null;
            }
        }

        if (recordingThread != null) {
            try {
                recordingThread.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            recordingThread = null;
        }

    }


    @Override
    public SensorType getSensorType() {
        return SensorType.SOUND;
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        // Not used for AudioRecord
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {
        // Not needed for AudioRecord
    }
    @Override
    public Boolean isRunning() {
        synchronized (audioRecordLock) {
            return audioRecord != null && audioRecord.getRecordingState() == AudioRecord.RECORDSTATE_RECORDING;
        }
    }

}
