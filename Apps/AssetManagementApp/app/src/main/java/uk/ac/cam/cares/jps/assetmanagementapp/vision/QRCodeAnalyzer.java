package uk.ac.cam.cares.jps.assetmanagementapp.vision;

import android.annotation.SuppressLint;
import android.media.Image;

import androidx.annotation.NonNull;
import androidx.camera.core.ImageAnalysis;
import androidx.camera.core.ImageProxy;

import com.google.mlkit.vision.barcode.BarcodeScanner;
import com.google.mlkit.vision.barcode.BarcodeScannerOptions;
import com.google.mlkit.vision.barcode.BarcodeScanning;
import com.google.mlkit.vision.barcode.common.Barcode;
import com.google.mlkit.vision.common.InputImage;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.Timer;
import java.util.TimerTask;

public class QRCodeAnalyzer implements ImageAnalysis.Analyzer {
    final static Logger LOGGER = Logger.getLogger(QRCodeAnalyzer.class);
    final QRCodeViewModel viewModel;

    // ===============================================================================
    private final Timer fpsTimer = new Timer();
    private int frameProcessedInOneSecondInterval = 0;
    private int framesPerSecond = 0;




    public QRCodeAnalyzer(QRCodeViewModel viewModel) {
        BasicConfigurator.configure();
        this.viewModel = viewModel;


        fpsTimer.scheduleAtFixedRate(
                new TimerTask() {
                    @Override
                    public void run() {
                        framesPerSecond = frameProcessedInOneSecondInterval;
                        frameProcessedInOneSecondInterval = 0;
                        LOGGER.info("FPS: " + framesPerSecond);
                    }
                },
                0,
                1000);
    }

    @Override
    public void analyze(@NonNull ImageProxy imageProxy) {
        @SuppressLint("UnsafeOptInUsageError") Image image = imageProxy.getImage();
        if (image != null) {
            frameProcessedInOneSecondInterval++;
            InputImage imageInput =
                    InputImage.fromMediaImage(image, imageProxy.getImageInfo().getRotationDegrees());

            BarcodeScannerOptions options =
                    new BarcodeScannerOptions.Builder()
                            .setBarcodeFormats(Barcode.FORMAT_QR_CODE)
                            .build();

            BarcodeScanner scanner = BarcodeScanning.getClient(options);
            scanner.process(imageInput)
                    .addOnSuccessListener(barcodes -> {
                        for (Barcode barcode : barcodes) {
                            if (barcode.getUrl() != null) {
                                LOGGER.info(barcode.getUrl().getUrl() + " " + barcode.getUrl().getTitle());
                                if (barcode.getBoundingBox() != null) {
                                    LOGGER.info(barcode.getBoundingBox().left + " "+ barcode.getBoundingBox().top+ " "+barcode.getBoundingBox().right+ " "+barcode.getBoundingBox().bottom);
                                    LOGGER.info(barcode.getBoundingBox().toShortString());
                                    viewModel.setBBox(barcode.getBoundingBox());
                                }

                            }
                        }
                    })
                    .addOnFailureListener(e -> { });
        }

        imageProxy.close();

    }


}

