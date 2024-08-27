package uk.ac.cam.cares.jps.qrscan.analyzer;

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

import uk.ac.cam.cares.jps.qrscan.ui.ScanViewModel;

public class QRCodeAnalyzer implements ImageAnalysis.Analyzer {
    final static Logger LOGGER = Logger.getLogger(QRCodeAnalyzer.class);
    final ScanViewModel viewModel;
    BarcodeScanner scanner;

    public QRCodeAnalyzer(ScanViewModel viewModel) {
        BasicConfigurator.configure();
        this.viewModel = viewModel;

        BarcodeScannerOptions options =
                new BarcodeScannerOptions.Builder()
                        .setBarcodeFormats(Barcode.FORMAT_QR_CODE)
                        .build();

        scanner = BarcodeScanning.getClient(options);
    }

    @Override
    public void analyze(@NonNull ImageProxy imageProxy) {
        @SuppressLint("UnsafeOptInUsageError") Image image = imageProxy.getImage();
        if (image != null) {
            InputImage imageInput =
                    InputImage.fromMediaImage(image, imageProxy.getImageInfo().getRotationDegrees());

            scanner.process(imageInput)
                    .addOnSuccessListener(barcodes -> {


                        if (barcodes.size() == 0) {
                            // clear current overlay
                            viewModel.setUrl("");
                            viewModel.setBBoxAndResolution(null, imageProxy.getWidth(), imageProxy.getHeight());
                        } else {
                            for (Barcode barcode : barcodes) {
                                if (barcode.getUrl() != null) {
                                    LOGGER.info(barcode.getUrl().getUrl() + " " + barcode.getUrl().getTitle());
                                    if (barcode.getBoundingBox() != null) {
                                        LOGGER.info(barcode.getBoundingBox().left + " "+ barcode.getBoundingBox().top+ " "+barcode.getBoundingBox().right+ " "+barcode.getBoundingBox().bottom);
                                        LOGGER.info(barcode.getBoundingBox().toShortString());
                                        viewModel.setBBoxAndResolution(barcode.getBoundingBox(), imageProxy.getWidth(), imageProxy.getHeight());
                                        viewModel.setUrl(barcode.getUrl().getUrl());
                                    }

                                }
                            }
                        }

                        image.close();
                        imageProxy.close();
                    })
                    .addOnFailureListener(e -> {
                        viewModel.setBBoxAndResolution(null, imageProxy.getWidth(), imageProxy.getHeight());
                        LOGGER.error(e.getMessage());
                        image.close();
                        imageProxy.close();
                    });
        }

    }


}

