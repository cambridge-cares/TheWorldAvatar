package uk.ac.cam.cares.jps.assetmanagementapp;

import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.ScaleGestureDetector;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.camera.core.Camera;
import androidx.camera.core.CameraSelector;
import androidx.camera.core.ImageAnalysis;
import androidx.camera.core.Preview;
import androidx.camera.lifecycle.ProcessCameraProvider;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.common.util.concurrent.ListenableFuture;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;

import uk.ac.cam.cares.jps.assetmanagementapp.databinding.ActivityScanBinding;
import uk.ac.cam.cares.jps.assetmanagementapp.vision.BoxOverlayView;
import uk.ac.cam.cares.jps.assetmanagementapp.vision.QRCodeAnalyzer;
import uk.ac.cam.cares.jps.assetmanagementapp.vision.ScanViewModel;

public class ScanActivity extends AppCompatActivity {
    private ListenableFuture<ProcessCameraProvider> cameraProviderFuture;
    private ActivityScanBinding binding;
    private final Logger LOGGER = Logger.getLogger(ScanActivity.class);
    private final int CAMERA_PERMISSION_REQUEST_CODE = 777;
    private Executor cameraExecutor;

    private ScanViewModel viewModel;
    private BoxOverlayView boxOverlayView;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        LOGGER.info("started scan activity");

        binding = ActivityScanBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        boxOverlayView = binding.boxOverlayView;

        cameraExecutor = ContextCompat.getMainExecutor(this);
        viewModel = new ViewModelProvider(this).get(ScanViewModel.class);
        viewModel.getBBox().observe(this, bBox -> boxOverlayView.setRect(bBox));
        binding.topActionBar.flashButton.setOnClickListener(v -> viewModel.toggleFlashState());



        if (ContextCompat.checkSelfPermission(this, android.Manifest.permission.CAMERA) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(this, new String[]{android.Manifest.permission.CAMERA}, CAMERA_PERMISSION_REQUEST_CODE);
        } else {
            startCamera();
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        if( requestCode == CAMERA_PERMISSION_REQUEST_CODE ) {
            for( int p = 0; p < permissions.length; p++ ) {
                if( android.Manifest.permission.CAMERA.equals(permissions[p]) ) {
                    if( grantResults[p] == PackageManager.PERMISSION_GRANTED ) {
                        startCamera();
                    } else {
                        new MaterialAlertDialogBuilder(this)
                                .setTitle(R.string.request_camera_permission)
                                .setMessage(R.string.faile_to_get_camera_permission)
                                .setPositiveButton(R.string.retry, (dialogInterface, i) -> ActivityCompat.requestPermissions(this, new String[]{android.Manifest.permission.CAMERA}, CAMERA_PERMISSION_REQUEST_CODE))
                                .setNegativeButton(R.string.cancel, (dialogInterface, i) -> finish())
                                .show();
                    }
                }
            }
        } else {
            super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        }
    }

    private void startCamera() {
        cameraProviderFuture = ProcessCameraProvider.getInstance(this);
        cameraProviderFuture.addListener(() -> {
            try {
                ProcessCameraProvider cameraProvider = cameraProviderFuture.get();
                bindPreview(cameraProvider);
            } catch (ExecutionException | InterruptedException e) {
                // No errors need to be handled for this Future.
                // This should never be reached.
            }
        }, ContextCompat.getMainExecutor(this));
    }

    void bindPreview(@NonNull ProcessCameraProvider cameraProvider) {
        Preview preview = new Preview.Builder()
                .build();

        CameraSelector cameraSelector = new CameraSelector.Builder()
                .requireLensFacing(CameraSelector.LENS_FACING_BACK)
                .build();

        preview.setSurfaceProvider(binding.previewView.getSurfaceProvider());

        ImageAnalysis qrAnalysis = new ImageAnalysis.Builder()
                .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
                .build();
        qrAnalysis.setAnalyzer(cameraExecutor, new QRCodeAnalyzer(viewModel));

        cameraProvider.unbindAll();
        Camera camera = cameraProvider.bindToLifecycle(this, cameraSelector, preview, qrAnalysis);
        setupCameraControl(camera);

        // image resolution width and height are opposite
        boxOverlayView.setImageInfo(qrAnalysis.getResolutionInfo().getResolution().getHeight(), qrAnalysis.getResolutionInfo().getResolution().getWidth());
    }

    void setupCameraControl(Camera camera) {
        // flash light control
        if (camera.getCameraInfo().hasFlashUnit()) {
            viewModel.getIsFlashOn().observe(this, isFlashOn -> camera.getCameraControl().enableTorch(isFlashOn));
        }

        //  pinch-to-zoom
        ScaleGestureDetector scaleGestureDetector = new ScaleGestureDetector(this, new ScaleGestureDetector.SimpleOnScaleGestureListener() {
            @Override
            public boolean onScale(ScaleGestureDetector detector) {
                // Get the current camera zoom ratio
                float currentZoomRatio = camera.getCameraInfo().getZoomState().getValue() != null ? camera.getCameraInfo().getZoomState().getValue().getZoomRatio() : 1.0F;

                // Get by how much the scale has changed due to the user's pinch gesture
                float delta = detector.getScaleFactor();

                // Update the camera's zoom ratio
                camera.getCameraControl().setZoomRatio(currentZoomRatio * delta);
                return true;
            }
        });
        binding.previewView.setOnTouchListener((view, motionEvent) ->  {
            scaleGestureDetector.onTouchEvent(motionEvent);
            return true;
        });
    }
}
