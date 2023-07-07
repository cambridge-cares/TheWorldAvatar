package uk.ac.cam.cares.jps.assetmanagementapp;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.camera.core.Camera;
import androidx.camera.core.CameraControl;
import androidx.camera.core.CameraSelector;
import androidx.camera.core.FocusMeteringAction;
import androidx.camera.core.ImageAnalysis;
import androidx.camera.core.MeteringPoint;
import androidx.camera.core.MeteringPointFactory;
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
        binding.topActionBar.closeButton.setOnClickListener(v -> finish());

        cameraExecutor = ContextCompat.getMainExecutor(this);
        viewModel = new ViewModelProvider(this).get(ScanViewModel.class);
        viewModel.getBBox().observe(this, bBox -> boxOverlayView.setRect(bBox));
        viewModel.getConfirmedUrl().observe(this, url -> {
            Intent intent = new Intent(getBaseContext(), AssetInfoActivity.class);
            intent.putExtra(AssetInfoActivity.ASSET_URI, url);
            startActivity(intent);
        });

        binding.topActionBar.flashButton.setOnClickListener(v -> viewModel.toggleFlashState());


        if (ContextCompat.checkSelfPermission(this, android.Manifest.permission.CAMERA) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(this, new String[]{android.Manifest.permission.CAMERA}, CAMERA_PERMISSION_REQUEST_CODE);
        } else {
            startCamera();
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        viewModel.resetUrlState();
        boxOverlayView.setRect(null);
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
        ListenableFuture<ProcessCameraProvider>  cameraProviderFuture = ProcessCameraProvider.getInstance(this);
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

    @SuppressLint("ClickableViewAccessibility")
    void setupCameraControl(Camera camera) {
        // flash light control
        if (camera.getCameraInfo().hasFlashUnit()) {
            viewModel.getIsFlashOn().observe(this, isFlashOn -> camera.getCameraControl().enableTorch(isFlashOn));
        }

        CameraControl cameraControl = camera.getCameraControl();
        //  pinch-to-zoom
        ScaleGestureDetector scaleGestureDetector = new ScaleGestureDetector(this, new ScaleGestureDetector.SimpleOnScaleGestureListener() {
            @Override
            public boolean onScale(ScaleGestureDetector detector) {
                // Get the current camera zoom ratio
                float currentZoomRatio = camera.getCameraInfo().getZoomState().getValue() != null ? camera.getCameraInfo().getZoomState().getValue().getZoomRatio() : 1.0F;

                // Get by how much the scale has changed due to the user's pinch gesture
                float delta = detector.getScaleFactor();

                // Update the camera's zoom ratio
                cameraControl.setZoomRatio(currentZoomRatio * delta);
                return true;
            }
        });
        binding.previewView.setOnTouchListener((view, motionEvent) ->  {
            if (motionEvent.getAction() == MotionEvent.ACTION_UP) {
                MeteringPointFactory factory = binding.previewView.getMeteringPointFactory();
                MeteringPoint point = factory.createPoint(motionEvent.getX(), motionEvent.getY());
                FocusMeteringAction action = new FocusMeteringAction.Builder(point).build();
                cameraControl.startFocusAndMetering(action);

                binding.focusCircleView.showCircle(motionEvent.getX(), motionEvent.getY());
            }
            scaleGestureDetector.onTouchEvent(motionEvent);

            return true;
        });
    }
}
