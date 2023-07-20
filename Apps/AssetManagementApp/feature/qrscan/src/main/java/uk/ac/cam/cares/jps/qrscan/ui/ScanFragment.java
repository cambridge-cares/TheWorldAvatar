package uk.ac.cam.cares.jps.qrscan.ui;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.view.View;
import android.view.ViewGroup;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.camera.core.Camera;
import androidx.camera.core.CameraControl;
import androidx.camera.core.CameraSelector;
import androidx.camera.core.FocusMeteringAction;
import androidx.camera.core.ImageAnalysis;
import androidx.camera.core.MeteringPoint;
import androidx.camera.core.MeteringPointFactory;
import androidx.camera.core.Preview;
import androidx.camera.lifecycle.ProcessCameraProvider;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.common.util.concurrent.ListenableFuture;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;

import uk.ac.cam.cares.jps.qrscan.R;
import uk.ac.cam.cares.jps.qrscan.analyzer.QRCodeAnalyzer;
import uk.ac.cam.cares.jps.qrscan.databinding.FragmentScanBinding;

public class ScanFragment extends Fragment {
    private final Logger LOGGER = Logger.getLogger(ScanFragment.class);
    private ScanViewModel viewModel;
    private FragmentScanBinding binding;
    private BoxOverlayView boxOverlayView;


    private Executor cameraExecutor;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentScanBinding.inflate(inflater);

        return binding.getRoot();
    }

    @Override
    public void onResume() {
        super.onResume();
        viewModel.resetUrlState();
        boxOverlayView.setRect(null);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        boxOverlayView = binding.boxOverlayView;
        viewModel = new ViewModelProvider(this).get(ScanViewModel.class);

        viewModel.getBBox().observe(getViewLifecycleOwner(), bBox -> boxOverlayView.setRect(bBox));
        viewModel.getConfirmedUrl().observe(getViewLifecycleOwner(), url -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/info_page?uri=" + url))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
        });

        binding.topActionBar.closeButton.setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());
        binding.topActionBar.flashButton.setOnClickListener(v -> viewModel.toggleFlashState());

        cameraExecutor = ContextCompat.getMainExecutor(getContext());

        if (ContextCompat.checkSelfPermission(getContext(), android.Manifest.permission.CAMERA) != PackageManager.PERMISSION_GRANTED) {
//            ActivityCompat.requestPermissions(getActivity(), new String[]{android.Manifest.permission.CAMERA}, CAMERA_PERMISSION_REQUEST_CODE);
            MaterialAlertDialogBuilder permissionDeniedDialog = new MaterialAlertDialogBuilder(getContext())
                    .setTitle(R.string.request_camera_permission)
                    .setMessage(R.string.faile_to_get_camera_permission)
                    .setNegativeButton(R.string.cancel, (dialogInterface, i) -> NavHostFragment.findNavController(this).navigateUp());
            ActivityResultLauncher<String> requestPermissionLauncher =
                    registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
                        if (isGranted) {
                            startCamera();
                        } else {
                            permissionDeniedDialog.show();
                        }
                    });
            permissionDeniedDialog.setPositiveButton(R.string.retry, (dialogInterface, i) -> requestPermissionLauncher.launch(android.Manifest.permission.CAMERA));

            requestPermissionLauncher.launch(android.Manifest.permission.CAMERA);
        } else {
            startCamera();
        }
    }

    private void startCamera() {
        ListenableFuture<ProcessCameraProvider> cameraProviderFuture = ProcessCameraProvider.getInstance(getContext());
        cameraProviderFuture.addListener(() -> {
            try {
                ProcessCameraProvider cameraProvider = cameraProviderFuture.get();
                bindPreview(cameraProvider);
            } catch (ExecutionException | InterruptedException e) {
                // No errors need to be handled for this Future.
                // This should never be reached.
            }
        }, ContextCompat.getMainExecutor(getContext()));
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
            viewModel.getIsFlashOn().observe(getViewLifecycleOwner(), isFlashOn -> camera.getCameraControl().enableTorch(isFlashOn));
        }

        CameraControl cameraControl = camera.getCameraControl();
        //  pinch-to-zoom
        ScaleGestureDetector scaleGestureDetector = new ScaleGestureDetector(getContext(), new ScaleGestureDetector.SimpleOnScaleGestureListener() {
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
