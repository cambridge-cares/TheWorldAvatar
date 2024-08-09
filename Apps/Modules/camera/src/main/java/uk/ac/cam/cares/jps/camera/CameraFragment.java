package uk.ac.cam.cares.jps.camera;

import android.annotation.SuppressLint;
import android.content.pm.PackageManager;
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
import androidx.camera.core.MeteringPoint;
import androidx.camera.core.MeteringPointFactory;
import androidx.camera.core.Preview;
import androidx.camera.core.UseCase;
import androidx.camera.lifecycle.ProcessCameraProvider;
import androidx.camera.view.PreviewView;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.common.util.concurrent.ListenableFuture;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;

import uk.ac.cam.cares.jps.camera.databinding.FragmentCameraBinding;

public abstract class CameraFragment extends Fragment {
    private final Logger LOGGER = Logger.getLogger(CameraFragment.class);
    public FragmentCameraBinding binding;
    public Executor cameraExecutor;
    public CameraViewModel viewModel;

    public List<UseCase> useCaseList = new ArrayList<>();

    private PreviewView previewView;
    private FocusCircleView focusCircleView;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentCameraBinding.inflate(inflater);
        viewModel = new ViewModelProvider(this).get(CameraViewModel.class);

        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        view.findViewById(R.id.topActionBar).findViewById(R.id.close_button).setOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());
        view.findViewById(R.id.topActionBar).findViewById(R.id.flash_button).setOnClickListener(v -> viewModel.toggleFlashState());
        previewView = view.findViewById(R.id.previewView);
        focusCircleView = view.findViewById(R.id.focusCircleView);

        cameraExecutor = ContextCompat.getMainExecutor(requireContext());

        initUseCases();
        checkCameraPermissionAndStartCamera();
    }

    @SuppressLint("ClickableViewAccessibility")
    private void setupCameraControl(Camera camera) {
        // flash light control
        if (camera.getCameraInfo().hasFlashUnit()) {
            viewModel.getIsFlashOn().observe(getViewLifecycleOwner(), isFlashOn -> camera.getCameraControl().enableTorch(isFlashOn));
        }

        CameraControl cameraControl = camera.getCameraControl();
        //  pinch-to-zoom
        ScaleGestureDetector scaleGestureDetector = new ScaleGestureDetector(requireContext(), new ScaleGestureDetector.SimpleOnScaleGestureListener() {
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
        previewView.setOnTouchListener((view, motionEvent) ->  {
            if (motionEvent.getAction() == MotionEvent.ACTION_UP) {
                MeteringPointFactory factory = previewView.getMeteringPointFactory();
                MeteringPoint point = factory.createPoint(motionEvent.getX(), motionEvent.getY());
                FocusMeteringAction action = new FocusMeteringAction.Builder(point).build();
                cameraControl.startFocusAndMetering(action);

                focusCircleView.showCircle(motionEvent.getX(), motionEvent.getY());
            }
            scaleGestureDetector.onTouchEvent(motionEvent);

            return true;
        });
    }

    private void checkCameraPermissionAndStartCamera() {
        if (ContextCompat.checkSelfPermission(requireContext(), android.Manifest.permission.CAMERA) != PackageManager.PERMISSION_GRANTED) {
            MaterialAlertDialogBuilder permissionDeniedDialog = new MaterialAlertDialogBuilder(requireContext())
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
        ListenableFuture<ProcessCameraProvider> cameraProviderFuture = ProcessCameraProvider.getInstance(requireContext());
        cameraProviderFuture.addListener(() -> {
            try {
                ProcessCameraProvider cameraProvider = cameraProviderFuture.get();
                cameraProvider.unbindAll();

                CameraSelector cameraSelector = new CameraSelector.Builder()
                        .requireLensFacing(CameraSelector.LENS_FACING_BACK)
                        .build();
                Camera camera = cameraProvider.bindToLifecycle(this, cameraSelector, useCaseList.toArray(new UseCase[0]));
                setupCameraControl(camera);
            } catch (ExecutionException | InterruptedException e) {
                // No errors need to be handled for this Future.
                // This should never be reached.
            }
        }, ContextCompat.getMainExecutor(requireContext()));
    }

    public void initUseCases() {
        Preview preview = new Preview.Builder()
                .build();
        preview.setSurfaceProvider(this.previewView.getSurfaceProvider());
        useCaseList.add(preview);
    }

    @Override
    public void onStop() {
        super.onStop();
        useCaseList.clear();
    }
}
