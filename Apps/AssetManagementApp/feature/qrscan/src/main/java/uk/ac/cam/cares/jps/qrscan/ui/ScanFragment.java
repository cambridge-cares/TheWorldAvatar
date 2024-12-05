package uk.ac.cam.cares.jps.qrscan.ui;

import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.camera.core.ImageAnalysis;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.camera.CameraFragment;
import uk.ac.cam.cares.jps.qrscan.R;
import uk.ac.cam.cares.jps.qrscan.analyzer.QRCodeAnalyzer;
import uk.ac.cam.cares.jps.qrscan.databinding.FragmentScanBinding;

public class ScanFragment extends CameraFragment {
    private final Logger LOGGER = Logger.getLogger(ScanFragment.class);
    private ScanViewModel viewModel;
    private FragmentScanBinding binding;
    private BoxOverlayView boxOverlayView;
    private ImageAnalysis qrAnalysis;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentScanBinding.inflate(inflater);
        viewModel = new ViewModelProvider(this).get(ScanViewModel.class);
        super.viewModel = viewModel;

        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        // Use cases and added to the camera in the super.onViewCreated
        super.onViewCreated(view.findViewById(R.id.camera), savedInstanceState);

        LOGGER.info("on view created");
        boxOverlayView = binding.boxOverlayView;

        // clear state
        viewModel.resetUrlState();
        boxOverlayView.setRect(null);

        viewModel.getBBox().observe(getViewLifecycleOwner(), bBox -> {
            boxOverlayView.setImageInfo(viewModel.imageHeight, viewModel.imageWidth);
            boxOverlayView.setRect(bBox);
        });
        viewModel.getConfirmedUrl().observe(getViewLifecycleOwner(), url -> {
            LOGGER.info("url changed observed");
            if (url == null || url.isEmpty()) {
                return;
            }

            LOGGER.info("url not empty: " + url);
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/info_page?uri=" + url))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
        });
    }

    @Override
    public void initUseCases() {
        super.initUseCases();

        qrAnalysis = new ImageAnalysis.Builder()
                .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
                .build();
        qrAnalysis.setAnalyzer(cameraExecutor, new QRCodeAnalyzer(viewModel));
        useCaseList.add(qrAnalysis);
    }

}
