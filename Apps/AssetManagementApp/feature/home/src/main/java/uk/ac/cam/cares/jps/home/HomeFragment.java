package uk.ac.cam.cares.jps.home;

import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentTransaction;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.home.databinding.FragmentHomeBinding;
import uk.ac.cam.cares.jps.ui.UiUtils;

public class HomeFragment extends Fragment {
    private FragmentHomeBinding binding;
    private final Logger LOGGER = Logger.getLogger(HomeFragment.class);
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        BasicConfigurator.configure();
        binding = FragmentHomeBinding.inflate(inflater);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), new OnBackPressedCallback(true) {
            private boolean doubleBackToExitPressedOnce;

            @Override
            public void handleOnBackPressed() {
                if (doubleBackToExitPressedOnce) {
                    requireActivity().finishAffinity();
                    return;
                }

                this.doubleBackToExitPressedOnce = true;
                Toast.makeText(requireContext(), "Please click BACK again to exit", Toast.LENGTH_SHORT).show();

                new Handler(Looper.getMainLooper()).postDelayed(() -> doubleBackToExitPressedOnce=false, 2000);
            }
        });

        binding.settingBt.setOnClickListener(v -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/setting_page"))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
        });

        binding.mailBt.setOnClickListener(v -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/mail_box"))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
        });

        binding.scanCard.setOnClickListener(v -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/scan_page"))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
        });

        binding.addAssetCard.setOnClickListener(v -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/add_asset"))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
        });

        binding.sweepCard.setOnClickListener(v -> {
            UiUtils.showNotImplementedDialog(requireContext());
        });

        binding.scheduleCard.setOnClickListener(v -> {
            UiUtils.showNotImplementedDialog(requireContext());
        });

        binding.printCard.setOnClickListener(v -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/qr_printing"))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
        });

    }
}
