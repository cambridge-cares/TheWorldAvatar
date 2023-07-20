package uk.ac.cam.cares.jps.home;

import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.home.databinding.FragmentHomeBinding;

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

        binding.settingBt.setOnClickListener(v -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/setting_page"))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);
//            Intent intent = new Intent(getBaseContext(), SettingActivity.class);
//            startActivity(intent);
        });

        binding.scanCard.setOnClickListener(v -> {
            NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                    .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/scan_page"))
                    .build();
            NavHostFragment.findNavController(this).navigate(request);

//            Intent intent = new Intent(getBaseContext(), ScanActivity.class);
//            startActivity(intent);
        });
    }
}
