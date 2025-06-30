package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.HelpAboutBinding;

@AndroidEntryPoint
public class HelpAboutFragment extends Fragment {

    private HelpAboutBinding binding;

    public HelpAboutFragment() {
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = HelpAboutBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.section1Title.setOnClickListener(v -> toggleSectionVisibility(binding.section1Subtitle));
        binding.section2Title.setOnClickListener(v -> toggleSectionVisibility(binding.section2Subtitle));
        binding.section3Title.setOnClickListener(v -> toggleSectionVisibility(binding.section3Subtitle));

    }

    public void toggleSectionVisibility(TextView subtitle) {
        if (subtitle.getVisibility() == View.VISIBLE) {
            subtitle.setVisibility(View.GONE);
        } else {
            subtitle.setVisibility(View.VISIBLE);
        }
    }
}
