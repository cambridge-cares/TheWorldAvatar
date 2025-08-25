package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.text.Html;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.HelpRecordingBinding;

@AndroidEntryPoint
public class HelpRecordingFragment extends Fragment {

    private HelpRecordingBinding binding;

    public HelpRecordingFragment() {
        // Required empty public constructor
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater,
                             @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = HelpRecordingBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view,
                              @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.topAppbar.setNavigationOnClickListener(v -> NavHostFragment.findNavController(this).navigateUp());

        // Optional: Expand/collapse feature for future card toggles (if sections are made expandable)
        // binding.sectionRecordingQuickstart.setOnClickListener(v -> toggleSectionVisibility(binding.recordingQuickstartDetails));
        // binding.sectionRecordingSensor.setOnClickListener(v -> toggleSectionVisibility(binding.recordingSensorDetails));

        binding.quickStartText.setText(Html.fromHtml(getString(R.string.help_quick_start_html), Html.FROM_HTML_MODE_LEGACY));
        binding.sensorSettingsText.setText(Html.fromHtml(getString(R.string.help_sensor_settings_html), Html.FROM_HTML_MODE_LEGACY));
    }

    public void toggleSectionVisibility(View view) {
        if (view.getVisibility() == View.VISIBLE) {
            view.setVisibility(View.GONE);
        } else {
            view.setVisibility(View.VISIBLE);
        }
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        binding = null;
    }
}
