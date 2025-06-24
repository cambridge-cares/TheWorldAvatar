package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.FragmentHelpBinding;

@AndroidEntryPoint
public class HelpPageFragment extends Fragment {

    private FragmentHelpBinding binding;

    public HelpPageFragment() {
        // Required empty constructor
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = FragmentHelpBinding.inflate(inflater, container, false);

        binding.topAppbar.setNavigationOnClickListener(v ->
                NavHostFragment.findNavController(this).navigateUp());

        binding.helpAboutRow.helpTopicTitle.setText(getString(R.string.help_topic_about));
        binding.helpGettingStartedRow.helpTopicTitle.setText(getString(R.string.help_topic_getting_started));
        binding.helpTroubleshootingRow.helpTopicTitle.setText(getString(R.string.help_topic_map));
        binding.helpSensorDocumentationRow.helpTopicTitle.setText(getString(R.string.help_topic_sensor_documentation));
        binding.helpFeaturesRow.helpTopicTitle.setText(getString(R.string.help_topic_helpful_features));
        binding.helpLocationHistoryRow.helpTopicTitle.setText(getString(R.string.help_topic_location_history));
        binding.helpRecordingRow.helpTopicTitle.setText(getString(R.string.help_topic_recording));
        binding.helpLocationHistoryRow2.helpTopicTitle.setText(getString(R.string.help_topic_location_history));
        binding.helpFaqRow.helpTopicTitle.setText(getString(R.string.help_topic_faq));

        return binding.getRoot();
    }
}
