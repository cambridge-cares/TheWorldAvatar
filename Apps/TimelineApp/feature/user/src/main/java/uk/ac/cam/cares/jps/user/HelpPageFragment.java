package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.navigation.NavDirections;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.FragmentHelpBinding;

@AndroidEntryPoint
public class HelpPageFragment extends Fragment {

    private FragmentHelpBinding binding;

    public HelpPageFragment() {
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
        binding.helpMapInteractionRow.helpTopicTitle.setText(getString(R.string.help_topic_map));
        binding.helpSensorDocumentationRow.helpTopicTitle.setText(getString(R.string.help_topic_sensor_documentation));
        binding.helpFeaturesRow.helpTopicTitle.setText(getString(R.string.help_topic_helpful_features));
        binding.helpRecordingRow.helpTopicTitle.setText(getString(R.string.help_topic_recording));
        binding.helpLocationHistoryRow.helpTopicTitle.setText(getString(R.string.help_topic_location_history));
        binding.helpFaqRow.helpTopicTitle.setText(getString(R.string.help_topic_faq));

        binding.helpFeaturesRow.getRoot().setOnClickListener(v -> {
            NavDirections action = HelpPageFragmentDirections.actionHelpPageFragmentToHelpFeaturesFragment();
            NavHostFragment.findNavController(this).navigate(action);
        });

        binding.helpLocationHistoryRow.getRoot().setOnClickListener(v -> {
            NavDirections action = HelpPageFragmentDirections.actionHelpPageFragmentToHelpLocationFragment();
            NavHostFragment.findNavController(this).navigate(action);
        });

        binding.helpMapInteractionRow.getRoot().setOnClickListener(v -> {
            NavDirections action = HelpPageFragmentDirections.actionHelpPageFragmentToHelpMapFragment();
            NavHostFragment.findNavController(this).navigate(action);
        });

        binding.helpAboutRow.getRoot().setOnClickListener(v -> {
            NavDirections action = HelpPageFragmentDirections.actionHelpPageFragmentToHelpAboutFragment();
            NavHostFragment.findNavController(this).navigate(action);
        });

        binding.helpRecordingRow.getRoot().setOnClickListener(v -> {
            NavDirections action = HelpPageFragmentDirections.actionHelpPageFragmentToHelpRecordingFragment();
            NavHostFragment.findNavController(this).navigate(action);
        });

        binding.helpFaqRow.getRoot().setOnClickListener(v -> {
            NavDirections action = HelpPageFragmentDirections.actionHelpPageFragmentToHelpFaqFragment();
            NavHostFragment.findNavController(this).navigate(action);
        });

        binding.helpSensorDocumentationRow.getRoot().setOnClickListener(v -> {
            NavDirections action = HelpPageFragmentDirections.actionHelpPageFragmentToHelpSensorFragment();
            NavHostFragment.findNavController(this).navigate(action);
        });

        binding.helpGettingStartedRow.getRoot().setOnClickListener(v -> {
            NavDirections action = HelpPageFragmentDirections.actionHelpPageFragmentToHelpGettingStartedFragment();
            NavHostFragment.findNavController(this).navigate(action);
        });

        return binding.getRoot();
    }
}