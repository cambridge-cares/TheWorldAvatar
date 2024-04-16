package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.FragmentSensorSettingBinding;
import uk.ac.cam.cares.jps.user.viewmodel.SensorViewModel;

@AndroidEntryPoint
public class SensorSettingFragment extends Fragment {

    private FragmentSensorSettingBinding binding;
    private SensorViewModel sensorViewModel;


    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentSensorSettingBinding.inflate(inflater);
        sensorViewModel = new ViewModelProvider(requireActivity()).get(SensorViewModel.class);

        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.topAppbar.setNavigationOnClickListener(view1 -> NavHostFragment.findNavController(this).navigateUp());

        binding.editProfile.setOnClickListener(this::onRecordButtonClicked);
        if (sensorViewModel.getIsRecording().getValue()) {
            binding.editProfile.setText(R.string.stop_recording);
        } else {
            binding.editProfile.setText(R.string.start_recording);
        }
    }

    private void onRecordButtonClicked(View view) {
        TextView textView = (TextView) view;
        if (sensorViewModel.getIsRecording().getValue()) {
            textView.setText(R.string.start_recording);
            sensorViewModel.stopRecording();
            sensorViewModel.setIsRecording(false);
        } else {
            textView.setText(R.string.stop_recording);
            sensorViewModel.startRecording();
            sensorViewModel.setIsRecording(true);
        }
    }
}
