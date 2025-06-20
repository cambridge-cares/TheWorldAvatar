package uk.ac.cam.cares.jps.user;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.FragmentTimelineSettingBinding;

@AndroidEntryPoint
public class TimelineSettingFragment extends Fragment {

    private FragmentTimelineSettingBinding binding;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater,
                             @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {

        binding = FragmentTimelineSettingBinding.inflate(inflater, container, false);
        binding.setLifecycleOwner(getViewLifecycleOwner());
        initListeners();
        return binding.getRoot();
    }

    private void initListeners() {
        binding.btnToggleTimeline.setOnClickListener(v -> {
            boolean currentlyOn = binding.btnToggleTimeline.getText().toString().equals("Turn Off");
            binding.btnToggleTimeline.setText(currentlyOn ? "Turn On" : "Turn Off");
            Toast.makeText(requireContext(),
                    "Recording " + (currentlyOn ? "stopped" : "started"),
                    Toast.LENGTH_SHORT).show();
        });

        binding.switchAutostart.setOnCheckedChangeListener(
                (btn, isChecked) -> Toast.makeText(requireContext(),
                        "Auto-start " + (isChecked ? "enabled" : "disabled"),
                        Toast.LENGTH_SHORT).show());

        binding.btnAutoDelete.setOnClickListener(v ->
                Toast.makeText(requireContext(),
                        "Auto-delete options coming soon",
                        Toast.LENGTH_SHORT).show());

        binding.btnViewHistory.setOnClickListener(v ->
                Toast.makeText(requireContext(),
                        "History viewer coming soon",
                        Toast.LENGTH_SHORT).show());

        binding.btnDeleteAll.setOnClickListener(v ->
                Toast.makeText(requireContext(),
                        "Delete-all confirmation coming soon",
                        Toast.LENGTH_SHORT).show());

        binding.timelineTopAppbar.setNavigationOnClickListener(view ->
                requireActivity().getOnBackPressedDispatcher().onBackPressed());

    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        binding = null;
    }
}
