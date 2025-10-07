package uk.ac.cam.cares.jps.user.setting;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.FragmentTimelineSettingBinding;
import uk.ac.cam.cares.jps.ui.viewmodel.AppPreferenceViewModel;

@AndroidEntryPoint
public class TimelineSettingFragment extends Fragment {

    private FragmentTimelineSettingBinding binding;
    private AppPreferenceViewModel appPreferenceViewModel;

    private final String[] durations = {
            "15 minutes", "30 minutes", "1 hour", "2 hours", "1 day", "No limit"
    };


    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater,
                             @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {

        binding = FragmentTimelineSettingBinding.inflate(inflater, container, false);
        binding.setLifecycleOwner(getViewLifecycleOwner());

        appPreferenceViewModel = new ViewModelProvider(requireActivity()).get(AppPreferenceViewModel.class);

        setupSpinner();
        initListeners();

        return binding.getRoot();
    }

    private void initListeners() {
        appPreferenceViewModel.autoStart.observe(getViewLifecycleOwner(), isAutostartEnabled -> {
            if (isAutostartEnabled == null) {
                return;
            }

            binding.switchAutostart.setChecked(isAutostartEnabled);
            binding.labelAutostart.setText("Auto-Start Recording (" + (isAutostartEnabled ? "on" : "off") + ")");
        });

        binding.switchAutostart.setOnCheckedChangeListener((btn, isChecked) -> {
            appPreferenceViewModel.setAutoStart(isChecked);
            Toast.makeText(requireContext(),
                    "Auto-start " + (isChecked ? "enabled" : "disabled"),
                    Toast.LENGTH_SHORT).show();
        });

        binding.btnAutoDelete.setOnClickListener(v ->
                Toast.makeText(requireContext(),
                        "Auto-delete options coming soon",
                        Toast.LENGTH_SHORT).show());

        binding.btnDeleteAll.setOnClickListener(v ->
                Toast.makeText(requireContext(),
                        "Delete-all confirmation coming soon",
                        Toast.LENGTH_SHORT).show());

        binding.timelineTopAppbar.setNavigationOnClickListener(view ->
                requireActivity().getOnBackPressedDispatcher().onBackPressed());
    }


    // TODO: just placeholder durations, not implemented yet
    private void setupSpinner() {
        ArrayAdapter<String> adapter = new ArrayAdapter<>(
                requireContext(),
                android.R.layout.simple_spinner_item,
                durations
        );
        adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        binding.spinnerDuration.setAdapter(adapter);

        appPreferenceViewModel.uploadDuration.observe(getViewLifecycleOwner(), duration -> {
            int savedIndex = java.util.Arrays.asList(durations).indexOf(duration);
            if (savedIndex >= 0) {
                binding.spinnerDuration.setSelection(savedIndex);
            }
        });

        binding.spinnerDuration.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            boolean firstSelection = true;

            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                if (firstSelection) {
                    firstSelection = false;
                    return;
                }

                String selected = durations[position];
                appPreferenceViewModel.setUploadDuration(selected);
                Toast.makeText(requireContext(),
                        "Duration set to " + selected,
                        Toast.LENGTH_SHORT).show();
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {
               //not implemented yet
            }
        });
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        binding = null;
    }
}
