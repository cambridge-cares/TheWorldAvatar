package uk.ac.cam.cares.jps.user;

import android.content.Context;
import android.content.SharedPreferences;
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

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.databinding.FragmentTimelineSettingBinding;

@AndroidEntryPoint
public class TimelineSettingFragment extends Fragment {

    private FragmentTimelineSettingBinding binding;
    private SharedPreferences preferences;
    private static final String PREF_NAME = "TimelinePrefs";
    private static final String KEY_DURATION = "recording_duration";
    private static final String KEY_AUTOSTART = "autostart_enabled";

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

        preferences = requireContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);

        setupSpinner();
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

        boolean isAutostartEnabled = preferences.getBoolean(KEY_AUTOSTART, false);
        binding.switchAutostart.setChecked(isAutostartEnabled);
        updateAutostartLabel(isAutostartEnabled);

        binding.switchAutostart.setOnCheckedChangeListener((btn, isChecked) -> {
            preferences.edit().putBoolean(KEY_AUTOSTART, isChecked).apply();
            updateAutostartLabel(isChecked);
            Toast.makeText(requireContext(),
                    "Auto-start " + (isChecked ? "enabled" : "disabled"),
                    Toast.LENGTH_SHORT).show();
        });

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

    private void updateAutostartLabel(boolean enabled) {
        binding.labelAutostart.setText("Auto-Start Recording (" + (enabled ? "on" : "off") + ")");
    }


    //just placeholder durations, not implemented yet
    private void setupSpinner() {
        ArrayAdapter<String> adapter = new ArrayAdapter<>(
                requireContext(),
                android.R.layout.simple_spinner_item,
                durations
        );
        adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        binding.spinnerDuration.setAdapter(adapter);

        String saved = preferences.getString(KEY_DURATION, "15 minutes");
        int savedIndex = java.util.Arrays.asList(durations).indexOf(saved);
        if (savedIndex >= 0) {
            binding.spinnerDuration.setSelection(savedIndex);
        }

        binding.spinnerDuration.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            boolean firstSelection = true;

            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                if (firstSelection) {
                    firstSelection = false;
                    return;
                }

                String selected = durations[position];
                preferences.edit().putString(KEY_DURATION, selected).apply();
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
